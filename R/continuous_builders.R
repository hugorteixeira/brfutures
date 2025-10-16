.brf_select_continuous_builder <- function(ticker_root) {
  candidate <- if (is.null(ticker_root)) "" else ticker_root
  if (length(candidate) > 1L) {
    candidate <- candidate[1L]
  }
  if (is.na(candidate) || !nzchar(candidate)) {
    candidate <- ""
  }
  root <- toupper(candidate)
  if (startsWith(root, "BGI")) {
    return(.brf_build_continuous_bgi)
  }
  if (startsWith(root, "CCM")) {
    return(.brf_build_continuous_ccm)
  }
  if (startsWith(root, "DI")) {
    return(.brf_build_continuous_di)
  }
  if (startsWith(root, "WDO")) {
    return(.brf_build_continuous_wdo)
  }
  if (startsWith(root, "WIN")) {
    return(.brf_build_continuous_win)
  }
  .brf_build_continuous_generic
}

.brf_build_continuous_generic <- function(ctx) {
  roll_spec <- ctx$roll_spec
  ticker_root_norm <- ctx$ticker_root
  roll_type_internal <- roll_spec$roll_type
  if (is.character(roll_type_internal)) {
    roll_type_internal <- match.arg(roll_type_internal, c("days_before_roll", "windsor_log_spread", "regression"))
  } else if (!is.function(roll_type_internal)) {
    stop("roll_type must be a recognised identifier or a custom function.", call. = FALSE)
  }
  roll_days_before_expiry <- roll_spec$roll_days_before_expiry
  clamp_ratio <- roll_spec$clamp_ratio
  roll_control <- roll_spec$roll_control
  roll_stagger <- roll_spec$roll_stagger
  contract_ranks <- .brf_normalize_contract_ranks(roll_spec$contract_ranks)
  include_roll_details <- roll_spec$include_roll_details
  aggregate <- .brf_prepare_continuous_aggregate(ctx$aggregate_raw, ticker_root_norm)
  if (isTRUE(ctx$include_older)) {
    aggregate <- .brf_normalize_older_contracts(aggregate)
    if (!inherits(aggregate$estimated_maturity, "Date")) {
      aggregate$estimated_maturity <- as.Date(aggregate$estimated_maturity)
    }
    sym_for_maturity <- if ("ticker" %in% names(aggregate) &&
      any(!is.na(aggregate$ticker) & nzchar(aggregate$ticker))) {
      as.character(aggregate$ticker)
    } else {
      as.character(aggregate$symbol)
    }
    valid_sym <- !is.na(sym_for_maturity) & nzchar(sym_for_maturity)
    missing_idx <- which(is.na(aggregate$estimated_maturity) & valid_sym)
    if (length(missing_idx)) {
      calendar_name <- .brf_get_calendar()
      unique_syms <- unique(sym_for_maturity[missing_idx])
      unique_syms <- unique_syms[!is.na(unique_syms) & nzchar(unique_syms)]
      if (length(unique_syms)) {
        est_map <- stats::setNames(
          vapply(
            unique_syms,
            function(sym) .brf_estimate_maturity(sym, calendar_name = calendar_name),
            FUN.VALUE = as.Date(NA)
          ),
          unique_syms
        )
        aggregate$estimated_maturity[missing_idx] <- est_map[sym_for_maturity[missing_idx]]
      }
    }
  }
  aggregate <- .brf_filter_by_maturity(aggregate, ctx$maturities)
  if (!nrow(aggregate)) {
    stop("No data available after applying the requested maturities.", call. = FALSE)
  }
  chain <- .brf_build_daily_front_chain(
    aggregate,
    roll_days_before_expiry = roll_days_before_expiry,
    clamp_ratio = clamp_ratio,
    roll_type = roll_type_internal,
    adjust_type = ctx$build_type,
    roll_control = roll_control,
    include_roll_details = include_roll_details,
    contract_ranks = contract_ranks,
    roll_stagger = roll_stagger,
    use_notional = ctx$use_notional
  )
  no_adj <- chain$no_adj
  adj <- chain$adj
  daily <- chain$daily_table
  no_adj <- .brf_xts_force_tz(no_adj)
  adj <- .brf_xts_force_tz(adj)
  if (!is.null(daily) && NROW(daily)) {
    daily$refdate <- .brf_to_posix_br_tz(daily$refdate)
    daily <- as.data.frame(daily)
  } else {
    daily <- data.frame()
  }
  aggregate$refdate <- .brf_to_posix_br_tz(aggregate$refdate)
  meta <- .brf_continuous_meta(ticker_root_norm)
  attr(no_adj, "fut_tick_size") <- meta$tick_size
  attr(no_adj, "fut_multiplier") <- meta$fut_multiplier
  attr(adj, "fut_tick_size") <- meta$tick_size
  attr(adj, "fut_multiplier") <- meta$fut_multiplier
  no_adj <- .brf_append_return_columns(no_adj)
  adj <- .brf_append_return_columns(adj)
  if (isTRUE(ctx$debug)) {
    adj_disc <- as.numeric(adj$Discrete)
    adj_log <- as.numeric(adj$Log)
    no_adj_disc <- as.numeric(no_adj$Discrete)
    no_adj_log <- as.numeric(no_adj$Log)
    adj_summary <- sprintf(
      "Adjusted cumulative: %.2f%% | annualised: %.2f%%",
      .brf_cumulative_return(adj_disc) * 100,
      .brf_annualized_return(adj_disc) * 100
    )
    adj_summary_log <- sprintf(
      "Adjusted log cumulative: %.2f%% | annualised: %.2f%%",
      .brf_cumulative_return(adj_log) * 100,
      .brf_annualized_return(adj_log) * 100
    )
    no_adj_summary <- sprintf(
      "Unadjusted cumulative: %.2f%% | annualised: %.2f%%",
      .brf_cumulative_return(no_adj_disc) * 100,
      .brf_annualized_return(no_adj_disc) * 100
    )
    no_adj_summary_log <- sprintf(
      "Unadjusted log cumulative: %.2f%% | annualised: %.2f%%",
      .brf_cumulative_return(no_adj_log) * 100,
      .brf_annualized_return(no_adj_log) * 100
    )
    message(
      paste(
        "Continuous series summary for", ticker_root_norm, "with",
        roll_days_before_expiry, "roll-day window:",
        adj_summary,
        adj_summary_log,
        no_adj_summary,
        no_adj_summary_log,
        sep = "\n  "
      )
    )
  }
  if (isTRUE(ctx$everything)) {
    result <- list(
      aggregate = aggregate,
      no_adj = no_adj,
      adj = adj,
      daily = daily
    )
    return(invisible(result))
  }
  if (!isTRUE(ctx$single_xts)) {
    if (isTRUE(ctx$adjusted)) {
      name_adj <- paste0(ticker_root_norm, "FUT_adj_roll", roll_days_before_expiry)
      result_adj <- stats::setNames(list(adj), name_adj)
      return(invisible(result_adj))
    }
    name_noadj <- paste0(ticker_root_norm, "FUT_noadj_roll", roll_days_before_expiry)
    result_noadj <- stats::setNames(list(no_adj), name_noadj)
    return(invisible(result_noadj))
  }
  if (isTRUE(ctx$adjusted)) {
    return(invisible(adj))
  }
  invisible(no_adj)
}

# Dedicated family builders default to the generic implementation so product-
# specific tweaks remain encapsulated in one place.
.brf_build_continuous_bgi <- function(ctx) {
  .brf_build_continuous_generic(ctx)
}

.brf_build_continuous_ccm <- function(ctx) {
  .brf_build_continuous_generic(ctx)
}

.brf_build_continuous_di <- function(ctx) {
  if (!isTRUE(ctx$use_notional)) {
    return(.brf_build_continuous_generic(ctx))
  }
  original_everything <- isTRUE(ctx$everything)
  original_single_xts <- isTRUE(ctx$single_xts)
  original_adjusted <- isTRUE(ctx$adjusted)
  roll_spec <- ctx$roll_spec
  roll_days_before_expiry <- roll_spec$roll_days_before_expiry
  ctx_internal <- ctx
  ctx_internal$everything <- TRUE
  ctx_internal$single_xts <- TRUE
  ctx_internal$use_notional <- TRUE
  full_result <- .brf_build_continuous_generic(ctx_internal)
  if (!is.list(full_result) ||
    !all(c("aggregate", "no_adj", "adj", "daily") %in% names(full_result))) {
    stop(
      "DI builder expected the generic builder to return aggregate/no_adj/adj/daily components.",
      call. = FALSE
    )
  }
  daily_tbl <- full_result$daily
  tenor_lookup <- NULL
  if (is.data.frame(daily_tbl) &&
    nrow(daily_tbl) &&
    "refdate" %in% names(daily_tbl) &&
      "sel_days_to_maturity" %in% names(daily_tbl)) {
    df_lookup <- daily_tbl[, c("refdate", "sel_days_to_maturity")]
    df_lookup$refdate <- as.Date(df_lookup$refdate)
    df_lookup$sel_days_to_maturity <- suppressWarnings(as.numeric(df_lookup$sel_days_to_maturity))
    df_lookup <- df_lookup[is.finite(df_lookup$sel_days_to_maturity) & !is.na(df_lookup$refdate), , drop = FALSE]
    if (nrow(df_lookup)) {
      df_lookup <- df_lookup[order(df_lookup$refdate), , drop = FALSE]
      df_lookup <- df_lookup[!duplicated(df_lookup$refdate, fromLast = TRUE), , drop = FALSE]
      tenor_lookup <- stats::setNames(
        df_lookup$sel_days_to_maturity,
        as.character(df_lookup$refdate)
      )
    }
  }
  fallback_days <- NA_real_
  roll_control <- roll_spec$roll_control
  if (is.list(roll_control) &&
    length(roll_control) &&
    is.numeric(roll_control$target_days_to_maturity) &&
      is.finite(roll_control$target_days_to_maturity) &&
      roll_control$target_days_to_maturity > 0) {
    fallback_days <- as.numeric(roll_control$target_days_to_maturity)
  }
  transform_chain <- function(x_xts) {
    if (!xts::is.xts(x_xts) || !NROW(x_xts)) {
      return(x_xts)
    }
    idx_dates <- as.Date(zoo::index(x_xts))
    n <- length(idx_dates)
    attr_keep <- intersect(
      c("fut_tick_size", "fut_multiplier", "active_symbol"),
      names(attributes(x_xts))
    )
    saved_attrs <- lapply(attr_keep, function(nm) attr(x_xts, nm, exact = TRUE))
    names(saved_attrs) <- attr_keep
    col_has <- function(nm) nm %in% colnames(x_xts)
    pull_numeric <- function(nm) {
      if (!col_has(nm)) {
        return(rep(NA_real_, n))
      }
      as.numeric(x_xts[, nm])
    }
    pu_open <- if (col_has("PU_o")) pull_numeric("PU_o") else if (col_has("Open")) pull_numeric("Open") else rep(NA_real_, n)
    pu_high <- if (col_has("PU_h")) pull_numeric("PU_h") else if (col_has("High")) pull_numeric("High") else rep(NA_real_, n)
    pu_low <- if (col_has("PU_l")) pull_numeric("PU_l") else if (col_has("Low")) pull_numeric("Low") else rep(NA_real_, n)
    pu_close <- if (col_has("PU_c")) pull_numeric("PU_c") else if (col_has("Close")) pull_numeric("Close") else rep(NA_real_, n)
    valid_days <- if (col_has("DaysToMaturity")) pull_numeric("DaysToMaturity") else rep(NA_real_, n)
    if (!is.null(tenor_lookup)) {
      matched <- tenor_lookup[as.character(idx_dates)]
      matched <- as.numeric(matched)
      fill_idx <- is.na(valid_days) & !is.na(matched)
      if (any(fill_idx)) {
        valid_days[fill_idx] <- matched[fill_idx]
      }
    }
    if (any(is.na(valid_days))) {
      valid_days <- zoo::na.locf(valid_days, na.rm = FALSE)
      valid_days <- zoo::na.locf(valid_days, na.rm = FALSE, fromLast = TRUE)
    }
    if (is.finite(fallback_days) && fallback_days > 0) {
      invalid <- !is.finite(valid_days) | valid_days <= 0
      if (any(invalid)) {
        valid_days[invalid] <- fallback_days
      }
    }
    valid_days[!is.finite(valid_days) | valid_days <= 0] <- NA_real_
    calc_rate <- function(pu_vec) {
      out <- rep(NA_real_, n)
      ok <- is.finite(pu_vec) & pu_vec > 0 & is.finite(valid_days) & valid_days > 0
      if (any(ok)) {
        out[ok] <- .di_rate_from_pu(pu_vec[ok], valid_days[ok])
      }
      out
    }
    rate_open <- calc_rate(pu_open)
    rate_high <- calc_rate(pu_high)
    rate_low <- calc_rate(pu_low)
    rate_close <- calc_rate(pu_close)
    if (col_has("Open")) {
      x_xts$Open <- rate_open
    }
    if (col_has("High")) {
      x_xts$High <- rate_high
    }
    if (col_has("Low")) {
      x_xts$Low <- rate_low
    }
    if (col_has("Close")) {
      x_xts$Close <- rate_close
    }
    if (col_has("CM_o")) {
      x_xts$CM_o <- rate_open
    }
    if (col_has("CM_h")) {
      x_xts$CM_h <- rate_high
    }
    if (col_has("CM_l")) {
      x_xts$CM_l <- rate_low
    }
    if (col_has("CM_c")) {
      x_xts$CM_c <- rate_close
    }
    if (col_has("DaysToMaturity")) {
      x_xts$DaysToMaturity <- valid_days
    } else if (any(is.finite(valid_days))) {
      x_xts$DaysToMaturity <- valid_days
    }
    if (!col_has("PU_o")) {
      x_xts$PU_o <- pu_open
    }
    if (!col_has("PU_h")) {
      x_xts$PU_h <- pu_high
    }
    if (!col_has("PU_l")) {
      x_xts$PU_l <- pu_low
    }
    if (!col_has("PU_c")) {
      x_xts$PU_c <- pu_close
    }
    pu_close_xts <- xts::xts(pu_close, order.by = zoo::index(x_xts))
    prev <- xts::lag.xts(pu_close_xts, k = 1)
    ratio <- pu_close_xts / prev
    ratio_vals <- as.numeric(ratio)
    ratio_vals[!is.finite(ratio_vals) | ratio_vals <= 0] <- NA_real_
    discrete <- ratio_vals - 1
    log_ret <- log(ratio_vals)
    discrete[is.na(discrete)] <- 0
    log_ret[is.na(log_ret)] <- 0
    if (length(discrete)) {
      discrete[1] <- 0
    }
    if (length(log_ret)) {
      log_ret[1] <- 0
    }
    x_xts$Discrete <- discrete
    x_xts$Log <- log_ret
    preferred <- c(
      "Open", "High", "Low", "Close", "Volume",
      "PU_o", "PU_h", "PU_l", "PU_c",
      "CM_o", "CM_h", "CM_l", "CM_c",
      "DaysToMaturity", "Discrete", "Log"
    )
    present <- intersect(preferred, colnames(x_xts))
    remainder <- setdiff(colnames(x_xts), present)
    ordered_xts <- x_xts[, c(present, remainder), drop = FALSE]
    for (nm in attr_keep) {
      attr(ordered_xts, nm) <- saved_attrs[[nm]]
    }
    ordered_xts
  }
  full_result$no_adj <- transform_chain(full_result$no_adj)
  full_result$adj <- transform_chain(full_result$adj)
  if (original_everything) {
    return(invisible(full_result))
  }
  if (!isTRUE(original_single_xts)) {
    if (isTRUE(original_adjusted)) {
      name_adj <- paste0(ctx$ticker_root, "FUT_adj_roll", roll_days_before_expiry)
      result_adj <- stats::setNames(list(full_result$adj), name_adj)
      return(invisible(result_adj))
    }
    name_noadj <- paste0(ctx$ticker_root, "FUT_noadj_roll", roll_days_before_expiry)
    result_noadj <- stats::setNames(list(full_result$no_adj), name_noadj)
    return(invisible(result_noadj))
  }
  if (isTRUE(original_adjusted)) {
    return(invisible(full_result$adj))
  }
  invisible(full_result$no_adj)
}

.brf_build_continuous_wdo <- function(ctx) {
  .brf_build_continuous_generic(ctx)
}

.brf_build_continuous_win <- function(ctx) {
  .brf_build_continuous_generic(ctx)
}
