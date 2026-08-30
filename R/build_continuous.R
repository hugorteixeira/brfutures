#' Build backward-adjusted continuous futures series
#'
#' Constructs a continuous futures curve for a given root using the standard
#' ratio back-adjustment (Panama) methodology. The resulting series preserves
#' the latest contract levels and scales prior history whenever a roll occurs.
#'
#' @param data Data frame with daily contract quotes as returned by
#'   `get_brfut_agg()` (must contain `date`, `root`, `ticker`, `maturity`,
#'   and OHLC columns).
#' @param root Character scalar with the primary contract root (e.g. "WIN").
#' @param days_before_roll Non-negative integer indicating how many calendar
#'   days before maturity the function should roll into the next contract.
#' @param roll_date_col Column used as the roll anchor. Defaults to
#'   `"last_trade_date"`. Uses the column if present, otherwise computed from
#'   the last available `date` per ticker.
#' @param lock_roll When `TRUE`, once the series rolls into a newer contract it
#'   never rolls back to an older one. Days without eligible contracts are
#'   skipped.
#' @param maturities Either "all" (default) to allow every listed maturity or
#'   a character vector with month codes (e.g. `c("F", "G", "H")`).
#' @param add_root Optional character vector with additional roots to treat as
#'   extensions of `root` (useful for historical naming changes).
#' @param add_attrs When `TRUE` (default), enrich the series with futures
#'   metadata via `.brf_add_futures_attrs()`. Set to `FALSE` to skip.
#' @param add_globalenv When `TRUE` (default), assigns the resulting series into
#'   the global environment. Set to `FALSE` to skip.
#' @return An OHLCV `xts` object with attributes describing the roll schedule
#'   and active contracts.
#' @export
build_backward_adjusted <- function(data,
                                    root,
                                    days_before_roll = 5,
                                    roll_date_col = "last_trade_date",
                                    lock_roll = TRUE,
                                    maturities = "all",
                                    add_root = NULL,
                                    add_attrs = TRUE,
                                    add_globalenv = TRUE) {
  .brf_build_continuous(
    data = data,
    root = root,
    days_before_roll = days_before_roll,
    roll_date_col = roll_date_col,
    lock_roll = lock_roll,
    maturities = maturities,
    add_root = add_root,
    add_attrs = add_attrs,
    add_globalenv = add_globalenv,
    mode = "backward"
  )
}

#' Build forward-adjusted continuous futures series
#'
#' Generates a continuous futures curve that preserves the early history of the
#' front contract and scales forward contracts to avoid price gaps after each
#' roll.
#'
#' @inheritParams build_backward_adjusted
#' @return An OHLCV `xts` object with attributes describing the roll schedule
#'   and active contracts.
#' @export
build_forward_adjusted <- function(data,
                                   root,
                                   days_before_roll = 5,
                                   roll_date_col = "last_trade_date",
                                   lock_roll = TRUE,
                                   maturities = "all",
                                   add_root = NULL,
                                   add_attrs = TRUE,
                                   add_globalenv = TRUE) {
  .brf_build_continuous(
    data = data,
    root = root,
    days_before_roll = days_before_roll,
    roll_date_col = roll_date_col,
    lock_roll = lock_roll,
    maturities = maturities,
    add_root = add_root,
    add_attrs = add_attrs,
    add_globalenv = add_globalenv,
    mode = "forward"
  )
}

.brf_build_continuous <- function(data,
                                  root,
                                  days_before_roll,
                                  roll_date_col,
                                  lock_roll,
                                  maturities,
                                  add_root,
                                  add_attrs,
                                  add_globalenv,
                                  mode) {
  mode <- match.arg(mode, c("backward", "forward"))
  df <- .brf_validate_contract_data(data)
  days_before_roll <- .brf_normalize_days_before_roll(days_before_roll)

  main_root <- .brf_normalize_root(root)
  extra_roots <- unique(.brf_normalize_root_vector(add_root))
  roots <- unique(c(main_root, extra_roots))

  df <- df[df$root %in% roots, , drop = FALSE]
  if (!nrow(df)) {
    stop("No data available for the requested root(s).", call. = FALSE)
  }

  df$date <- as.Date(df$date)
  df$maturity <- as.Date(df$maturity)
  df <- df[!is.na(df$date) & !is.na(df$maturity), , drop = FALSE]
  if (!nrow(df)) {
    stop("No valid rows after filtering out missing dates/maturities.", call. = FALSE)
  }

  df$ticker <- toupper(trimws(as.character(df$ticker)))
  df$root <- toupper(trimws(as.character(df$root)))

  roll_date_col <- if (is.null(roll_date_col) || length(roll_date_col) != 1L || is.na(roll_date_col)) {
    "maturity"
  } else {
    trimws(as.character(roll_date_col))
  }
  if (!nzchar(roll_date_col)) {
    roll_date_col <- "maturity"
  }
  roll_key <- tolower(roll_date_col)
  if (roll_key == "last_trade_date") {
    if ("last_trade_date" %in% names(df)) {
      df$roll_date <- as.Date(df$last_trade_date)
    } else {
      last_by_ticker <- tapply(df$date, df$ticker, max, na.rm = TRUE)
      df$roll_date <- as.Date(last_by_ticker[df$ticker])
    }
  } else {
    if (!roll_date_col %in% names(df)) {
      stop("Roll date column '", roll_date_col, "' not found in data.", call. = FALSE)
    }
    df$roll_date <- as.Date(df[[roll_date_col]])
  }
  df <- df[!is.na(df$roll_date), , drop = FALSE]
  if (!nrow(df)) {
    stop("No valid rows after filtering out missing roll dates.", call. = FALSE)
  }

  numeric_cols <- intersect(c("open", "high", "low", "close", "volume", "volume_qty"), names(df))
  if (length(numeric_cols)) {
    df[numeric_cols] <- lapply(df[numeric_cols], function(col) {
      if (is.numeric(col)) {
        col
      } else {
        suppressWarnings(as.numeric(col))
      }
    })
  }

  df$month_code <- .brf_extract_month_code(df$ticker, roots)

  if (!(is.character(maturities) && length(maturities) == 1L && toupper(maturities) == "ALL")) {
    allowed_mats <- toupper(trimws(as.character(maturities)))
    allowed_mats <- allowed_mats[nzchar(allowed_mats)]
    allowed_mats <- unique(allowed_mats)
    if (!length(allowed_mats)) {
      stop("Supply at least one maturity month code (e.g. 'F', 'G').", call. = FALSE)
    }
    df <- df[df$month_code %in% allowed_mats, , drop = FALSE]
    if (!nrow(df)) {
      stop("No rows left after filtering by the requested maturities.", call. = FALSE)
    }
  } else {
    allowed_mats <- "ALL"
  }

  df$days_to_maturity <- as.integer(df$maturity - df$date)
  df$days_to_roll <- as.integer(df$roll_date - df$date)
  df <- df[order(df$date, df$maturity, df$ticker), , drop = FALSE]

  by_date <- split(df, df$date)
  lock_roll <- isTRUE(lock_roll)
  roll_info <- stats::aggregate(roll_date ~ ticker, df, max)
  maturity_info <- stats::aggregate(maturity ~ ticker, df, max)
  roll_info <- merge(roll_info, maturity_info, by = "ticker", all.x = TRUE)
  roll_info <- roll_info[order(roll_info$roll_date, roll_info$maturity, roll_info$ticker), , drop = FALSE]
  ticker_rank <- stats::setNames(seq_len(nrow(roll_info)), roll_info$ticker)
  trading_days <- sort(unique(df$date))
  selected_list <- vector("list", length(trading_days))
  selected_count <- 0L
  min_rank <- NA_integer_
  for (i in seq_along(trading_days)) {
    day <- trading_days[i]
    candidates <- by_date[[as.character(day)]]
    if (lock_roll && !is.na(min_rank)) {
      ranks <- ticker_rank[candidates$ticker]
      keep_idx <- !is.na(ranks) & ranks >= min_rank
      candidates <- candidates[keep_idx, , drop = FALSE]
    }
    pick <- .brf_pick_front_contract(candidates, days_before_roll)
    if (!nrow(pick)) {
      next
    }
    selected_count <- selected_count + 1L
    selected_list[[selected_count]] <- pick
    if (lock_roll) {
      pick_rank <- ticker_rank[pick$ticker]
      if (!is.na(pick_rank)) {
        min_rank <- pick_rank
      }
    }
  }
  if (!selected_count) {
    stop("Unable to determine an active contract for the selected period.", call. = FALSE)
  }
  selected <- do.call(rbind, selected_list[seq_len(selected_count)])
  rownames(selected) <- NULL

  per_ticker <- split(df, df$ticker)

  col_map <- c(
    Open = "open",
    High = "high",
    Low = "low",
    Close = "close",
    Volume = "volume",
    Volume_Qty = "volume_qty"
  )
  xts_cols <- lapply(col_map, function(src) {
    if (src %in% names(selected)) {
      values <- selected[[src]]
      if (!is.numeric(values)) {
        values <- suppressWarnings(as.numeric(values))
      }
      values
    } else {
      NULL
    }
  })
  keep_cols <- lengths(xts_cols) > 0
  if (!any(keep_cols)) {
    stop("Selected data does not contain any OHLCV columns to build the series.", call. = FALSE)
  }

  xts_matrix <- do.call(cbind, xts_cols[keep_cols])
  colnames(xts_matrix) <- names(col_map)[keep_cols]
  xts_matrix <- as.matrix(xts_matrix)

  idx <- selected$date
  order_idx <- order(idx)
  xts_matrix <- xts_matrix[order_idx, , drop = FALSE]
  selected <- selected[order_idx, , drop = FALSE]
  idx <- idx[order_idx]

  price_cols <- intersect(c("Open", "High", "Low", "Close"), colnames(xts_matrix))
  adjustment_factors <- rep(1, nrow(selected))
  roll_schedule <- .brf_compute_roll_schedule(selected, per_ticker)

  if (nrow(roll_schedule)) {
    for (j in seq_len(nrow(roll_schedule))) {
      switch_pos <- roll_schedule$switch_position[j]
      backward_factor <- roll_schedule$backward_factor[j]
      forward_factor <- roll_schedule$forward_factor[j]
      if (mode == "backward" && switch_pos > 1L) {
        adjustment_factors[seq_len(switch_pos - 1L)] <- adjustment_factors[seq_len(switch_pos - 1L)] * backward_factor
      }
      if (mode == "forward" && switch_pos <= length(adjustment_factors)) {
        adjustment_factors[switch_pos:length(adjustment_factors)] <- adjustment_factors[switch_pos:length(adjustment_factors)] * forward_factor
      }
    }
  }

  if (length(price_cols)) {
    for (col in price_cols) {
      xts_matrix[, col] <- xts_matrix[, col] * adjustment_factors
    }
  }

  series <- xts::xts(xts_matrix, order.by = idx)

  roll_export <- roll_schedule
  if (nrow(roll_export)) {
    roll_export$switch_position <- NULL
  }

  attr(series, "continuous_spec") <- list(
    method = mode,
    root = main_root,
    extra_roots = extra_roots,
    all_roots = roots,
    roll_date_col = roll_date_col,
    lock_roll = lock_roll,
    days_before_roll = days_before_roll,
    maturities = allowed_mats
  )
  active_contracts <- data.frame(
    date = idx,
    root = selected$root,
    ticker = selected$ticker,
    maturity = selected$maturity,
    days_to_maturity = selected$days_to_maturity,
    stringsAsFactors = FALSE
  )
  if ("roll_date" %in% names(selected)) {
    active_contracts$roll_date <- selected$roll_date
  }
  if ("days_to_roll" %in% names(selected)) {
    active_contracts$days_to_roll <- selected$days_to_roll
  }
  attr(series, "active_contracts") <- active_contracts
  attr(series, "roll_schedule") <- roll_export
  root <- paste0(root, "FUT_1D_", days_before_roll, "DBR_", toupper(substr(mode, 1, 1)))
  if (isTRUE(add_attrs)) {
    series <- .brf_add_futures_attrs(series, root)
  }
  if (isTRUE(add_globalenv)) {
    assign(root, series, envir = .GlobalEnv)
  }
  return(series)
}

.brf_validate_contract_data <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) {
    stop("`data` must be a non-empty data frame with futures quotes.", call. = FALSE)
  }
  required_cols <- c("date", "root", "ticker", "maturity", "close")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  data
}

.brf_normalize_days_before_roll <- function(value) {
  if (length(value) != 1L || is.na(value)) {
    stop("`days_before_roll` must be a single non-negative integer.", call. = FALSE)
  }
  if (!is.numeric(value)) {
    stop("`days_before_roll` must be numeric.", call. = FALSE)
  }
  normalized <- as.integer(value)
  if (normalized < 0) {
    stop("`days_before_roll` cannot be negative.", call. = FALSE)
  }
  normalized
}

.brf_extract_month_code <- function(ticker, roots) {
  if (!length(ticker)) {
    return(character())
  }
  roots <- unique(toupper(roots))
  roots <- roots[order(nchar(roots), decreasing = TRUE)]
  vapply(ticker, function(sym) {
    sym <- toupper(sym)
    matched <- roots[startsWith(sym, roots)]
    base_root <- if (length(matched)) matched[which.max(nchar(matched))] else ""
    remainder <- substring(sym, nchar(base_root) + 1L)
    if (!nzchar(remainder)) {
      return(NA_character_)
    }
    substr(remainder, 1L, 1L)
  }, character(1L))
}

.brf_pick_front_contract <- function(rows, days_before_roll) {
  if (!nrow(rows)) {
    return(rows)
  }
  days_col <- if ("days_to_roll" %in% names(rows)) "days_to_roll" else "days_to_maturity"
  date_col <- if ("roll_date" %in% names(rows)) "roll_date" else "maturity"
  days_vals <- rows[[days_col]]
  rows <- rows[order(days_vals, rows[[date_col]], rows$ticker), , drop = FALSE]
  days_vals <- rows[[days_col]]
  future_mask <- !is.na(days_vals) & days_vals >= 0L
  eligible <- future_mask & days_vals > days_before_roll
  if (any(eligible)) {
    return(rows[which(eligible)[1L], , drop = FALSE])
  }
  if (any(future_mask)) {
    return(rows[which(future_mask)[1L], , drop = FALSE])
  }
  rows[which.max(days_vals), , drop = FALSE]
}

.brf_compute_roll_schedule <- function(selected, per_ticker) {
  n <- nrow(selected)
  if (n <= 1L) {
    return(data.frame(
      from_ticker = character(),
      to_ticker = character(),
      switch_date = as.Date(character()),
      bridge_date = as.Date(character()),
      from_close = numeric(),
      to_close = numeric(),
      backward_factor = numeric(),
      forward_factor = numeric(),
      switch_position = integer(),
      stringsAsFactors = FALSE
    ))
  }

  tickers <- selected$ticker
  changes <- which(tickers[-1L] != utils::head(tickers, -1L)) + 1L
  if (!length(changes)) {
    return(data.frame(
      from_ticker = character(),
      to_ticker = character(),
      switch_date = as.Date(character()),
      bridge_date = as.Date(character()),
      from_close = numeric(),
      to_close = numeric(),
      backward_factor = numeric(),
      forward_factor = numeric(),
      switch_position = integer(),
      stringsAsFactors = FALSE
    ))
  }

  schedule <- vector("list", length(changes))
  for (i in seq_along(changes)) {
    pos <- changes[i]
    from_ticker <- tickers[pos - 1L]
    to_ticker <- tickers[pos]
    switch_date <- selected$date[pos]
    bridge <- .brf_find_bridge(per_ticker[[from_ticker]], per_ticker[[to_ticker]], switch_date)

    from_close <- bridge$from_close
    to_close <- bridge$to_close

    ratio_new_old <- if (!is.na(from_close) && !is.na(to_close) && from_close != 0) to_close / from_close else NA_real_
    ratio_old_new <- if (!is.na(from_close) && !is.na(to_close) && to_close != 0) from_close / to_close else NA_real_

    backward_factor <- if (!is.na(ratio_new_old) && is.finite(ratio_new_old) && ratio_new_old > 0) ratio_new_old else 1
    forward_factor <- if (!is.na(ratio_old_new) && is.finite(ratio_old_new) && ratio_old_new > 0) ratio_old_new else 1

    schedule[[i]] <- data.frame(
      from_ticker = from_ticker,
      to_ticker = to_ticker,
      switch_date = switch_date,
      bridge_date = bridge$bridge_date,
      from_close = from_close,
      to_close = to_close,
      backward_factor = backward_factor,
      forward_factor = forward_factor,
      switch_position = pos,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, schedule)
}

.brf_find_bridge <- function(from_df, to_df, switch_date) {
  if (is.null(from_df) || is.null(to_df) || !nrow(from_df) || !nrow(to_df)) {
    return(list(
      bridge_date = as.Date(NA),
      from_close = NA_real_,
      to_close = NA_real_
    ))
  }
  from_dates <- as.Date(from_df$date)
  to_dates <- as.Date(to_df$date)
  common_dates <- intersect(from_dates, to_dates)
  common_dates <- common_dates[common_dates <= switch_date]
  if (!length(common_dates)) {
    return(list(
      bridge_date = as.Date(NA),
      from_close = NA_real_,
      to_close = NA_real_
    ))
  }
  bridge_date <- max(common_dates)
  list(
    bridge_date = bridge_date,
    from_close = .brf_pick_bridge_price(from_df, bridge_date),
    to_close = .brf_pick_bridge_price(to_df, bridge_date)
  )
}

.brf_pick_bridge_price <- function(df, bridge_date) {
  rows <- df[as.Date(df$date) == bridge_date, , drop = FALSE]
  if (!nrow(rows)) {
    return(NA_real_)
  }
  price_cols <- c("close", "settlement_price", "average_price")
  for (col in price_cols) {
    if (col %in% names(rows)) {
      values <- rows[[col]]
      if (!is.numeric(values)) {
        values <- suppressWarnings(as.numeric(values))
      }
      candidate <- values[!is.na(values)][1L]
      if (length(candidate) && !is.na(candidate)) {
        return(candidate)
      }
    }
  }
  NA_real_
}
