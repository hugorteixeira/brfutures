# Continuous series helpers ---------------------------------------------------

.brf_sanitize_ohlcv_xts <- function(x_xts) {
  if (is.null(x_xts) || NROW(x_xts) == 0) {
    return(xts::xts())
  }
  std_name <- function(nm) {
    ln <- tolower(nm)
    if (grepl("^o(pen)?$", ln) || grepl("abert", ln)) return("Open")
    if (grepl("^h(igh)?$", ln) || grepl("max", ln))   return("High")
    if (grepl("^l(ow)?$", ln)  || grepl("min", ln))   return("Low")
    if (grepl("^c(lose)?$", ln) || grepl("fech|sett(le)?|px_last|last", ln)) return("Close")
    if (grepl("^v(ol|olume)?$", ln) || grepl("q(t)?y|contracts|neg|trades", ln)) return("Volume")
    nm
  }
  cn <- colnames(x_xts)
  if (is.null(cn) || !length(cn)) {
    cn <- "Close"
  }
  cn_std <- vapply(cn, std_name, character(1), USE.NAMES = FALSE)
  colnames(x_xts) <- cn_std
  if (NCOL(x_xts) == 1L && !identical(colnames(x_xts)[1], "Close")) {
    colnames(x_xts) <- "Close"
  }
  keep <- intersect(c("Open", "High", "Low", "Close", "Volume"), colnames(x_xts))
  if (!length(keep)) {
    keep <- colnames(x_xts)[1L]
    colnames(x_xts)[1L] <- "Close"
    keep <- "Close"
  }
  x_xts <- x_xts[, keep, drop = FALSE]
  d <- as.Date(zoo::index(x_xts))
  y <- xts::xts(zoo::coredata(x_xts), order.by = d)
  if ("Open" %in% colnames(y)) {
    y$Open[!is.finite(y$Open) | y$Open <= 0] <- NA_real_
  }
  if ("High" %in% colnames(y)) {
    y$High[!is.finite(y$High) | y$High <= 0] <- NA_real_
  }
  if ("Low" %in% colnames(y)) {
    y$Low[!is.finite(y$Low) | y$Low <= 0] <- NA_real_
  }
  if ("Close" %in% colnames(y)) {
    y$Close[!is.finite(y$Close) | y$Close <= 0] <- NA_real_
  }
  if ("Volume" %in% colnames(y)) {
    y$Volume[!is.finite(y$Volume) | y$Volume < 0] <- NA_real_
  }
  if ("Close" %in% colnames(y)) {
    y <- y[is.finite(y$Close)]
  }
  y
}

.brf_xts_deduplicate_daily_last <- function(x_xts) {
  if (is.null(x_xts) || NROW(x_xts) == 0) {
    return(x_xts)
  }
  d <- as.Date(zoo::index(x_xts))
  keep <- !duplicated(d, fromLast = TRUE)
  x_xts[keep]
}

.brf_compute_roll_date_from_maturity <- function(days_vec,
                                                 estimated_maturity,
                                                 roll_days) {
  if (length(days_vec) == 0) {
    return(as.Date(NA))
  }
  days_vec <- sort(unique(as.Date(days_vec)))
  cand <- days_vec[days_vec <= estimated_maturity]
  last_tradable <- if (length(cand)) max(cand) else max(days_vec)
  if (!is.finite(last_tradable)) {
    return(as.Date(NA))
  }
  idx <- which(days_vec == last_tradable)
  if (!length(idx)) {
    idx <- length(days_vec)
  }
  roll_idx <- max(1, idx - roll_days)
  days_vec[roll_idx]
}

.brf_price_last_on_or_before <- function(x_xts, d, col = "Close") {
  if (NROW(x_xts) == 0) {
    return(NA_real_)
  }
  col <- if (col %in% colnames(x_xts)) col else colnames(x_xts)[1]
  ix <- which(as.Date(zoo::index(x_xts)) <= as.Date(d))
  if (!length(ix)) {
    return(NA_real_)
  }
  as.numeric(x_xts[ix[length(ix)], col])
}

.brf_price_first_on_or_after <- function(x_xts, d, col = "Close") {
  if (NROW(x_xts) == 0) {
    return(NA_real_)
  }
  col <- if (col %in% colnames(x_xts)) col else colnames(x_xts)[1]
  ix <- which(as.Date(zoo::index(x_xts)) >= as.Date(d))
  if (!length(ix)) {
    return(NA_real_)
  }
  as.numeric(x_xts[ix[1], col])
}

.brf_default_roll_control <- function() {
  list(
    window_pre = 20L,
    window_post = 5L,
    min_overlap = 3L,
    winsor = c(0.05, 0.95),
    price_column = "Close"
  )
}

.brf_normalize_roll_control <- function(ctrl) {
  defaults <- .brf_default_roll_control()
  if (is.null(ctrl)) {
    return(defaults)
  }
  if (!is.list(ctrl)) {
    stop("roll_control must be NULL or a list.", call. = FALSE)
  }
  merged <- utils::modifyList(defaults, ctrl, keep.null = TRUE)
  merged$window_pre <- as.integer(merged$window_pre)
  if (is.na(merged$window_pre) || merged$window_pre < 0) {
    merged$window_pre <- defaults$window_pre
  }
  merged$window_post <- as.integer(merged$window_post)
  if (is.na(merged$window_post) || merged$window_post < 0) {
    merged$window_post <- defaults$window_post
  }
  merged$min_overlap <- as.integer(merged$min_overlap)
  if (is.na(merged$min_overlap) || merged$min_overlap < 1) {
    merged$min_overlap <- defaults$min_overlap
  }
  if (!is.numeric(merged$winsor) || length(merged$winsor) != 2L) {
    merged$winsor <- defaults$winsor
  }
  merged$winsor <- sort(pmax(pmin(merged$winsor, 1), 0))
  if (!is.character(merged$price_column) || !length(merged$price_column)) {
    merged$price_column <- defaults$price_column
  }
  merged$price_column <- merged$price_column[1L]
  merged
}

.brf_roll_price_columns <- function(daily, price_column = "Close") {
  suffix <- tolower(price_column)
  front_col <- paste0("f_", suffix)
  secondary_col <- paste0("n_", suffix)
  if (!front_col %in% names(daily) || !secondary_col %in% names(daily)) {
    front_col <- "f_close"
    secondary_col <- "n_close"
  }
  list(front = front_col, secondary = secondary_col)
}

.brf_enrich_roll_details <- function(daily,
                                     seam_idx,
                                     ratios,
                                     roll_control) {
  if (!NROW(daily)) {
    return(daily)
  }
  ramp_mask <- if ("secondary_weight" %in% names(daily)) {
    daily$secondary_weight > 0 & daily$secondary_weight < 1
  } else {
    rep(FALSE, NROW(daily))
  }
  daily$roll_event <- ramp_mask
  daily$roll_seam <- rep(FALSE, NROW(daily))
  daily$roll_ratio <- NA_real_
  daily$roll_front_price <- NA_real_
  daily$roll_secondary_price <- NA_real_
  daily$roll_spread <- NA_real_
  daily$roll_spread_pct <- NA_real_
  daily$roll_basis_bp <- NA_real_
  if (!length(seam_idx)) {
    return(daily)
  }
  cols <- .brf_roll_price_columns(daily, roll_control$price_column)
  front_px <- daily[[cols$front]]
  secondary_px <- daily[[cols$secondary]]
  seam_idx <- seam_idx[seam_idx <= NROW(daily)]
  seam_ratios <- ratios
  if (length(seam_ratios) < length(seam_idx)) {
    seam_ratios <- c(seam_ratios, rep(NA_real_, length(seam_idx) - length(seam_ratios)))
  }
  seam_ratios <- seam_ratios[seq_len(length(seam_idx))]
  daily$roll_event[seam_idx] <- TRUE
  daily$roll_seam[seam_idx] <- TRUE
  daily$roll_ratio[seam_idx] <- seam_ratios
  daily$roll_front_price[seam_idx] <- front_px[seam_idx]
  daily$roll_secondary_price[seam_idx] <- secondary_px[seam_idx]
  spread <- secondary_px - front_px
  with_valid <- seam_idx[is.finite(front_px[seam_idx]) & is.finite(secondary_px[seam_idx])]
  if (length(with_valid)) {
    daily$roll_spread[with_valid] <- spread[with_valid]
    pct <- (secondary_px[with_valid] / front_px[with_valid]) - 1
    daily$roll_spread_pct[with_valid] <- pct
    daily$roll_basis_bp[with_valid] <- pct * 10000
  }
  daily
}

.brf_normalize_contract_ranks <- function(contract_ranks) {
  default <- list(primary = 1L, secondary = 2L)
  if (is.null(contract_ranks)) {
    return(default)
  }
  if (is.list(contract_ranks)) {
    vals <- unlist(contract_ranks, use.names = FALSE)
  } else {
    vals <- contract_ranks
  }
  vals <- as.integer(vals)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) {
    return(default)
  }
  primary <- vals[1L]
  if (is.na(primary) || primary < 1L) {
    primary <- default$primary
  }
  secondary <- if (length(vals) >= 2L) vals[2L] else (primary + 1L)
  if (is.na(secondary) || secondary <= primary) {
    secondary <- primary + 1L
  }
  list(primary = primary, secondary = secondary)
}

.brf_roll_spec <- function(method, ...) {
  structure(
    list(
      method = method,
      params = list(...)
    ),
    class = "brf_roll_spec"
  )
}

.brf_is_roll_spec <- function(x) inherits(x, "brf_roll_spec")

.brf_resolve_roll_spec <- function(roll) {
  if (missing(roll) || is.null(roll)) {
    roll <- days_before_roll()
  }
  defaults <- list(
    roll_type = "days_before_roll",
    roll_days_before_expiry = 20L,
    clamp_ratio = c(0.6, 1.6),
    roll_control = NULL,
    roll_stagger = 1L,
    contract_ranks = c(1L, 2L),
    include_roll_details = FALSE
  )
  if (.brf_is_roll_spec(roll)) {
    params <- roll$params
    aliases <- list(
      days_before_expiry = "roll_days_before_expiry",
      clamp = "clamp_ratio",
      stagger = "roll_stagger",
      include_details = "include_roll_details",
      include_stats = "include_roll_details"
    )
    for (alias in names(aliases)) {
      if (alias %in% names(params)) {
        params[[aliases[[alias]]]] <- params[[alias]]
      }
    }
    spec <- utils::modifyList(defaults, params, keep.null = TRUE)
    spec$roll_type <- roll$method
  } else if (is.character(roll)) {
    spec <- defaults
    spec$roll_type <- match.arg(roll, c("days_before_roll", "windsor_log_spread", "regression"))
  } else if (is.function(roll)) {
    spec <- defaults
    spec$roll_type <- roll
  } else {
    stop("roll_type must be a roll specification, character identifier, or function.", call. = FALSE)
  }
  spec$roll_days_before_expiry <- as.integer(spec$roll_days_before_expiry)
  if (is.na(spec$roll_days_before_expiry) || spec$roll_days_before_expiry < 0L) {
    spec$roll_days_before_expiry <- 20L
  }
  clamp <- as.numeric(spec$clamp_ratio)
  if (!length(clamp)) {
    clamp <- c(0.6, 1.6)
  } else if (length(clamp) == 1L) {
    clamp <- rep(clamp, 2L)
  } else {
    clamp <- clamp[seq_len(2L)]
  }
  clamp[!is.finite(clamp)] <- NA
  if (any(is.na(clamp))) {
    clamp <- c(0.6, 1.6)
  }
  spec$clamp_ratio <- clamp
  spec$roll_stagger <- as.integer(spec$roll_stagger)
  if (is.na(spec$roll_stagger) || spec$roll_stagger < 1L) {
    spec$roll_stagger <- 1L
  }
  ranks <- as.integer(spec$contract_ranks)
  ranks <- ranks[is.finite(ranks)]
  if (!length(ranks)) {
    ranks <- c(1L, 2L)
  }
  spec$contract_ranks <- ranks
  spec$include_roll_details <- isTRUE(spec$include_roll_details)
  spec$roll_control <- .brf_normalize_roll_control(spec$roll_control)
  spec
}

#' Roll specification: days-before-expiry switching
#'
#' Create a roll specification that transitions exposure from the primary to the
#' secondary contract a fixed number of business days before expiry. The
#' returned object is intended to be passed to the `roll_type` argument of
#' [brf_build_continuous_series()].
#'
#' @param days_before_expiry Business days prior to expiry to begin rolling.
#' @param clamp_ratio Lower/upper bounds applied to seam ratios.
#' @param roll_control Optional list merged into the default roll control
#'   settings (see [brf_build_continuous_series()]).
#' @param roll_stagger Number of business days used to stagger the roll.
#' @param contract_ranks Which contract ranks to stitch (defaults to front/next).
#' @param include_roll_details When `TRUE`, enrich the daily output with roll
#'   diagnostics.
#'
#' @return A roll specification consumable by [brf_build_continuous_series()].
#' @export
days_before_roll <- function(days_before_expiry = 20,
                             clamp_ratio = c(0.6, 1.6),
                             roll_control = NULL,
                             roll_stagger = 1L,
                             contract_ranks = c(1L, 2L),
                             include_roll_details = FALSE) {
  .brf_roll_spec(
    method = "days_before_roll",
    roll_days_before_expiry = days_before_expiry,
    clamp_ratio = clamp_ratio,
    roll_control = roll_control,
    roll_stagger = roll_stagger,
    contract_ranks = contract_ranks,
    include_roll_details = include_roll_details
  )
}

#' Roll specification: winsorised log-spread seam ratio
#'
#' @inheritParams days_before_roll
#' @param winsor Probability cut-offs for the winsorisation step applied to log
#'   spreads around the seam.
#'
#' @export
windsor_log_spread_roll <- function(days_before_expiry = 20,
                                    clamp_ratio = c(0.6, 1.6),
                                    winsor = c(0.05, 0.95),
                                    roll_control = NULL,
                                    roll_stagger = 1L,
                                    contract_ranks = c(1L, 2L),
                                    include_roll_details = FALSE) {
  if (is.null(roll_control)) {
    roll_control <- list()
  }
  roll_control$winsor <- winsor
  .brf_roll_spec(
    method = "windsor_log_spread",
    roll_days_before_expiry = days_before_expiry,
    clamp_ratio = clamp_ratio,
    roll_control = roll_control,
    roll_stagger = roll_stagger,
    contract_ranks = contract_ranks,
    include_roll_details = include_roll_details
  )
}

#' Roll specification: regression-based seam ratio
#'
#' @inheritParams days_before_roll
#'
#' @export
regression_roll <- function(days_before_expiry = 20,
                            clamp_ratio = c(0.6, 1.6),
                            roll_control = NULL,
                            roll_stagger = 1L,
                            contract_ranks = c(1L, 2L),
                            include_roll_details = FALSE) {
  .brf_roll_spec(
    method = "regression",
    roll_days_before_expiry = days_before_expiry,
    clamp_ratio = clamp_ratio,
    roll_control = roll_control,
    roll_stagger = roll_stagger,
    contract_ranks = contract_ranks,
    include_roll_details = include_roll_details
  )
}

#' Roll specification: custom seam function
#'
#' @inheritParams days_before_roll
#' @param fun Custom function used to estimate seam ratios. It must accept the
#'   same arguments as the `roll_type` callback documented in
#'   [brf_build_continuous_series()].
#'
#' @export
custom_roll <- function(fun,
                        days_before_expiry = 20,
                        clamp_ratio = c(0.6, 1.6),
                        roll_control = NULL,
                        roll_stagger = 1L,
                        contract_ranks = c(1L, 2L),
                        include_roll_details = FALSE) {
  if (!is.function(fun)) {
    stop("fun must be a function.", call. = FALSE)
  }
  .brf_roll_spec(
    method = fun,
    roll_days_before_expiry = days_before_expiry,
    clamp_ratio = clamp_ratio,
    roll_control = roll_control,
    roll_stagger = roll_stagger,
    contract_ranks = contract_ranks,
    include_roll_details = include_roll_details
  )
}

.brf_compute_seam_ratio <- function(prev_raw,
                                    curr_raw,
                                    seam_date,
                                    window_pre = 20,
                                    window_post = 0,
                                    min_overlap = 3,
                                    clamp = c(0.6, 1.6),
                                    price_column = "Close") {
  if (NROW(prev_raw) == 0 || NROW(curr_raw) == 0) {
    return(NA_real_)
  }
  extract_price <- function(x) {
    if (NROW(x) == 0) {
      return(xts::xts())
    }
    column <- price_column
    if (!column %in% colnames(x)) {
      column <- if ("Close" %in% colnames(x)) {
        "Close"
      } else {
        colnames(x)[1L]
      }
    }
    x[, column, drop = FALSE]
  }
  rng <- paste0(as.Date(seam_date) - window_pre, "/",
                as.Date(seam_date) + window_post)
  a <- extract_price(prev_raw[rng])
  b <- extract_price(curr_raw[rng])
  if (NROW(a) > 0 && NROW(b) > 0) {
    merged <- merge(a, b, join = "inner")
    if (NROW(merged) > 0) {
      v1 <- as.numeric(merged[, 1])
      v2 <- as.numeric(merged[, 2])
      ok <- is.finite(v1) & is.finite(v2) & v1 > 0 & v2 > 0
      if (sum(ok) >= min_overlap) {
        lr <- log(v1[ok]) - log(v2[ok])
        r <- exp(stats::median(lr, na.rm = TRUE))
        if (is.finite(r) && r > 0) {
          return(max(min(r, clamp[2]), clamp[1]))
        }
      }
    }
  }
  p_prev <- .brf_price_last_on_or_before(prev_raw, seam_date, col = price_column)
  p_curr <- .brf_price_first_on_or_after(curr_raw, seam_date, col = price_column)
  if (is.finite(p_prev) && is.finite(p_curr) && p_prev > 0 && p_curr > 0) {
    r <- p_prev / p_curr
    return(max(min(r, clamp[2]), clamp[1]))
  }
  NA_real_
}

.brf_prepare_continuous_aggregate <- function(data, ticker_root) {
  if (is.null(data)) {
    stop("Aggregate data is empty for ", ticker_root, ".", call. = FALSE)
  }
  if (xts::is.xts(data)) {
    df <- as.data.frame(zoo::coredata(data), stringsAsFactors = FALSE)
    idx <- zoo::index(data)
    if (!any(grepl("ref|date|data|dt", tolower(names(df))))) {
      df$refdate <- as.Date(idx)
    }
    implicit_cols <- c("symbol", "open", "high", "low", "close", "volume", "estimated_maturity", "contract_code", "ticker")
    missing_cols <- setdiff(implicit_cols, names(df))
    if (length(missing_cols)) {
      df[missing_cols] <- NA
    }
  } else if (is.data.frame(data)) {
    df <- data
  } else if (is.list(data)) {
    df <- tryCatch(
      as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE),
      error = function(...) NULL
    )
    if (is.null(df)) {
      stop("Aggregate data for ", ticker_root, " could not be coerced to a data frame.", call. = FALSE)
    }
  } else {
    stop("Aggregate data for ", ticker_root, " must be an xts object or data frame.", call. = FALSE)
  }
  if (!nrow(df)) {
    stop("Aggregate data is empty for ", ticker_root, ".", call. = FALSE)
  }
  names_lower <- tolower(names(df))
  names_map <- stats::setNames(names(df), names_lower)
  best_name <- function(pattern) {
    ids <- which(grepl(pattern, names_lower, ignore.case = TRUE))
    if (length(ids)) {
      return(unname(names_map[ids[1]]))
    }
    NA_character_
  }
  ref_col <- if ("refdate" %in% names_lower) unname(names_map["refdate"]) else best_name("ref|date|data|dt|index")
  sym_col <- if ("symbol" %in% names_lower) unname(names_map["symbol"]) else best_name("symb|ticker|contr|symbol")
  o_col <- best_name("^open$|abert")
  h_col <- best_name("^high$|max")
  l_col <- best_name("^low$|min")
  c_col <- best_name("^close$|fech|sett(le)?|px_last|last")
  v_col <- if ("volume" %in% names_lower) unname(names_map["volume"]) else best_name("^vol$|volume|q(t)?y|contracts|neg|trades")
  exp_col <- if ("estimated_maturity" %in% names_lower) unname(names_map["estimated_maturity"]) else best_name("matur|expiry|venc")
  contract_col <- if ("contract_code" %in% names_lower) {
    unname(names_map["contract_code"])
  } else if ("maturity_code" %in% names_lower) {
    unname(names_map["maturity_code"])
  } else {
    best_name("contract(_code)?")
  }
  ticker_col <- if ("ticker" %in% names_lower) unname(names_map["ticker"]) else best_name("^ticker$")
  settle_col <- best_name("sett(lement)?|ajuste")
  avg_col <- best_name("average_price|media")
  last_bid_col <- best_name("last_bid|ult\\.?of\\.?comp")
  last_ask_col <- best_name("last_ask|ult\\.?of\\.?vend")
  if (any(is.na(c(ref_col, sym_col, o_col, h_col, l_col, c_col, exp_col)))) {
    stop(
      "Aggregate data for ", ticker_root,
      " is missing required columns (refdate, symbol, open, high, low, close, estimated_maturity).",
      call. = FALSE
    )
  }
  column_map <- c(
    refdate = ref_col,
    symbol = sym_col,
    open = o_col,
    high = h_col,
    low = l_col,
    close = c_col,
    estimated_maturity = exp_col
  )
  if (!is.na(v_col)) {
    column_map["volume"] <- v_col
  }
  if (!is.na(contract_col)) {
    column_map["contract_code"] <- contract_col
  }
  if (!is.na(ticker_col)) {
    column_map["ticker"] <- ticker_col
  }
  if (!is.na(settle_col)) {
    column_map["settlement_price"] <- settle_col
  }
  if (!is.na(avg_col)) {
    column_map["average_price"] <- avg_col
  }
  if (!is.na(last_bid_col)) {
    column_map["last_bid"] <- last_bid_col
  }
  if (!is.na(last_ask_col)) {
    column_map["last_ask"] <- last_ask_col
  }
  column_map <- stats::setNames(unname(column_map), names(column_map))
  out <- df[, column_map, drop = FALSE]
  colnames(out) <- names(column_map)
  out$refdate <- as.Date(out$refdate)
  out$estimated_maturity <- as.Date(out$estimated_maturity)
  out$symbol <- as.character(out$symbol)
  if ("ticker" %in% names(out)) {
    out$ticker <- as.character(out$ticker)
  }
  if ("contract_code" %in% names(out)) {
    out$contract_code <- as.character(out$contract_code)
  }
  if (any(is.na(out$estimated_maturity))) {
    sym_for_maturity <- if ("ticker" %in% names(out) &&
      any(!is.na(out$ticker) & nzchar(out$ticker))) {
      as.character(out$ticker)
    } else {
      as.character(out$symbol)
    }
    valid_idx <- is.na(out$estimated_maturity) &
      !is.na(sym_for_maturity) & nzchar(sym_for_maturity)
    if (any(valid_idx)) {
      calendar_name <- .brf_get_calendar()
      unique_syms <- unique(sym_for_maturity[valid_idx])
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
        out$estimated_maturity[valid_idx] <- est_map[sym_for_maturity[valid_idx]]
      }
    }
  }
  num_cols <- intersect(c("open", "high", "low", "close", "volume", "settlement_price", "average_price", "last_bid", "last_ask"), names(out))
  for (nm in num_cols) {
    out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
  }
  if ("settlement_price" %in% names(out)) {
    needs_close <- !is.finite(out$close) | out$close <= 0
    replaceable <- needs_close & is.finite(out$settlement_price) & out$settlement_price > 0
    out$close[replaceable] <- out$settlement_price[replaceable]
  }
  if ("average_price" %in% names(out)) {
    needs_close <- !is.finite(out$close) | out$close <= 0
    replaceable <- needs_close & is.finite(out$average_price) & out$average_price > 0
    out$close[replaceable] <- out$average_price[replaceable]
  }
  if ("last_bid" %in% names(out)) {
    needs_close <- !is.finite(out$close) | out$close <= 0
    replaceable <- needs_close & is.finite(out$last_bid) & out$last_bid > 0
    out$close[replaceable] <- out$last_bid[replaceable]
  }
  if ("last_ask" %in% names(out)) {
    needs_close <- !is.finite(out$close) | out$close <= 0
    replaceable <- needs_close & is.finite(out$last_ask) & out$last_ask > 0
    out$close[replaceable] <- out$last_ask[replaceable]
  }
  for (nm in c("open", "high", "low")) {
    if (nm %in% names(out)) {
      needs <- !is.finite(out[[nm]]) | out[[nm]] <= 0
      out[[nm]][needs] <- out$close[needs]
    }
  }
  out <- out[!is.na(out$refdate) & !is.na(out$symbol) & !is.na(out$estimated_maturity) &
               is.finite(out$close) & out$close > 0, , drop = FALSE]
  out <- out[order(out$symbol, out$refdate), , drop = FALSE]
  out
}

.brf_split_contracts_xts <- function(agg_df) {
  ordered <- agg_df[order(agg_df$symbol, agg_df$refdate), , drop = FALSE]
  dup_mask <- !duplicated(ordered[, c("symbol", "refdate")], fromLast = TRUE)
  ordered <- ordered[dup_mask, , drop = FALSE]
  splits <- split(ordered, ordered$symbol, drop = TRUE)
  lst <- lapply(splits, function(sub) {
    cols <- c("refdate", "open", "high", "low", "close")
    if ("volume" %in% names(sub)) {
      cols <- c(cols, "volume")
    }
    sub <- sub[, cols, drop = FALSE]
    mat <- as.matrix(sub[, -1, drop = FALSE])
    xt <- xts::xts(mat, order.by = sub$refdate)
    if (NCOL(xt) == 5L) {
      colnames(xt) <- c("Open", "High", "Low", "Close", "Volume")
    } else {
      colnames(xt) <- c("Open", "High", "Low", "Close")
    }
    .brf_sanitize_ohlcv_xts(xt)
  })
  expiry <- vapply(splits, function(sub) {
    vals <- unique(na.omit(sub$estimated_maturity))
    if (!length(vals)) {
      as.Date(NA)
    } else {
      vals[1]
    }
  }, as.Date(NA))
  nz <- vapply(lst, NROW, integer(1)) > 0
  lst <- lst[nz]
  expiry <- expiry[names(lst)]
  list(lst_xts = lst, expiry = expiry)
}

.brf_filter_by_maturity <- function(agg_df, maturities) {
  if (is.null(maturities) || !length(maturities)) {
    return(agg_df)
  }
  mats <- toupper(as.character(maturities))
  if (length(mats) == 1L && mats %in% c("ALL", "")) {
    return(agg_df)
  }
  if (!nrow(agg_df)) {
    return(agg_df)
  }
  if ("contract_code" %in% names(agg_df)) {
    codes <- toupper(substr(agg_df$contract_code, 1, 1))
  } else {
    sym_upper <- toupper(agg_df$symbol)
    match_idx <- regexpr("([FGHJKMNQUVXZ])[0-9]{1,2}$", sym_upper)
    codes <- rep(NA_character_, length(match_idx))
    hits <- match_idx > 0
    if (any(hits)) {
      codes[hits] <- substr(sym_upper[hits], match_idx[hits], match_idx[hits])
    }
  }
  keep <- codes %in% mats
  if (!any(keep, na.rm = TRUE)) {
    warning("Requested maturities not found; returning original data.", call. = FALSE)
    return(agg_df)
  }
  agg_df[keep, , drop = FALSE]
}

.brf_overlap_prices <- function(prev_xts, curr_xts, seam_date,
                                window_pre = 20, window_post = 5,
                                price_column = "Close") {
  if (is.null(prev_xts) || is.null(curr_xts)) {
    return(data.frame(prev = numeric(), curr = numeric()))
  }
  if (!NCOL(prev_xts) || !NCOL(curr_xts)) {
    return(data.frame(prev = numeric(), curr = numeric()))
  }
  pick_col <- function(x) {
    col_match <- price_column
    if (!col_match %in% colnames(x)) {
      col_match <- if ("Close" %in% colnames(x)) "Close" else colnames(x)[1L]
    }
    col_match
  }
  prev_col <- pick_col(prev_xts)
  curr_col <- pick_col(curr_xts)
  rng <- paste0(as.Date(seam_date) - window_pre, "/", as.Date(seam_date) + window_post)
  prev_win <- prev_xts[rng, prev_col, drop = FALSE]
  curr_win <- curr_xts[rng, curr_col, drop = FALSE]
  if (!NROW(prev_win) || !NROW(curr_win)) {
    return(data.frame(prev = numeric(), curr = numeric()))
  }
  merged <- xts::merge.xts(prev_win, curr_win, join = "inner")
  if (!NROW(merged)) {
    return(data.frame(prev = numeric(), curr = numeric()))
  }
  prev_vals <- as.numeric(merged[, 1])
  curr_vals <- as.numeric(merged[, 2])
  ok <- is.finite(prev_vals) & is.finite(curr_vals) & prev_vals > 0 & curr_vals > 0
  data.frame(prev = prev_vals[ok], curr = curr_vals[ok])
}

.brf_compute_roll_ratios <- function(sym_vec,
                                     dates,
                                     lst_xts,
                                     seam_idx,
                                     clamp_ratio,
                                     roll_type,
                                     roll_control) {
  if (!length(seam_idx)) {
    return(numeric(0))
  }
  rc <- .brf_normalize_roll_control(roll_control)
  default_ratio <- function(ii) {
    prev_sym <- sym_vec[ii - 1L]
    curr_sym <- sym_vec[ii]
    seam_d <- dates[ii]
    prev_xts <- lst_xts[[prev_sym]]
    curr_xts <- lst_xts[[curr_sym]]
    .brf_compute_seam_ratio(
      prev_xts,
      curr_xts,
      seam_d,
      window_pre = rc$window_pre,
      window_post = rc$window_post,
      min_overlap = rc$min_overlap,
      clamp = clamp_ratio,
      price_column = rc$price_column
    )
  }
  windsor_ratio <- function(ii) {
    overlap <- .brf_overlap_prices(
      lst_xts[[sym_vec[ii - 1L]]],
      lst_xts[[sym_vec[ii]]],
      dates[ii],
      window_pre = rc$window_pre,
      window_post = rc$window_post,
      price_column = rc$price_column
    )
    if (nrow(overlap) < 3) {
      return(as.numeric(default_ratio(ii)))
    }
    log_spread <- log(overlap$prev) - log(overlap$curr)
    wins <- rc$winsor
    q <- stats::quantile(log_spread, probs = wins, na.rm = TRUE, type = 8)
    log_spread <- pmin(pmax(log_spread, q[1L]), q[2L])
    ratio <- exp(stats::median(log_spread, na.rm = TRUE))
    if (!is.finite(ratio) || ratio <= 0) {
      return(as.numeric(default_ratio(ii)))
    }
    max(min(ratio, clamp_ratio[2L]), clamp_ratio[1L])
  }
  regression_ratio <- function(ii) {
    overlap <- .brf_overlap_prices(
      lst_xts[[sym_vec[ii - 1L]]],
      lst_xts[[sym_vec[ii]]],
      dates[ii],
      window_pre = rc$window_pre,
      window_post = rc$window_post,
      price_column = rc$price_column
    )
    if (nrow(overlap) < 3) {
      return(as.numeric(default_ratio(ii)))
    }
    curr <- overlap$curr
    prev <- overlap$prev
    sst <- sum(curr^2)
    if (!is.finite(sst) || sst <= 0) {
      return(as.numeric(default_ratio(ii)))
    }
    slope <- sum(curr * prev) / sst
    if (!is.finite(slope) || slope <= 0) {
      return(as.numeric(default_ratio(ii)))
    }
    max(min(slope, clamp_ratio[2L]), clamp_ratio[1L])
  }
  if (is.function(roll_type)) {
    custom_ratios <- tryCatch(
      roll_type(sym_vec, dates, lst_xts, seam_idx, clamp_ratio, rc),
      error = function(...) roll_type(sym_vec, dates, lst_xts, seam_idx, clamp_ratio)
    )
    return(as.numeric(custom_ratios))
  }
  roll_type <- match.arg(roll_type, c("days_before_roll", "windsor_log_spread", "regression"))
  switch(
    roll_type,
    days_before_roll = vapply(seam_idx, function(ii) as.numeric(default_ratio(ii)), numeric(1), USE.NAMES = FALSE),
    windsor_log_spread = vapply(seam_idx, windsor_ratio, numeric(1), USE.NAMES = FALSE),
    regression = vapply(seam_idx, regression_ratio, numeric(1), USE.NAMES = FALSE)
  )
}

.brf_build_daily_front_chain <- function(agg_df,
                                         roll_days_before_expiry = 20,
                                         clamp_ratio = c(0.6, 1.6),
                                         roll_type = "days_before_roll",
                                         adjust_type = "fwd_rt",
                                         roll_control = NULL,
                                         include_roll_details = FALSE,
                                         contract_ranks = NULL,
                                         roll_stagger = 1L) {
  rc <- .brf_normalize_roll_control(roll_control)
  ranks <- .brf_normalize_contract_ranks(contract_ranks)
  ordered <- agg_df[order(agg_df$symbol, agg_df$refdate), , drop = FALSE]
  dup_mask <- !duplicated(ordered[, c("symbol", "refdate")], fromLast = TRUE)
  PX <- ordered[dup_mask, , drop = FALSE]
  if (!"volume" %in% names(PX)) {
    PX$volume <- NA_real_
  }
  PX$refdate <- as.Date(PX$refdate)
  PX <- PX[order(PX$refdate, PX$estimated_maturity, PX$symbol), , drop = FALSE]
  PX$rnk <- ave(
    PX$estimated_maturity,
    PX$refdate,
    FUN = function(x) {
      ranks <- rank(x, ties.method = "min", na.last = "keep")
      as.integer(ranks)
    }
  )
  required_prices <- c("open", "high", "low", "close")
  missing_prices <- setdiff(required_prices, names(PX))
  if (length(missing_prices)) {
    stop(
      "Aggregate data is missing required price columns: ",
      paste(missing_prices, collapse = ", "),
      call. = FALSE
    )
  }
  value_cols <- required_prices
  has_volume_col <- "volume" %in% names(PX)
  if (has_volume_col) {
    value_cols <- c(value_cols, "volume")
  }
  front <- PX[PX$rnk == ranks$primary, c("refdate", "symbol", value_cols), drop = FALSE]
  front_names <- c("refdate", "front_symbol", paste0("f_", value_cols))
  names(front) <- front_names
  front <- front[order(front$refdate), , drop = FALSE]
  nextc <- PX[PX$rnk == ranks$secondary, c("refdate", "symbol", value_cols), drop = FALSE]
  next_names <- c("refdate", "next_symbol", paste0("n_", value_cols))
  names(nextc) <- next_names
  nextc <- nextc[order(nextc$refdate), , drop = FALSE]
  daily <- merge(front, nextc, by = "refdate", all.x = TRUE, sort = TRUE)
  daily$front_rank <- ranks$primary
  daily$secondary_rank <- ranks$secondary
  days_by_symbol <- tapply(PX$refdate, PX$symbol, function(x) sort(unique(as.Date(x))), simplify = FALSE)
  maturity_by_symbol <- sapply(split(PX$estimated_maturity, PX$symbol), function(x) {
    vals <- unique(na.omit(x))
    if (!length(vals)) {
      as.Date(NA)
    } else {
      vals[1]
    }
  })
  roll_dates <- sapply(names(days_by_symbol), function(sym) {
    dvec <- days_by_symbol[[sym]]
    est <- maturity_by_symbol[[sym]]
    .brf_compute_roll_date_from_maturity(dvec, est, roll_days_before_expiry)
  })
  roll_df <- data.frame(
    front_symbol = names(roll_dates),
    front_roll_date = as.Date(roll_dates),
    stringsAsFactors = FALSE
  )
  daily <- merge(daily, roll_df, by = "front_symbol", all.x = TRUE, sort = FALSE)
  daily <- daily[order(daily$refdate), , drop = FALSE]
  roll_stagger <- as.integer(roll_stagger)
  if (is.na(roll_stagger) || roll_stagger < 1L) {
    roll_stagger <- 1L
  }
  has_secondary <- !is.na(daily$next_symbol)
  secondary_weight <- rep(0, NROW(daily))
  if (any(has_secondary)) {
    delta <- as.numeric(daily$front_roll_date - daily$refdate)
    secondary_weight[has_secondary & !is.na(delta) & delta <= 0] <- 1
    ramp_mask <- has_secondary & !is.na(delta) & delta > 0 & delta < roll_stagger
    secondary_weight[ramp_mask] <- 1 - (delta[ramp_mask] / roll_stagger)
  }
  secondary_weight[!is.finite(secondary_weight)] <- 0
  secondary_weight <- pmin(pmax(secondary_weight, 0), 1)
  secondary_weight[!has_secondary] <- 0
  primary_weight <- 1 - secondary_weight
  blend_fn <- function(primary_vals, secondary_vals) {
    res <- primary_weight * primary_vals + secondary_weight * secondary_vals
    both_na <- is.na(primary_vals) & is.na(secondary_vals)
    only_primary <- is.na(secondary_vals) & !is.na(primary_vals)
    only_secondary <- is.na(primary_vals) & !is.na(secondary_vals)
    res[both_na] <- NA_real_
    res[only_primary] <- primary_vals[only_primary]
    res[only_secondary] <- secondary_vals[only_secondary]
    res
  }
  cond_roll <- has_secondary &
    !is.na(daily$front_roll_date) &
    daily$refdate >= daily$front_roll_date
  daily$sel_symbol <- ifelse(cond_roll, daily$next_symbol, daily$front_symbol)
  daily$primary_weight <- primary_weight
  daily$secondary_weight <- secondary_weight
  daily$sel_open  <- blend_fn(daily$f_open,  daily$n_open)
  daily$sel_high  <- blend_fn(daily$f_high,  daily$n_high)
  daily$sel_low   <- blend_fn(daily$f_low,   daily$n_low)
  daily$sel_close <- blend_fn(daily$f_close, daily$n_close)
  if (has_volume_col && "f_volume" %in% names(daily) && "n_volume" %in% names(daily)) {
    daily$sel_volume <- blend_fn(daily$f_volume, daily$n_volume)
  } else if (has_volume_col && "f_volume" %in% names(daily)) {
    daily$sel_volume <- daily$f_volume
  }
  stopifnot(!anyNA(daily$refdate))
  has_vol <- "sel_volume" %in% names(daily) && any(is.finite(daily$sel_volume))
  mat <- cbind(daily$sel_open, daily$sel_high, daily$sel_low, daily$sel_close)
  colnames(mat) <- c("Open", "High", "Low", "Close")
  if (has_vol) {
    mat <- cbind(mat, Volume = daily$sel_volume)
  }
  no_adj <- xts::xts(mat, order.by = daily$refdate)
  no_adj <- .brf_sanitize_ohlcv_xts(no_adj)
  no_adj <- .brf_xts_deduplicate_daily_last(no_adj)
  parts <- .brf_split_contracts_xts(agg_df)
  lst_xts <- parts$lst_xts
  sym_vec <- daily$sel_symbol
  dates <- daily$refdate
  seam_idx <- which(sym_vec[-1] != sym_vec[-length(sym_vec)]) + 1L
  roll_type_internal <- roll_type
  if (is.character(roll_type_internal)) {
    roll_type_internal <- match.arg(roll_type_internal, c("days_before_roll", "windsor_log_spread", "regression"))
  } else if (!is.function(roll_type_internal)) {
    stop("roll_type must be a character identifier or a custom function.", call. = FALSE)
  }
  r_list <- numeric(0)
  if (length(seam_idx)) {
    r_list <- .brf_compute_roll_ratios(sym_vec, dates, lst_xts, seam_idx, clamp_ratio, roll_type_internal, rc)
  }
  stopifnot(is.numeric(r_list))
  seg_starts <- c(1L, seam_idx)
  nseg <- length(seg_starts)
  adjust_type <- match.arg(adjust_type, c("fwd_rt", "panama"))
  f <- rep(1.0, nseg)
  if (nseg >= 2) {
    if (length(r_list) != (nseg - 1L)) {
      stop("length(r_list) != nseg - 1 (inconsistency at the seams).", call. = FALSE)
    }
    if (adjust_type == "fwd_rt") {
      for (k in 2:nseg) {
        f[k] <- f[k - 1] * r_list[k - 1]
      }
    } else {
      for (k in (nseg - 1):1) {
        f[k] <- f[k + 1] / r_list[k]
      }
    }
  }
  if (!NCOL(no_adj)) {
    adj <- no_adj
  } else {
    adj_vals <- matrix(NA_real_, nrow = length(dates), ncol = NCOL(no_adj))
    colnames(adj_vals) <- colnames(no_adj)
    price_cols <- intersect(c("Open", "High", "Low", "Close"), colnames(no_adj))
    vol_col <- if (has_vol) match("Volume", colnames(no_adj)) else NA_integer_
    for (k in seq_len(nseg)) {
      i_start <- seg_starts[k]
      i_end <- if (k < nseg) (seg_starts[k + 1] - 1L) else length(dates)
      if (i_start <= i_end) {
        rows <- i_start:i_end
        adj_vals[rows, ] <- as.matrix(no_adj[rows, ])
        if (length(price_cols)) {
          pc_idx <- match(price_cols, colnames(no_adj))
          adj_vals[rows, pc_idx] <- adj_vals[rows, pc_idx] * f[k]
        }
        if (length(vol_col) == 1L && !is.na(vol_col)) {
          adj_vals[rows, vol_col] <- as.matrix(no_adj[rows, vol_col])
        }
      }
    }
    adj <- xts::xts(adj_vals, order.by = zoo::index(no_adj))
  }
  adj <- .brf_sanitize_ohlcv_xts(adj)
  adj <- .brf_xts_deduplicate_daily_last(adj)
  if (isTRUE(include_roll_details)) {
    daily <- .brf_enrich_roll_details(daily, seam_idx, r_list, rc)
  }
  active_symbols <- xts::xts(as.character(sym_vec), order.by = dates)
  active_symbols <- active_symbols[zoo::index(no_adj)]
  attr(no_adj, "active_symbol") <- active_symbols
  attr(adj, "active_symbol") <- active_symbols
  list(no_adj = no_adj, adj = adj, daily_table = daily)
}

.brf_to_posix_br_tz <- function(x_date) {
  if (inherits(x_date, "POSIXt")) {
    return(lubridate::force_tz(x_date, tzone = "America/Sao_Paulo"))
  }
  lubridate::force_tz(
    as.POSIXct(as.Date(x_date), tz = "UTC"),
    tzone = "America/Sao_Paulo"
  )
}

.brf_xts_force_tz <- function(x_xts) {
  if (!xts::is.xts(x_xts) || !NROW(x_xts)) {
    return(x_xts)
  }
  idx <- .brf_to_posix_br_tz(zoo::index(x_xts))
  y <- xts::xts(zoo::coredata(x_xts), order.by = idx, tzone = "America/Sao_Paulo")
  attr_active <- attr(x_xts, "active_symbol")
  if (xts::is.xts(attr_active) && NROW(attr_active)) {
    attr(y, "active_symbol") <- xts::xts(
      zoo::coredata(attr_active),
      order.by = .brf_to_posix_br_tz(zoo::index(attr_active)),
      tzone = "America/Sao_Paulo"
    )
  }
  attr(y, "fut_tick_size") <- attr(x_xts, "fut_tick_size")
  attr(y, "fut_multiplier") <- attr(x_xts, "fut_multiplier")
  y
}

.brf_append_return_columns <- function(x_xts) {
  if (!xts::is.xts(x_xts) || !"Close" %in% colnames(x_xts)) {
    return(x_xts)
  }
  close <- x_xts[, "Close"]
  prev <- xts::lag.xts(close, k = 1)
  ratio <- close / prev
  ratio[!is.finite(ratio) | ratio <= 0] <- NA_real_
  discrete <- ratio - 1
  log_r <- log(ratio)
  discrete[is.na(discrete)] <- 0
  log_r[is.na(log_r)] <- 0
  if (NROW(discrete)) {
    discrete[1] <- 0
  }
  if (NROW(log_r)) {
    log_r[1] <- 0
  }
  x_xts$Discrete <- as.numeric(discrete)
  x_xts$Log <- as.numeric(log_r)
  x_xts
}

.brf_cumulative_return <- function(ret) {
  ret <- ret[is.finite(ret)]
  if (!length(ret)) {
    return(0)
  }
  prod(1 + ret) - 1
}

.brf_annualized_return <- function(ret, periods_per_year = 252) {
  ret <- ret[is.finite(ret)]
  n <- length(ret)
  if (!n) {
    return(0)
  }
  total <- prod(1 + ret)
  if (!is.finite(total) || total <= 0) {
    return(NA_real_)
  }
  total^(periods_per_year / n) - 1
}

.brf_continuous_meta <- function(ticker_root) {
  r <- toupper(ticker_root)
  list(
    tick_size     = if (r == "CCM") 0.01 else if (r == "BGI") 0.05 else 0.01,
    fut_multiplier = if (r == "CCM") 450 else if (r == "BGI") 330 else 1
  )
}

#' Build a continuous futures series with front/next rolls
#'
#' Construct a continuous daily series for a futures ticker root by rolling
#' from the front contract into the next contract a fixed number of business
#' days before the estimated maturity.
#'
#' @param ticker_root Commodity root symbol (e.g. `"DI1"` or `"WIN"`).
#' @param build_type Adjustment style applied to the stitched chain. `"fwd_rt"`
#'   (forward-ratio) multiplies the ratio through to subsequent contracts so the
#'   most recent prices remain untouched, while `"panama"` (classic Panama style)
#'   scales historical segments backward so the oldest leg is preserved and
#'   newer legs are shifted.
#' @param roll_type Roll specification describing how contracts are stitched.
#'   Supply one of the helper constructors such as [days_before_roll()],
#'   [windsor_log_spread_roll()], [regression_roll()], or [custom_roll()], or
#'   provide a custom function that matches the legacy seam interface.
#' @param adjusted When `TRUE`, return the back-adjusted continuous series;
#'   otherwise the unadjusted (stitched) series is returned.
#' @param single_xts If `TRUE`, return a single xts object; when `FALSE`, return
#'   a named list containing the series.
#' @param debug If `TRUE`, emit summary information about cumulative and
#'   annualised returns for the continuous series.
#' @param everything When `TRUE`, return a list containing the aggregate data,
#'   the unadjusted series, the adjusted series, and the daily contract table.
#' @param data_dir Optional directory containing cached files for `ticker_root`.
#' @param which Storage location passed to [tools::R_user_dir()] when
#'   `data_dir` is `NULL`.
#' @param source Data source to build the series from: `"local"` reads cached
#'   aggregates via [brf_get_aggregate()] while `"sm_api"` attempts to call
#'   `sm_get_data()` (when available) and falls back to the local cache with a
#'   warning.
#' @param include_older When `TRUE`, legacy contracts whose symbols use
#'   Portuguese month abbreviations (e.g. `"BGIABR1"`) are normalised to their
#'   modern equivalents so they participate in the roll chain. Set to `FALSE`
#'   to keep only modern `"BGIZ01"`-style contracts.
#' @param maturities Optional vector of month codes (e.g. `c("F", "U")`) to
#'   restrict the chain to specific maturities. Use `"all"` (default) to keep
#'   every available contract month.
#'
#' @details The function automatically merges legacy naming conventions from the
#'   B3 bulletins. Portuguese month suffixes are mapped to standard CME-style
#'   month codes when `include_older = TRUE`, and ticker-root aliases (such as
#'   the historical `"ICN"` commodity root for what is now traded as `"CCM"` or
#'   `"BOI"` for `"BGI"`) are consolidated before the roll logic runs. This keeps
#'   historical data continuous through renames without requiring manual
#'   preprocessing.
#'
#' @return By default, invisibly returns an xts object. When `single_xts` is
#'   `FALSE`, returns a named list containing the requested series; when
#'   `everything` is `TRUE`, returns a list with the aggregate data, the
#'   unadjusted chain, the adjusted chain, and the daily contract metadata.
#' @export

brf_build_continuous_series <- function(ticker_root,
                                        build_type = c("fwd_rt", "panama"),
                                        roll_type = days_before_roll(),
                                        adjusted = TRUE,
                                        single_xts = TRUE,
                                        debug = FALSE,
                                        everything = FALSE,
                                        data_dir = NULL,
                                        which = c("cache", "data", "config"),
                                        source = c("local", "sm_api"),
                                        include_older = TRUE,
                                        maturities = "all") {
  which <- match.arg(which)
  source <- match.arg(source)
  build_type <- match.arg(build_type)
  roll_spec <- .brf_resolve_roll_spec(roll_type)
  roll_type_internal <- roll_spec$roll_type
  roll_days_before_expiry <- roll_spec$roll_days_before_expiry
  clamp_ratio <- roll_spec$clamp_ratio
  roll_control <- roll_spec$roll_control
  roll_stagger <- roll_spec$roll_stagger
  contract_ranks <- roll_spec$contract_ranks
  include_roll_details <- roll_spec$include_roll_details
  if (is.character(roll_type_internal)) {
    roll_type_internal <- match.arg(roll_type_internal, c("days_before_roll", "windsor_log_spread", "regression"))
  } else if (!is.function(roll_type_internal)) {
    stop("roll_type must be a recognised identifier or a custom function.", call. = FALSE)
  }
  contract_ranks <- .brf_normalize_contract_ranks(contract_ranks)
  normalized <- .brf_normalize_ticker_root(ticker_root)
  normalized <- normalized[!is.na(normalized)]
  if (!length(normalized)) {
    stop("ticker_root cannot be missing or empty.", call. = FALSE)
  }
  ticker_root_norm <- normalized[1L]
  fetch_local <- function() {
    suppressMessages(
      brf_get_aggregate(
        ticker_root_norm,
        data_dir = data_dir,
        which = which,
        type = "full",
        return = "agg"
      )
    )
  }
  aggregate_raw <- switch(
    source,
    local = fetch_local(),
    sm_api = {
      api_res <- try(
        sm_get_data(
          ticker_root_norm,
          future_history = TRUE,
          single_xts = TRUE,
          set_instruments = FALSE
        ),
        silent = FALSE
      )
      if (inherits(api_res, "try-error") || is.null(api_res)) {
        warning(
          "sm_get_data() is not available; falling back to cached aggregate data.",
          call. = FALSE
        )
        fetch_local()
      } else {
        api_res
      }
    }
  )
  aggregate <- .brf_prepare_continuous_aggregate(aggregate_raw, ticker_root_norm)
  if (isTRUE(include_older)) {
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
  aggregate <- .brf_filter_by_maturity(aggregate, maturities)
  if (!nrow(aggregate)) {
    stop("No data available after applying the requested maturities.", call. = FALSE)
  }
  chain <- .brf_build_daily_front_chain(
    aggregate,
    roll_days_before_expiry = roll_days_before_expiry,
    clamp_ratio = clamp_ratio,
    roll_type = roll_type_internal,
    adjust_type = build_type,
    roll_control = roll_control,
    include_roll_details = include_roll_details,
    contract_ranks = contract_ranks,
    roll_stagger = roll_stagger
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
  if (isTRUE(debug)) {
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
  if (isTRUE(everything)) {
    result <- list(
      aggregate = aggregate,
      no_adj = no_adj,
      adj = adj,
      daily = daily
    )
    return(invisible(result))
  }
  if (!isTRUE(single_xts)) {
    if (isTRUE(adjusted)) {
      name_adj <- paste0(ticker_root_norm, "FUT_adj_roll", roll_days_before_expiry)
      result_adj <- stats::setNames(list(adj), name_adj)
      return(invisible(result_adj))
    }
    name_noadj <- paste0(ticker_root_norm, "FUT_noadj_roll", roll_days_before_expiry)
    result_noadj <- stats::setNames(list(no_adj), name_noadj)
    return(invisible(result_noadj))
  }
  if (isTRUE(adjusted)) {
    return(invisible(adj))
  }
  invisible(no_adj)
}
