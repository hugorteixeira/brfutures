.brf_di_env <- new.env(parent = emptyenv())
.brf_di_env$calendar <- NULL

.brf_di_month_letter <- c(F = 1L, G = 2L, H = 3L, J = 4L, K = 5L, M = 6L, N = 7L, Q = 8L, U = 9L, V = 10L, X = 11L, Z = 12L)

.brf_di_has_bizdays <- function() {
  requireNamespace("bizdays", quietly = TRUE)
}

.brf_di_require_bizdays <- function() {
  if (!.brf_di_has_bizdays()) {
    stop(
      "Package 'bizdays' is required for DI futures helpers. ",
      "Install it via install.packages('bizdays').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.brf_di_resolve_calendar <- function(cal = NULL) {
  if (!is.null(cal)) {
    return(cal)
  }
  .brf_di_require_bizdays()
  if (is.null(.brf_di_env$calendar)) {
    .brf_di_env$calendar <- bizdays::create.calendar(
      name = "Brazil/ANBIMA_brf",
      holidays = bizdays::holidays("Brazil/ANBIMA"),
      weekdays = c("saturday", "sunday")
    )
  }
  .brf_di_env$calendar
}

.brf_di_get_tick_size <- function(mm, basis_date, rule_change_date = as.Date("2025-08-25")) {
  basis_date <- as.Date(basis_date)
  if (basis_date < rule_change_date) {
    if (mm <= 3) 0.001 else if (mm <= 60) 0.005 else 0.010
  } else {
    if (mm <= 3) 0.001 else 0.005
  }
}

.brf_di_snap_rate_to_tick <- function(rates, mm, basis_date, rule_change_date = as.Date("2025-08-25")) {
  tick <- .brf_di_get_tick_size(mm, basis_date, rule_change_date)
  round(rates / tick) * tick
}

.brf_di_snap_di_rates <- function(rates, mm, basis_date, rule_change_date = as.Date("2025-08-25")) {
  mapply(
    function(rate, month_bucket, basis) .brf_di_snap_rate_to_tick(rate, month_bucket, basis, rule_change_date),
    rates,
    mm,
    as.Date(basis_date),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
}

.brf_di_months_between_floor <- function(basis_date, maturity_date) {
  basis_date <- as.Date(basis_date)
  maturity_date <- as.Date(maturity_date)
  len <- max(length(basis_date), length(maturity_date))
  if (len == 0L) {
    return(integer())
  }
  basis_date <- rep_len(basis_date, len)
  maturity_date <- rep_len(maturity_date, len)
  out <- rep(NA_integer_, len)
  valid <- !is.na(basis_date) & !is.na(maturity_date)
  if (!any(valid)) {
    return(out)
  }
  bd <- basis_date[valid]
  md <- maturity_date[valid]
  year_diff <- as.integer(format(md, "%Y")) - as.integer(format(bd, "%Y"))
  month_diff <- as.integer(format(md, "%m")) - as.integer(format(bd, "%m"))
  diff <- year_diff * 12L + month_diff
  day_basis <- as.integer(format(bd, "%d"))
  day_maturity <- as.integer(format(md, "%d"))
  diff <- diff - as.integer(day_maturity < day_basis)
  diff[diff < 0] <- 0L
  out[valid] <- diff
  out
}

.brf_di_biz_n <- function(basis_date, maturity_date, cal, include_basis_day = TRUE) {
  .brf_di_require_bizdays()
  n <- bizdays::bizdays(as.Date(basis_date), as.Date(maturity_date), cal)
  if (isTRUE(include_basis_day)) {
    n <- n + as.integer(bizdays::is.bizday(as.Date(basis_date), cal))
  }
  if (any(n <= 0, na.rm = TRUE)) {
    stop("Non-positive valid-days. Check basis/maturity dates for DI contract.", call. = FALSE)
  }
  n
}

.brf_di_resolve_tenor <- function(maturity_date,
                                  basis_date,
                                  cal,
                                  include_basis_day = TRUE,
                                  allow_coercion = FALSE) {
  basis_date <- as.Date(basis_date)

  if (inherits(maturity_date, "Date")) {
    maturity <- as.Date(maturity_date)
  } else if (is.numeric(maturity_date)) {
    n <- as.integer(maturity_date)
    if (n <= 0) stop("'maturity_date' as numeric must be positive business days.", call. = FALSE)
    return(list(
      valid_days = n,
      months_bucket = as.numeric(n) / 21,
      maturity_date = NA
    ))
  } else if (allow_coercion) {
    maturity <- try(as.Date(maturity_date), silent = TRUE)
    if (inherits(maturity, "try-error") || is.na(maturity)) {
      stop("'maturity_date' must be Date or numeric business days (or coercible to Date).", call. = FALSE)
    }
  } else {
    stop("'maturity_date' must be Date or numeric business days.", call. = FALSE)
  }

  n <- .brf_di_biz_n(basis_date, maturity, cal, include_basis_day = include_basis_day)
  mm <- .brf_di_months_between_floor(basis_date, maturity)
  if (any(n <= 0, na.rm = TRUE)) {
    stop("Number of business days to maturity must be positive.", call. = FALSE)
  }

  list(
    valid_days = n,
    months_bucket = mm,
    maturity_date = maturity
  )
}

.brf_di_pu_from_rate <- function(rates, valid_days, round_pu = TRUE) {
  rates <- as.numeric(rates)
  valid_days <- as.numeric(valid_days)
  pu <- 1e5 / (1 + rates / 100)^(valid_days / 252)
  bad <- !is.finite(rates) | rates <= -100 | !is.finite(valid_days) | valid_days <= 0
  pu[bad] <- NA_real_
  if (isTRUE(round_pu)) {
    pu <- round(pu, 2L)
  }
  pu
}

.brf_di_rate_from_pu <- function(pu, valid_days) {
  pu <- as.numeric(pu)
  valid_days <- as.numeric(valid_days)
  rates <- 100 * ((1e5 / pu)^(252 / valid_days) - 1)
  bad <- !is.finite(pu) | pu <= 0 | !is.finite(valid_days) | valid_days <= 0
  rates[bad] <- NA_real_
  rates
}

.brf_di_tick_value_from_close <- function(close_rates, valid_days, tick_size) {
  n <- length(close_rates)
  if (!n) {
    return(numeric())
  }
  rates <- as.numeric(close_rates)
  valid_days <- rep_len(as.numeric(valid_days), n)
  tick_size <- rep_len(as.numeric(tick_size), n)
  result <- rep(NA_real_, n)
  valid <- is.finite(rates) &
    is.finite(valid_days) & valid_days > 0 &
    is.finite(tick_size) & tick_size > 0
  if (!any(valid)) {
    return(result)
  }
  r_close <- rates[valid] / 100
  tick_decimal <- tick_size[valid] / 100
  exponent <- valid_days[valid] / 252
  base_0 <- 1 + r_close
  base_1 <- 1 + r_close + tick_decimal
  ok <- is.finite(exponent) & base_0 > 0 & base_1 > 0
  if (any(ok)) {
    pu0 <- 1e5 / (base_0[ok]^exponent[ok])
    pu1 <- 1e5 / (base_1[ok]^exponent[ok])
    delta <- pu1 - pu0
  } else {
    delta <- numeric()
  }
  value <- rep(NA_real_, sum(valid))
  if (length(delta)) {
    value[ok] <- delta
  }
  result[valid] <- value
  result
}

.brf_di_resolve_ohlc_columns <- function(x) {
  nm <- tolower(colnames(x))
  find_first <- function(candidates) {
    idx <- match(candidates, nm, nomatch = 0L)
    idx <- idx[idx > 0L]
    if (length(idx) == 0L) {
      return(NA_integer_)
    }
    idx[1L]
  }
  open_idx <- find_first(c("open", "o"))
  high_idx <- find_first(c("high", "h"))
  low_idx <- find_first(c("low", "l"))
  close_idx <- find_first(c("close", "c", "last", "settle", "settlement_price"))
  if (any(is.na(c(open_idx, high_idx, low_idx, close_idx)))) {
    stop("Could not resolve OHLC columns (require open/high/low/close as percent rates).", call. = FALSE)
  }
  volume_idx <- find_first(c("volume", "vol", "contracts_traded", "qty", "contracts", "volume_brl", "volume_brls"))
  list(
    open = open_idx,
    high = high_idx,
    low = low_idx,
    close = close_idx,
    volume = volume_idx
  )
}

.brf_di_resolve_maturity_input <- function(maturity_date, x = NULL) {
  if (!is.null(maturity_date)) {
    return(maturity_date)
  }
  if (is.null(x)) {
    return(maturity_date)
  }

  attr_val <- attr(x, "maturity", exact = TRUE)
  if (is.null(attr_val)) {
    attrs <- try(xts::xtsAttributes(x), silent = TRUE)
    if (!inherits(attrs, "try-error") && length(attrs) && "maturity" %in% names(attrs)) {
      attr_val <- attrs[["maturity"]]
    }
  }
  if (is.null(attr_val)) {
    return(maturity_date)
  }

  if (length(attr_val) > 1) attr_val <- attr_val[[1]]
  if (is.list(attr_val)) attr_val <- attr_val[[1]]

  if (inherits(attr_val, "Date")) {
    return(attr_val)
  }
  if (inherits(attr_val, "POSIXt")) {
    return(as.Date(attr_val))
  }
  if (is.character(attr_val)) {
    parsed <- as.Date(attr_val)
    if (any(is.na(parsed)) && grepl("^\\d{8}$", attr_val)) {
      parsed <- as.Date(attr_val, format = "%Y%m%d")
    }
    if (any(is.na(parsed))) stop("Could not coerce 'maturity' attribute to Date.", call. = FALSE)
    return(parsed)
  }

  attr_val
}

#' Derive DI maturity date from a B3 ticker
#'
#' @param ticker Character scalar such as `"DI1F25"`.
#' @param cal Optional `bizdays` calendar.
#' @return A `Date` with the contract maturity.
#' @export
di_maturity_from_ticker <- function(ticker, cal = NULL) {
  stopifnot(is.character(ticker), length(ticker) == 1)
  cal <- .brf_di_resolve_calendar(cal)

  month_code <- substr(ticker, 4, 4)
  mm <- .brf_di_month_letter[month_code]
  if (is.na(mm)) stop("Cannot parse month from ticker: ", ticker, call. = FALSE)

  y2 <- as.integer(substr(ticker, 5, 6))
  if (is.na(y2)) stop("Cannot parse year from ticker: ", ticker, call. = FALSE)
  y4 <- ifelse(y2 >= 90, 1900 + y2, 2000 + y2)

  first_day <- as.Date(sprintf("%04d-%02d-01", y4, mm))
  bizdays::adjust.next(first_day, cal)
}

#' DI futures notional (PU) from annualized rates
#'
#' @inheritParams di_maturity_from_ticker
#' @param rates Annualized DI rate(s) in percent.
#' @param maturity_date Maturity `Date` or number of business days to expiry.
#' @param basis_date Trade/reference date.
#' @param include_basis_day Whether to add the basis day to the business-day count.
#' @param snap_to_tick Snap `rates` to the DI tick grid before pricing.
#' @param round_pu Round the PU to two decimals (B3 convention).
#' @param rule_change_date Date where the DI tick regime changes.
#' @return A list with elements `valid_days`, `pu`, `tick_size`, and `tick_value`.
#' @export
calculate_futures_di_notional <- function(rates,
                                          maturity_date,
                                          basis_date = Sys.Date(),
                                          cal = NULL,
                                          include_basis_day = TRUE,
                                          snap_to_tick = TRUE,
                                          round_pu = TRUE,
                                          rule_change_date = as.Date("2025-08-25")) {
  positionsizer::ps_di_rate_to_pu(
    rates = rates,
    maturity_date = maturity_date,
    basis_date = basis_date,
    cal = cal,
    include_basis_day = include_basis_day,
    snap_to_tick = snap_to_tick,
    round_pu = round_pu,
    rule_change_date = rule_change_date
  )
}

#' DI futures rates from notional (PU)
#'
#' @inheritParams calculate_futures_di_notional
#' @param pu Futures price (PU) in monetary units.
#' @return A list with elements `valid_days`, `rates`, `tick_size`, and `tick_value`.
#' @export
calculate_futures_di_rates <- function(pu,
                                       maturity_date,
                                       basis_date = Sys.Date(),
                                       cal = NULL,
                                       include_basis_day = TRUE,
                                       snap_to_tick = TRUE,
                                       rule_change_date = as.Date("2025-08-25")) {
  positionsizer::ps_di_pu_to_rate(
    pu = pu,
    maturity_date = maturity_date,
    basis_date = basis_date,
    cal = cal,
    include_basis_day = include_basis_day,
    snap_to_tick = snap_to_tick,
    rule_change_date = rule_change_date
  )
}

#' Estimate DI settlement PU from daily OHLC data
#'
#' @inheritParams calculate_futures_di_notional
#' @param open,high,low,close Daily OHLC rates (percent per year).
#' @param average_price Optional VWAP (percent per year).
#' @param basis_date Date of the OHLC bar.
#' @param prefer Anchor preference order.
#' @param bias_pp Fixed adjustment in percentage points added to the anchor rate.
#' @return A list with elements `rate_anchor`, `rate_adj`, `pu_est`, `valid_days`, and `tick_size`.
#' @export
estimate_pu_from_daily_ohlc <- function(open, high, low, close,
                                        average_price = NULL,
                                        maturity_date,
                                        basis_date,
                                        cal = NULL,
                                        include_basis_day = TRUE,
                                        prefer = c("average_price", "close", "mid", "open"),
                                        bias_pp = 0,
                                        snap_to_tick = TRUE,
                                        rule_change_date = as.Date("2025-08-25")) {
  cal <- .brf_di_resolve_calendar(cal)
  basis_date <- as.Date(basis_date)
  prefer <- match.arg(prefer)

  tenor <- .brf_di_resolve_tenor(
    maturity_date = maturity_date,
    basis_date = basis_date,
    cal = cal,
    include_basis_day = include_basis_day
  )
  n <- tenor$valid_days
  mm <- tenor$months_bucket

  tick_size <- .brf_di_get_tick_size(mm, basis_date, rule_change_date)

  pick_first <- function(...) {
    xs <- list(...)
    xs <- xs[!vapply(xs, function(z) is.null(z) || !is.finite(z), logical(1))]
    if (length(xs) == 0) stop("No valid OHLC values.", call. = FALSE)
    xs[[1]]
  }

  mid <- if (is.finite(high) && is.finite(low)) (high + low) / 2 else NA_real_

  rate_anchor <- switch(prefer,
    average_price = pick_first(average_price, close, mid, open),
    close = pick_first(close, average_price, mid, open),
    mid = pick_first(mid, average_price, close, open),
    open = pick_first(open, average_price, close, mid)
  )

  rate_adj <- as.numeric(rate_anchor) + as.numeric(bias_pp)
  if (snap_to_tick) {
    rate_adj <- .brf_di_snap_di_rates(rate_adj, mm, basis_date, rule_change_date)
  }

  pu <- .brf_di_pu_from_rate(rate_adj, n, round_pu = TRUE)

  list(
    rate_anchor = as.numeric(rate_anchor),
    rate_adj = as.numeric(rate_adj),
    pu_est = as.numeric(pu),
    valid_days = as.integer(n),
    tick_size = tick_size
  )
}

#' Exponentially-weighted bias in percentage points
#'
#' @param settle_rate_hist Historical settle rates (percent).
#' @param anchor_rate_hist Historical anchor rates (percent).
#' @param lambda Smoothing factor in (0, 1].
#' @return Exponentially-weighted bias (settle minus anchor) in percentage points.
#' @export
learn_bias_pp_ema <- function(settle_rate_hist, anchor_rate_hist, lambda = 0.2) {
  stopifnot(length(settle_rate_hist) == length(anchor_rate_hist))
  if (!(lambda > 0 && lambda <= 1)) stop("'lambda' must be in (0, 1].", call. = FALSE)

  diffs <- settle_rate_hist - anchor_rate_hist
  ema <- 0
  for (d in diffs) {
    if (!is.finite(d)) next
    ema <- lambda * d + (1 - lambda) * ema
  }
  as.numeric(ema)
}

#' Convert DI OHLC rates to PU columns (xts)
#'
#' @param x xts object with daily OHLC rates (percent per year).
#' @param maturity_date Maturity `Date` or business days to expiry. If `NULL`,
#'   the function looks for a `"maturity"` attribute on `x`.
#' @inheritParams calculate_futures_di_notional
#' @param round_pu Round the output PUs to two decimals.
#' @return An xts object containing the PU columns.
#' @export
ohlc_rates_to_pu_xts <- function(x,
                                 maturity_date = NULL,
                                 cal = NULL,
                                 include_basis_day = TRUE,
                                 snap_to_tick = FALSE,
                                 rule_change_date = as.Date("2025-08-25"),
                                 round_pu = TRUE) {
  if (!xts::is.xts(x)) stop("'x' must be an xts object.", call. = FALSE)
  if (NROW(x) == 0) {
    return(x)
  }

  cal <- .brf_di_resolve_calendar(cal)
  maturity_date <- .brf_di_resolve_maturity_input(maturity_date, x)
  if (is.null(maturity_date)) {
    stop("Provide 'maturity_date' or set a 'maturity' attribute on 'x'.", call. = FALSE)
  }

  cols <- .brf_di_resolve_ohlc_columns(x)
  r_open <- as.numeric(x[, cols$open])
  r_high <- as.numeric(x[, cols$high])
  r_low <- as.numeric(x[, cols$low])
  r_close <- as.numeric(x[, cols$close])

  idx_dates <- zoo::index(x)
  tz_val <- attr(x, "tzone")
  if (!is.null(tz_val) && !inherits(idx_dates, "POSIXt")) {
    idx_dates <- as.POSIXct(idx_dates, tz = tz_val)
  }

  if (inherits(maturity_date, "Date")) {
    md <- as.Date(maturity_date)
    n_vec <- vapply(idx_dates, function(d) .brf_di_biz_n(d, md, cal, include_basis_day = include_basis_day), numeric(1))
    mm_vec <- vapply(idx_dates, function(d) .brf_di_months_between_floor(d, md), numeric(1))
  } else if (is.numeric(maturity_date)) {
    n_const <- as.integer(maturity_date)
    if (n_const <= 0) stop("'maturity_date' as numeric must be positive business days.", call. = FALSE)
    n_vec <- rep.int(n_const, length(idx_dates))
    mm_vec <- rep_len(as.numeric(n_const) / 21, length(idx_dates))
  } else {
    stop("'maturity_date' must be a Date or numeric business days.", call. = FALSE)
  }

  if (snap_to_tick) {
    r_open <- .brf_di_snap_di_rates(r_open, mm_vec, idx_dates, rule_change_date)
    r_high <- .brf_di_snap_di_rates(r_high, mm_vec, idx_dates, rule_change_date)
    r_low <- .brf_di_snap_di_rates(r_low, mm_vec, idx_dates, rule_change_date)
    r_close <- .brf_di_snap_di_rates(r_close, mm_vec, idx_dates, rule_change_date)
  }

  PU_o <- .brf_di_pu_from_rate(r_open, n_vec, round_pu = round_pu)
  # Rate and PU move inversely: the highest PU comes from the lowest rate.
  PU_h <- .brf_di_pu_from_rate(r_low, n_vec, round_pu = round_pu)
  PU_l <- .brf_di_pu_from_rate(r_high, n_vec, round_pu = round_pu)
  PU_c <- .brf_di_pu_from_rate(r_close, n_vec, round_pu = round_pu)

  xts::xts(
    cbind(
      PU_o = as.numeric(PU_o),
      PU_h = as.numeric(PU_h),
      PU_l = as.numeric(PU_l),
      PU_c = as.numeric(PU_c)
    ),
    order.by = idx_dates,
    tzone = tz_val
  )
}

#' Augment DI OHLC data with PU and diagnostics
#'
#' @param x xts with DI OHLC rates (percent per year).
#' @param snap_rates_back Snap the round-tripped rates back to the DI tick grid.
#' @param include_diagnostics When `TRUE`, adds diagnostics columns.
#' @inheritParams ohlc_rates_to_pu_xts
#' @return An xts object with OHLC, PU, and optional diagnostics columns.
#' @export
di_ohlc_to_pu_augmented_xts <- function(x,
                                        maturity_date = NULL,
                                        cal = NULL,
                                        include_basis_day = TRUE,
                                        snap_to_tick = FALSE,
                                        rule_change_date = as.Date("2025-08-25"),
                                        round_pu = FALSE,
                                        snap_rates_back = FALSE,
                                        include_diagnostics = FALSE) {
  if (!xts::is.xts(x)) stop("'x' must be an xts object.", call. = FALSE)
  if (NROW(x) == 0) {
    return(x)
  }

  cal <- .brf_di_resolve_calendar(cal)
  if (snap_rates_back && !include_diagnostics) include_diagnostics <- TRUE
  maturity_date <- .brf_di_resolve_maturity_input(maturity_date, x)
  if (is.null(maturity_date)) {
    stop("Provide 'maturity_date' or set a 'maturity' attribute on 'x'.", call. = FALSE)
  }

  cols <- .brf_di_resolve_ohlc_columns(x)
  r_open <- as.numeric(x[, cols$open])
  r_high <- as.numeric(x[, cols$high])
  r_low <- as.numeric(x[, cols$low])
  r_close <- as.numeric(x[, cols$close])

  idx_dates <- zoo::index(x)
  tz_val <- attr(x, "tzone")
  if (!is.null(tz_val) && !inherits(idx_dates, "POSIXt")) {
    idx_dates <- as.POSIXct(idx_dates, tz = tz_val)
  }

  if (inherits(maturity_date, "Date")) {
    md <- as.Date(maturity_date)
    n_vec <- vapply(idx_dates, function(d) .brf_di_biz_n(d, md, cal, include_basis_day = include_basis_day), numeric(1))
    mm_vec <- vapply(idx_dates, function(d) .brf_di_months_between_floor(d, md), numeric(1))
  } else if (is.numeric(maturity_date)) {
    n_const <- as.integer(maturity_date)
    if (n_const <= 0) stop("'maturity_date' as numeric must be positive business days.", call. = FALSE)
    n_vec <- rep.int(n_const, length(idx_dates))
    mm_vec <- rep_len(as.numeric(n_const) / 21, length(idx_dates))
  } else {
    stop("'maturity_date' must be a Date or numeric business days.", call. = FALSE)
  }

  if (snap_to_tick) {
    r_open <- .brf_di_snap_di_rates(r_open, mm_vec, idx_dates, rule_change_date)
    r_high <- .brf_di_snap_di_rates(r_high, mm_vec, idx_dates, rule_change_date)
    r_low <- .brf_di_snap_di_rates(r_low, mm_vec, idx_dates, rule_change_date)
    r_close <- .brf_di_snap_di_rates(r_close, mm_vec, idx_dates, rule_change_date)
  }

  PU_o <- .brf_di_pu_from_rate(r_open, n_vec, round_pu = round_pu)
  # Rate and PU move inversely: the highest PU comes from the lowest rate.
  PU_h <- .brf_di_pu_from_rate(r_low, n_vec, round_pu = round_pu)
  PU_l <- .brf_di_pu_from_rate(r_high, n_vec, round_pu = round_pu)
  PU_c <- .brf_di_pu_from_rate(r_close, n_vec, round_pu = round_pu)

  base_cols <- cbind(
    open = r_open,
    high = r_high,
    low = r_low,
    close = r_close
  )
  if (!is.na(cols$volume)) {
    base_cols <- cbind(base_cols, volume = as.numeric(x[, cols$volume]))
  }

  res_mat <- cbind(
    base_cols,
    PU_o = as.numeric(PU_o),
    PU_h = as.numeric(PU_h),
    PU_l = as.numeric(PU_l),
    PU_c = as.numeric(PU_c)
  )

  if (include_diagnostics || snap_rates_back) {
    adjOpen <- .brf_di_rate_from_pu(PU_o, n_vec)
    adjHigh <- .brf_di_rate_from_pu(PU_l, n_vec)
    adjLow <- .brf_di_rate_from_pu(PU_h, n_vec)
    adjClose <- .brf_di_rate_from_pu(PU_c, n_vec)

    if (snap_rates_back) {
      adjOpen <- .brf_di_snap_di_rates(adjOpen, mm_vec, idx_dates, rule_change_date)
      adjHigh <- .brf_di_snap_di_rates(adjHigh, mm_vec, idx_dates, rule_change_date)
      adjLow <- .brf_di_snap_di_rates(adjLow, mm_vec, idx_dates, rule_change_date)
      adjClose <- .brf_di_snap_di_rates(adjClose, mm_vec, idx_dates, rule_change_date)
    }

    adjPU_o <- .brf_di_pu_from_rate(adjOpen, n_vec, round_pu = round_pu)
    adjPU_h <- .brf_di_pu_from_rate(adjLow, n_vec, round_pu = round_pu)
    adjPU_l <- .brf_di_pu_from_rate(adjHigh, n_vec, round_pu = round_pu)
    adjPU_c <- .brf_di_pu_from_rate(adjClose, n_vec, round_pu = round_pu)

    if (include_diagnostics) {
      res_mat <- cbind(
        res_mat,
        adjOpen = as.numeric(adjOpen),
        adjHigh = as.numeric(adjHigh),
        adjLow = as.numeric(adjLow),
        adjClose = as.numeric(adjClose),
        adjPU_o = as.numeric(adjPU_o),
        adjPU_h = as.numeric(adjPU_h),
        adjPU_l = as.numeric(adjPU_l),
        adjPU_c = as.numeric(adjPU_c),
        diffOpen_pp = as.numeric(adjOpen - r_open),
        diffHigh_pp = as.numeric(adjHigh - r_high),
        diffLow_pp = as.numeric(adjLow - r_low),
        diffClose_pp = as.numeric(adjClose - r_close),
        diffPU_o = as.numeric(adjPU_o - PU_o),
        diffPU_h = as.numeric(adjPU_h - PU_h),
        diffPU_l = as.numeric(adjPU_l - PU_l),
        diffPU_c = as.numeric(adjPU_c - PU_c)
      )
    }
  }

  xts::xts(
    res_mat,
    order.by = idx_dates,
    tzone = tz_val
  )
}

.brf_di_add_pu_columns <- function(data,
                                   open_col = "open",
                                   high_col = "high",
                                   low_col = "low",
                                   close_col = "close",
                                   date_col = "date",
                                   maturity_col = "maturity",
                                   round_pu = TRUE) {
  if (!is.data.frame(data) || !nrow(data)) {
    return(data)
  }
  required <- c(open_col, high_col, low_col, close_col, date_col, maturity_col)
  if (!all(required %in% names(data))) {
    return(data)
  }
  if (!.brf_di_has_bizdays()) {
    warning("Skipping DI PU columns because package 'bizdays' is not installed.", call. = FALSE)
    return(data)
  }
  basis <- as.Date(data[[date_col]])
  maturity <- as.Date(data[[maturity_col]])
  cal <- .brf_di_resolve_calendar(NULL)
  valid_days <- suppressWarnings(
    bizdays::bizdays(basis, maturity, cal) + as.integer(bizdays::is.bizday(basis, cal))
  )
  valid_days[valid_days <= 0] <- NA_real_
  months_bucket <- .brf_di_months_between_floor(basis, maturity)
  tick_size <- mapply(
    function(mm, bd) .brf_di_get_tick_size(mm, bd),
    months_bucket,
    basis,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  to_numeric <- function(col) as.numeric(data[[col]])
  rate_open <- to_numeric(open_col)
  rate_high <- to_numeric(high_col)
  rate_low <- to_numeric(low_col)
  rate_close <- to_numeric(close_col)
  rate_open[!is.finite(rate_open) | rate_open == 0] <- NA_real_
  rate_high[!is.finite(rate_high) | rate_high == 0] <- NA_real_
  rate_low[!is.finite(rate_low) | rate_low == 0] <- NA_real_
  rate_close[!is.finite(rate_close) | rate_close == 0] <- NA_real_
  data$PU_open <- .brf_di_pu_from_rate(rate_open, valid_days, round_pu)
  data$PU_high <- .brf_di_pu_from_rate(rate_low, valid_days, round_pu)
  data$PU_low <- .brf_di_pu_from_rate(rate_high, valid_days, round_pu)
  data$PU_close <- .brf_di_pu_from_rate(rate_close, valid_days, round_pu)
  data$TickSize <- as.numeric(tick_size)
  data$TickValue <- .brf_di_tick_value_from_close(
    rate_close,
    valid_days,
    tick_size
  )
  data
}

.brf_di_add_pu_xts <- function(x,
                               ticker,
                               maturity_date = NULL,
                               include_basis_day = TRUE) {
  if (!xts::is.xts(x) || !NROW(x)) {
    return(x)
  }

  if (!.brf_di_has_bizdays()) {
    warning("Skipping DI PU columns because package 'bizdays' is not installed.", call. = FALSE)
    return(x)
  }

  n <- NROW(x)

  basis <- zoo::index(x)
  tz_val <- attr(x, "tzone")
  if (!inherits(basis, "POSIXt")) {
    basis <- as.POSIXct(basis, tz = if (!is.null(tz_val)) tz_val else "UTC")
  }
  pu_cols <- c("PU_open", "PU_high", "PU_low", "PU_close")

  maturity_vec <- maturity_date
  if (is.null(maturity_vec)) {
    maturity_vec <- attr(x, "maturity", exact = TRUE)
  }
  if (is.list(maturity_vec) && length(maturity_vec)) {
    maturity_vec <- maturity_vec[[1]]
  }
  maturity_vec <- suppressWarnings(as.Date(maturity_vec))

  if (is.null(maturity_vec) || all(is.na(maturity_vec))) {
    maturity_scalar <- try(di_maturity_from_ticker(ticker), silent = TRUE)
    if (inherits(maturity_scalar, "try-error") || !inherits(maturity_scalar, "Date")) {
      warning("Could not resolve maturity for ticker '", ticker, "'. Skipping PU columns.", call. = FALSE)
      return(x)
    }
    maturity_vec <- maturity_scalar
  }
  if (length(maturity_vec) == 1L) {
    maturity_vec <- rep(maturity_vec, n)
  } else if (length(maturity_vec) != n) {
    maturity_vec <- rep(maturity_vec[1], n)
  }

  cal <- .brf_di_resolve_calendar(NULL)
  valid_days <- suppressWarnings(
    bizdays::bizdays(basis, maturity_vec, cal) +
      as.integer(bizdays::is.bizday(basis, cal))
  )
  valid_days[valid_days <= 0] <- NA_real_
  months_bucket <- .brf_di_months_between_floor(basis, maturity_vec)
  tick_size <- mapply(
    function(mm, bd) .brf_di_get_tick_size(mm, bd),
    months_bucket,
    basis,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  build_pu <- function(col_name) {
    if (!(col_name %in% colnames(x))) {
      return(NULL)
    }
    rates <- as.numeric(x[, col_name])
    rates[!is.finite(rates) | rates == 0] <- NA_real_
    .brf_di_pu_from_rate(rates, valid_days, round_pu = TRUE)
  }
  build_tick_value <- function() {
    if (!("Close" %in% colnames(x))) {
      return(NULL)
    }
    rates <- as.numeric(x[, "Close"])
    rates[!is.finite(rates) | rates == 0] <- NA_real_
    .brf_di_tick_value_from_close(rates, valid_days, tick_size)
  }

  additions <- list(
    PU_open = build_pu("Open"),
    PU_high = build_pu("Low"),
    PU_low = build_pu("High"),
    PU_close = build_pu("Close"),
    TickSize = as.numeric(tick_size),
    TickValue = build_tick_value()
  )
  keep_add <- vapply(additions, function(col) !is.null(col), logical(1))
  if (!any(keep_add)) {
    return(x)
  }
  add_matrix <- do.call(cbind, additions[keep_add])
  pu_xts <- xts::xts(add_matrix, order.by = basis, tzone = tz_val)

  if (any(colnames(pu_xts) %in% colnames(x))) {
    x <- x[, setdiff(colnames(x), colnames(pu_xts)), drop = FALSE]
  }

  result <- cbind(x, pu_xts)
  unique_maturity <- unique(maturity_vec[!is.na(maturity_vec)])
  if (length(unique_maturity)) {
    attr(result, "maturity") <- unique_maturity[1]
  }
  result
}
