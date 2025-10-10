# Mapping from B3 month letters to calendar months.
.month_letter <- c(F = 1, G = 2, H = 3, J = 4, K = 5, M = 6, N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12)

# Build (or echo) the bizdays calendar used across DI helpers.
.resolve_di_calendar <- function(cal) {
  if (!is.null(cal)) return(cal)
  bizdays::create.calendar(
    name      = "Brazil/ANBIMA",
    holidays  = bizdays::holidays("Brazil/ANBIMA"),
    weekdays  = c("saturday", "sunday")
  )
}

# Tick regime for DI futures, expressed in percentage points.
.get_di_tick_size <- function(mm, basis_date, rule_change_date = as.Date("2025-08-25")) {
  basis_date <- as.Date(basis_date)
  if (basis_date < rule_change_date) {
    if (mm <= 3) 0.001 else if (mm <= 60) 0.005 else 0.010
  } else {
    if (mm <= 3) 0.001 else 0.005
  }
}

# Snap rate(s) to the closest valid tick for their tenor bucket.
.snap_rate_to_tick <- function(rates, mm, basis_date, rule_change_date = as.Date("2025-08-25")) {
  tick <- .get_di_tick_size(mm, basis_date, rule_change_date)
  round(rates / tick) * tick
}

# Vectorised wrapper used when basis dates (and month buckets) vary per element.
.snap_di_rates <- function(rates, mm, basis_date, rule_change_date = as.Date("2025-08-25")) {
  mapply(
    function(rate, month_bucket, basis) .snap_rate_to_tick(rate, month_bucket, basis, rule_change_date),
    rates,
    mm,
    as.Date(basis_date),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
}

# Floor of elapsed months between two dates (sufficient for tick bucket selection).
.months_between_floor <- function(basis_date, maturity_date) {
  interval(as.Date(basis_date), as.Date(maturity_date)) %/% months(1)
}

# Business-day count respecting B3 convention toggles.
.biz_n <- function(basis_date, maturity_date, cal, include_basis_day = TRUE) {
  n <- bizdays::bizdays(as.Date(basis_date), as.Date(maturity_date), cal)
  if (include_basis_day) {
    n <- n + as.integer(bizdays::is.bizday(as.Date(basis_date), cal))
  }
  if (n <= 0) stop("Non-positive valid-days. Check basis/maturity and calendar.")
  n
}

# Helper to derive business-day tenor and month bucket for a basis/maturity pair.
.resolve_di_tenor <- function(maturity_date,
                              basis_date,
                              cal,
                              include_basis_day = TRUE,
                              allow_coercion = FALSE) {
  basis_date <- as.Date(basis_date)

  if (inherits(maturity_date, "Date")) {
    maturity <- as.Date(maturity_date)
  } else if (is.numeric(maturity_date)) {
    n <- as.integer(maturity_date)
    if (n <= 0) stop("'maturity_date' as numeric must be positive business days.")
    return(list(
      valid_days    = n,
      months_bucket = as.numeric(n) / 21,
      maturity_date = NA
    ))
  } else if (allow_coercion) {
    maturity <- try(as.Date(maturity_date), silent = TRUE)
    if (inherits(maturity, "try-error") || is.na(maturity)) {
      stop("'maturity_date' must be Date or numeric business days (or coercible to Date).")
    }
  } else {
    stop("'maturity_date' must be Date or numeric business days.")
  }

  n <- .biz_n(basis_date, maturity, cal, include_basis_day = include_basis_day)
  mm <- .months_between_floor(basis_date, maturity)
  if (n <= 0) stop("Number of business days to maturity must be positive.")

  list(
    valid_days    = n,
    months_bucket = mm,
    maturity_date = maturity
  )
}

# Rate/PU conversions shared across helpers.
.di_pu_from_rate <- function(rates, valid_days, round_pu = TRUE) {
  rates <- as.numeric(rates)
  valid_days <- as.numeric(valid_days)
  pu <- 1e5 / (1 + rates / 100)^(valid_days / 252)
  bad <- !is.finite(rates) | rates <= -100
  pu[bad] <- NA_real_
  if (round_pu) pu <- round(pu, 2L)
  pu
}

.di_rate_from_pu <- function(pu, valid_days) {
  pu <- as.numeric(pu)
  valid_days <- as.numeric(valid_days)
  rates <- 100 * ((1e5 / pu)^(252 / valid_days) - 1)
  bad <- !is.finite(pu) | pu <= 0
  rates[bad] <- NA_real_
  rates
}

# Column resolver shared by xts helpers.
.resolve_ohlc_columns <- function(x) {
  nm <- tolower(colnames(x))
  find_first <- function(candidates) {
    idx <- match(candidates, nm, nomatch = 0L)
    idx <- idx[idx > 0L]
    if (length(idx) == 0L) return(NA_integer_)
    idx[1L]
  }
  open_idx  <- find_first(c("open", "o"))
  high_idx  <- find_first(c("high", "h"))
  low_idx   <- find_first(c("low", "l"))
  close_idx <- find_first(c("close", "c", "last", "settle", "settlement_price"))

  if (any(is.na(c(open_idx, high_idx, low_idx, close_idx)))) {
    stop("Could not resolve OHLC columns (require open/high/low/close as percent rates).")
  }

  volume_idx <- find_first(c(
    "volume", "vol", "contracts_traded", "qty", "contracts", "volume_brl", "volume_brls"
  ))

  list(
    open   = open_idx,
    high   = high_idx,
    low    = low_idx,
    close  = close_idx,
    volume = volume_idx
  )
}

.resolve_maturity_input <- function(maturity_date, x = NULL) {
  if (!is.null(maturity_date)) return(maturity_date)
  if (is.null(x)) return(maturity_date)

  attr_val <- attr(x, "maturity", exact = TRUE)
  if (is.null(attr_val)) {
    attrs <- try(xtsAttributes(x), silent = TRUE)
    if (!inherits(attrs, "try-error") && length(attrs) && "maturity" %in% names(attrs)) {
      attr_val <- attrs[["maturity"]]
    }
  }
  if (is.null(attr_val)) return(maturity_date)

  if (length(attr_val) > 1) attr_val <- attr_val[[1]]
  if (is.list(attr_val)) attr_val <- attr_val[[1]]

  if (inherits(attr_val, "Date")) return(attr_val)
  if (inherits(attr_val, "POSIXt")) return(as.Date(attr_val))
  if (is.character(attr_val)) {
    parsed <- as.Date(attr_val)
    if (any(is.na(parsed)) && grepl("^\\d{8}$", attr_val)) {
      parsed <- as.Date(attr_val, format = "%Y%m%d")
    }
    if (any(is.na(parsed))) stop("Could not coerce 'maturity' attribute to Date.")
    return(parsed)
  }

  attr_val
}

#' Derive DI maturity date from a B3 ticker
#'
#' The maturity is the first business day of the contract month, adjusted with the
#' supplied `bizdays` calendar.
#'
#' @param ticker Character scalar such as `"DI1F25"`.
#' @param cal `bizdays` calendar. If `NULL`, the standard `"Brazil/ANBIMA"` calendar is used.
#'
#' @return A `Date` with the contract maturity.
#' @examples
#' \dontrun{
#' di_maturity_from_ticker("DI1F25")
#' }
#' @export
di_maturity_from_ticker <- function(ticker, cal = NULL) {
  stopifnot(is.character(ticker), length(ticker) == 1)
  cal <- .resolve_di_calendar(cal)

  month_code <- substr(ticker, 4, 4)
  mm <- .month_letter[month_code]
  if (is.na(mm)) stop("Cannot parse month from ticker: ", ticker)

  y2 <- as.integer(substr(ticker, 5, 6))
  if (is.na(y2)) stop("Cannot parse year from ticker: ", ticker)
  y4 <- ifelse(y2 >= 90, 1900 + y2, 2000 + y2)

  first_day <- as.Date(sprintf("%04d-%02d-01", y4, mm))
  bizdays::adjust.next(first_day, cal)
}

#' DI futures notional (PU) from annualized rates
#'
#' Compute the DI futures notional price (PU) and tick value given an annualized DI
#' rate (percentage). Supports explicit business-day conventions, optional rate
#' snapping to the DI tick grid, and rounding to cents.
#'
#' @param rates Annualized DI rate(s) in percent.
#' @param maturity_date Maturity `Date` or number of business days to expiry.
#' @param basis_date Trade/reference date.
#' @param cal `bizdays` calendar. Defaults to `"Brazil/ANBIMA"` when `NULL`.
#' @param include_basis_day Whether to add the basis day to the business-day count.
#' @param snap_to_tick Snap `rates` to the DI tick grid before pricing.
#' @param round_pu Round the PU to two decimals (B3 convention).
#' @param rule_change_date Date where the DI tick regime changes.
#'
#' @return A list with elements `valid_days`, `pu`, `tick_size`, and `tick_value`.
#' @export
calculate_futures_di_notional <- function(
    rates,
    maturity_date,
    basis_date = Sys.Date(),
    cal = NULL,
    include_basis_day = TRUE,
    snap_to_tick = TRUE,
    round_pu = TRUE,
    rule_change_date = as.Date("2025-08-25")
) {
  cal <- .resolve_di_calendar(cal)
  basis_date <- as.Date(basis_date)

  tenor <- .resolve_di_tenor(
    maturity_date = maturity_date,
    basis_date = basis_date,
    cal = cal,
    include_basis_day = include_basis_day
  )
  n <- tenor$valid_days
  mm <- tenor$months_bucket

  rates <- as.numeric(rates)
  if (any(!is.finite(rates) | rates <= -100)) stop("'rates' must be finite and greater than -100%.")
  if (snap_to_tick) {
    rates <- .snap_di_rates(rates, mm, basis_date, rule_change_date)
  }

  pu <- .di_pu_from_rate(rates, n, round_pu = round_pu)

  tick_size  <- .get_di_tick_size(mm, basis_date, rule_change_date)
  deriv_pp   <- -(n / 252) * pu / (100 * (1 + rates / 100))
  tick_value <- abs(deriv_pp) * tick_size

  list(
    valid_days = as.integer(n),
    pu         = as.numeric(pu),
    tick_size  = tick_size,
    tick_value = as.numeric(tick_value)
  )
}

#' DI futures rates from notional (PU)
#'
#' Inverse of `calculate_futures_di_notional()`. Converts a DI futures PU back into
#' an annualized rate, optionally snapping the output to the DI tick grid.
#'
#' @inheritParams calculate_futures_di_notional
#' @param pu Futures price (PU) in monetary units.
#'
#' @return A list with elements `valid_days`, `rates`, `tick_size`, and `tick_value`.
#' @export
calculate_futures_di_rates <- function(
    pu,
    maturity_date,
    basis_date = Sys.Date(),
    cal = NULL,
    include_basis_day = TRUE,
    snap_to_tick = TRUE,
    rule_change_date = as.Date("2025-08-25")
) {
  cal <- .resolve_di_calendar(cal)
  basis_date <- as.Date(basis_date)

  tenor <- .resolve_di_tenor(
    maturity_date = maturity_date,
    basis_date = basis_date,
    cal = cal,
    include_basis_day = include_basis_day,
    allow_coercion = TRUE
  )
  n <- tenor$valid_days
  mm <- tenor$months_bucket

  pu <- as.numeric(pu)
  if (any(!is.finite(pu) | pu <= 0)) stop("'pu' must be positive and finite.")

  rates <- .di_rate_from_pu(pu, n)
  if (snap_to_tick) {
    rates <- .snap_di_rates(rates, mm, basis_date, rule_change_date)
  }

  tick_size  <- .get_di_tick_size(mm, basis_date, rule_change_date)
  deriv_pp   <- -(n / 252) * pu / (100 * (1 + rates / 100))
  tick_value <- abs(deriv_pp) * tick_size

  list(
    valid_days = as.integer(n),
    rates      = as.numeric(rates),
    tick_size  = tick_size,
    tick_value = as.numeric(tick_value)
  )
}

#' Estimate DI settlement PU from daily OHLC data
#'
#' Derive a settlement-like PU from daily DI futures rates. Selection priority for
#' the anchor rate can be customised, a bias adjustment (in percentage points) can
#' be applied, and the result optionally snaps to the DI tick grid.
#'
#' @param open,high,low,close Daily OHLC rates (percent per year).
#' @param average_price Optional VWAP (percent per year).
#' @inheritParams calculate_futures_di_notional
#' @param basis_date Date of the OHLC bar.
#' @param prefer Anchor preference order (`"average_price"`, `"close"`, `"mid"`, `"open"`).
#' @param bias_pp Fixed adjustment in percentage points added to the anchor rate.
#'
#' @return A list with elements `rate_anchor`, `rate_adj`, `pu_est`, `valid_days`, and `tick_size`.
#' @export
estimate_pu_from_daily_ohlc <- function(
    open, high, low, close,
    average_price = NULL,
    maturity_date,
    basis_date,
    cal = NULL,
    include_basis_day = TRUE,
    prefer = c("average_price", "close", "mid", "open"),
    bias_pp = 0,
    snap_to_tick = TRUE,
    rule_change_date = as.Date("2025-08-25")
) {
  cal <- .resolve_di_calendar(cal)
  basis_date <- as.Date(basis_date)
  prefer <- match.arg(prefer)

  tenor <- .resolve_di_tenor(
    maturity_date = maturity_date,
    basis_date = basis_date,
    cal = cal,
    include_basis_day = include_basis_day
  )
  n  <- tenor$valid_days
  mm <- tenor$months_bucket

  tick_size <- .get_di_tick_size(mm, basis_date, rule_change_date)

  pick_first <- function(...) {
    xs <- list(...)
    xs <- xs[!vapply(xs, function(z) is.null(z) || !is.finite(z), logical(1))]
    if (length(xs) == 0) stop("No valid OHLC values.")
    xs[[1]]
  }

  mid <- if (is.finite(high) && is.finite(low)) (high + low) / 2 else NA_real_

  rate_anchor <- switch(
    prefer,
    average_price = pick_first(average_price, close, mid, open),
    close         = pick_first(close, average_price, mid, open),
    mid           = pick_first(mid, average_price, close, open),
    open          = pick_first(open, average_price, close, mid)
  )

  rate_adj <- as.numeric(rate_anchor) + as.numeric(bias_pp)
  if (snap_to_tick) {
    rate_adj <- .snap_di_rates(rate_adj, mm, basis_date, rule_change_date)
  }

  pu <- .di_pu_from_rate(rate_adj, n, round_pu = TRUE)

  list(
    rate_anchor = as.numeric(rate_anchor),
    rate_adj    = as.numeric(rate_adj),
    pu_est      = as.numeric(pu),
    valid_days  = as.integer(n),
    tick_size   = tick_size
  )
}

#' Exponentially-weighted bias in percentage points
#'
#' @param settle_rate_hist Historical settle rates (percent).
#' @param anchor_rate_hist Historical anchor rates (percent), same length as `settle_rate_hist`.
#' @param lambda Smoothing factor in (0, 1].
#'
#' @return The exponentially-weighted bias (settle minus anchor) in percentage points.
#' @export
learn_bias_pp_ema <- function(settle_rate_hist, anchor_rate_hist, lambda = 0.2) {
  stopifnot(length(settle_rate_hist) == length(anchor_rate_hist))
  if (!(lambda > 0 && lambda <= 1)) stop("'lambda' must be in (0, 1].")

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
#' Adds PU columns (`PU_o`, `PU_h`, `PU_l`, `PU_c`) to a daily xts of DI rates. The
#' conversion follows the B3 convention `PU = 100000 / (1 + r/100)^(n/252)` with an
#' explicit business-day count and optional snap to the DI tick grid.
#'
#' @param x xts object with daily OHLC rates (percent per year).
#' @param maturity_date Maturity `Date` or business days to expiry. If `NULL`, the
#'   function looks for a `"maturity"` attribute on `x` (either a `Date` or
#'   character parsable via `as.Date`).
#' @inheritParams calculate_futures_di_notional
#' @param round_pu Round the output PUs to two decimals.
#'
#' @return An xts object containing the PU columns.
#' @export
ohlc_rates_to_pu_xts <- function(
    x,
    maturity_date = NULL,
    cal = NULL,
    include_basis_day = TRUE,
    snap_to_tick = FALSE,
    rule_change_date = as.Date("2025-08-25"),
    round_pu = TRUE
) {
  if (!is.xts(x)) stop("'x' must be an xts object.")
  if (NROW(x) == 0) return(x)

  cal <- .resolve_di_calendar(cal)
  maturity_date <- .resolve_maturity_input(maturity_date, x)
  if (is.null(maturity_date)) {
    stop("Provide 'maturity_date' or set a 'maturity' attribute on 'x'.")
  }

  cols <- .resolve_ohlc_columns(x)
  r_open  <- as.numeric(x[, cols$open])
  r_high  <- as.numeric(x[, cols$high])
  r_low   <- as.numeric(x[, cols$low])
  r_close <- as.numeric(x[, cols$close])

  idx_dates <- index(x)

  if (inherits(maturity_date, "Date")) {
    md <- as.Date(maturity_date)
    n_vec <- vapply(
      idx_dates,
      function(d) .biz_n(d, md, cal, include_basis_day = include_basis_day),
      numeric(1)
    )
    mm_vec <- vapply(
      idx_dates,
      function(d) .months_between_floor(d, md),
      numeric(1)
    )
  } else if (is.numeric(maturity_date)) {
    n_const <- as.integer(maturity_date)
    if (n_const <= 0) stop("'maturity_date' as numeric must be positive business days.")
    n_vec <- rep.int(n_const, length(idx_dates))
    mm_vec <- rep_len(as.numeric(n_const) / 21, length(idx_dates))
  } else {
    stop("'maturity_date' must be a Date or numeric business days.")
  }

  if (snap_to_tick) {
    r_open  <- .snap_di_rates(r_open,  mm_vec, idx_dates, rule_change_date)
    r_high  <- .snap_di_rates(r_high,  mm_vec, idx_dates, rule_change_date)
    r_low   <- .snap_di_rates(r_low,   mm_vec, idx_dates, rule_change_date)
    r_close <- .snap_di_rates(r_close, mm_vec, idx_dates, rule_change_date)
  }

  PU_o <- .di_pu_from_rate(r_open,  n_vec, round_pu = round_pu)
  PU_h <- .di_pu_from_rate(r_high,  n_vec, round_pu = round_pu)
  PU_l <- .di_pu_from_rate(r_low,   n_vec, round_pu = round_pu)
  PU_c <- .di_pu_from_rate(r_close, n_vec, round_pu = round_pu)

  xts(
    cbind(
      PU_o = as.numeric(PU_o),
      PU_h = as.numeric(PU_h),
      PU_l = as.numeric(PU_l),
      PU_c = as.numeric(PU_c)
    ),
    order.by = idx_dates,
    tzone = attr(x, "tzone")
  )
}

#' Augment DI OHLC data with PU and diagnostics
#'
#' Enhances a DI futures OHLC xts with PU columns computed via the B3 convention.
#' Optionally, the function can round-trip the PUs back to rates and report
#' diagnostics on the differences.
#'
#' @param x xts with DI OHLC rates (percent per year). A volume column (if present)
#'   is preserved.
#' @param snap_rates_back Snap the round-tripped rates back to the DI tick grid.
#'   When set to `TRUE`, diagnostics columns are returned.
#' @param include_diagnostics When `TRUE`, adds the round-tripped rates/PUs and the
#'   differences versus the original inputs. This is implicitly enabled when
#'   `snap_rates_back = TRUE`.
#' @inheritParams ohlc_rates_to_pu_xts
#'
#' @return An xts object with the original OHLC columns, PU columns, and optional diagnostics.
#' @export
di_ohlc_to_pu_augmented_xts <- function(
    x,
    maturity_date = NULL,
    cal = NULL,
    include_basis_day = TRUE,
    snap_to_tick = FALSE,
    rule_change_date = as.Date("2025-08-25"),
    round_pu = FALSE,
    snap_rates_back = FALSE,
    include_diagnostics = FALSE
) {
  if (!is.xts(x)) stop("'x' must be an xts object.")
  if (NROW(x) == 0) return(x)

  cal <- .resolve_di_calendar(cal)
  if (snap_rates_back && !include_diagnostics) include_diagnostics <- TRUE
  maturity_date <- .resolve_maturity_input(maturity_date, x)
  if (is.null(maturity_date)) {
    stop("Provide 'maturity_date' or set a 'maturity' attribute on 'x'.")
  }

  cols <- .resolve_ohlc_columns(x)
  r_open  <- as.numeric(x[, cols$open])
  r_high  <- as.numeric(x[, cols$high])
  r_low   <- as.numeric(x[, cols$low])
  r_close <- as.numeric(x[, cols$close])

  idx_dates <- index(x)

  if (inherits(maturity_date, "Date")) {
    md <- as.Date(maturity_date)
    n_vec <- vapply(
      idx_dates,
      function(d) .biz_n(d, md, cal, include_basis_day = include_basis_day),
      numeric(1)
    )
    mm_vec <- vapply(
      idx_dates,
      function(d) .months_between_floor(d, md),
      numeric(1)
    )
  } else if (is.numeric(maturity_date)) {
    n_const <- as.integer(maturity_date)
    if (n_const <= 0) stop("'maturity_date' as numeric must be positive business days.")
    n_vec <- rep.int(n_const, length(idx_dates))
    mm_vec <- rep_len(as.numeric(n_const) / 21, length(idx_dates))
  } else {
    stop("'maturity_date' must be a Date or numeric business days.")
  }

  if (snap_to_tick) {
    r_open  <- .snap_di_rates(r_open,  mm_vec, idx_dates, rule_change_date)
    r_high  <- .snap_di_rates(r_high,  mm_vec, idx_dates, rule_change_date)
    r_low   <- .snap_di_rates(r_low,   mm_vec, idx_dates, rule_change_date)
    r_close <- .snap_di_rates(r_close, mm_vec, idx_dates, rule_change_date)
  }

  PU_o <- .di_pu_from_rate(r_open,  n_vec, round_pu = round_pu)
  PU_h <- .di_pu_from_rate(r_high,  n_vec, round_pu = round_pu)
  PU_l <- .di_pu_from_rate(r_low,   n_vec, round_pu = round_pu)
  PU_c <- .di_pu_from_rate(r_close, n_vec, round_pu = round_pu)

  base_cols <- cbind(
    open  = r_open,
    high  = r_high,
    low   = r_low,
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
    adjOpen  <- .di_rate_from_pu(PU_o, n_vec)
    adjHigh  <- .di_rate_from_pu(PU_h, n_vec)
    adjLow   <- .di_rate_from_pu(PU_l, n_vec)
    adjClose <- .di_rate_from_pu(PU_c, n_vec)

    if (snap_rates_back) {
      adjOpen  <- .snap_di_rates(adjOpen,  mm_vec, idx_dates, rule_change_date)
      adjHigh  <- .snap_di_rates(adjHigh,  mm_vec, idx_dates, rule_change_date)
      adjLow   <- .snap_di_rates(adjLow,   mm_vec, idx_dates, rule_change_date)
      adjClose <- .snap_di_rates(adjClose, mm_vec, idx_dates, rule_change_date)
    }

    adjPU_o <- .di_pu_from_rate(adjOpen,  n_vec, round_pu = round_pu)
    adjPU_h <- .di_pu_from_rate(adjHigh,  n_vec, round_pu = round_pu)
    adjPU_l <- .di_pu_from_rate(adjLow,   n_vec, round_pu = round_pu)
    adjPU_c <- .di_pu_from_rate(adjClose, n_vec, round_pu = round_pu)

    if (include_diagnostics) {
      res_mat <- cbind(
        res_mat,
        adjOpen  = as.numeric(adjOpen),
        adjHigh  = as.numeric(adjHigh),
        adjLow   = as.numeric(adjLow),
        adjClose = as.numeric(adjClose),
        adjPU_o  = as.numeric(adjPU_o),
        adjPU_h  = as.numeric(adjPU_h),
        adjPU_l  = as.numeric(adjPU_l),
        adjPU_c  = as.numeric(adjPU_c),
        diffOpen_pp  = as.numeric(adjOpen  - r_open),
        diffHigh_pp  = as.numeric(adjHigh  - r_high),
        diffLow_pp   = as.numeric(adjLow   - r_low),
        diffClose_pp = as.numeric(adjClose - r_close),
        diffPU_o = as.numeric(adjPU_o - PU_o),
        diffPU_h = as.numeric(adjPU_h - PU_h),
        diffPU_l = as.numeric(adjPU_l - PU_l),
        diffPU_c = as.numeric(adjPU_c - PU_c)
      )
    }
  }

  xts(
    res_mat,
    order.by = idx_dates,
    tzone = attr(x, "tzone")
  )
}
