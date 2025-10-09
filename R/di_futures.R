
# Map month letter (B3) to number
.month_letter <- c(F=1,G=2,H=3,J=4,K=5,M=6,N=7,Q=8,U=9,V=10,X=11,Z=12)

# Derive maturity date for DI from a contract code like "DI1F11"
# Uses: first business day of the maturity month.
di_maturity_from_ticker <- function(ticker, cal) {
  stopifnot(is.character(ticker), length(ticker) == 1)
  m <- .month_letter[substr(ticker, 4, 4)]
  if (is.na(m)) stop("Cannot parse month from ticker: ", ticker)
  y2 <- as.integer(substr(ticker, 5, 6))
  y4 <- ifelse(y2 >= 90, 1900 + y2, 2000 + y2)
  first_day <- as.Date(sprintf("%04d-%02d-01", y4, m))
  bizdays::adjust.next(first_day, cal)
}

# Tick regime for DI rates (in percentage points)
# Note: keep rule_change_date configurable (confirm with B3 "Parâmetros" doc if needed).
.get_di_tick_size <- function(mm, basis_date, rule_change_date = as.Date("2025-08-25")) {
  basis_date <- as.Date(basis_date)
  if (basis_date < rule_change_date) {
    # Legacy: 0???3m: 0.001 ; 3???60m: 0.005 ; >60m: 0.010
    if (mm <= 3) 0.001 else if (mm <= 60) 0.005 else 0.010
  } else {
    # New: 0???3m: 0.001 ; >3m: 0.005
    if (mm <= 3) 0.001 else 0.005
  }
}

# Snap a rate (in %) to the nearest valid tick for its tenor bucket
.snap_rate_to_tick <- function(rates, mm, basis_date, rule_change_date = as.Date("2025-08-25")) {
  tick <- .get_di_tick_size(mm, basis_date, rule_change_date)
  # Round to nearest multiple of tick (in percentage points)
  round(rates / tick) * tick
}

# Robust months-to-maturity used only to select tick bucket
.months_between_floor <- function(basis_date, maturity_date) {
  # Floor of whole months between basis and maturity.
  # Enough for tick-bucket selection; exact day-count comes separately.
  lubridate::interval(as.Date(basis_date), as.Date(maturity_date)) %/% months(1)
}

# Business-day count with explicit convention toggle.
# Bizdays::bizdays(a,b) excludes 'a' and includes 'b'. B3 convention can vary by spec.
# Use include_basis_day to add +1 when you need [including basis, excluding maturity] effect.
.biz_n <- function(basis_date, maturity_date, cal, include_basis_day = TRUE) {
  n <- bizdays::bizdays(as.Date(basis_date), as.Date(maturity_date), cal)
  if (include_basis_day) {
    # If the basis is a business day, add 1 to emulate "include basis"
    n <- n + as.integer(bizdays::is.bizday(as.Date(basis_date), cal))
  }
  if (n <= 0) stop("Non-positive valid-days. Check basis/maturity and calendar.")
  n
}

#' DI futures notional and tick value from rates
#'
#' Compute DI futures notional price (PU) and tick value given an annualized DI rate (%)
#' and time to expiry. Adds: explicit business-day convention, tick snapping, and rounding.
#'
#' @param rates Annualized DI rate in percent (vector OK).
#' @param maturity_date Date or number of business days to expiry.
#' @param basis_date Reference Date (trade date).
#' @param cal bizdays calendar; default "Brazil/ANBIMA".
#' @param include_basis_day Logical, whether to include basis day in day count.
#' @param snap_to_tick Logical, snap input rates to DI tick grid before pricing.
#' @param round_pu Logical, round PU to 0.01 (B3 prints cents).
#' @param rule_change_date Date, tick regime change cutoff.
#' @return list(valid_days, pu, tick_size, tick_value).
#' @export
.calculate_futures_di_notional <- function(
    rates,
    maturity_date,
    basis_date = Sys.Date(),
    cal = NULL,
    include_basis_day = TRUE,
    snap_to_tick = TRUE,
    round_pu = TRUE,
    rule_change_date = as.Date("2025-08-25")
) {
  if (is.null(cal)) {
    cal <- bizdays::create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = bizdays::holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  basis_date <- as.Date(basis_date)

  # Resolve n (valid business days) and mm (months for tick bucket)
  if (inherits(maturity_date, "Date")) {
    md <- as.Date(maturity_date)
    n  <- .biz_n(basis_date, md, cal, include_basis_day = include_basis_day)
    mm <- .months_between_floor(basis_date, md)
  } else if (is.numeric(maturity_date)) {
    md <- NA
    n  <- as.integer(maturity_date)
    mm <- n / 21   # coarse proxy only for tick-bucket if only 'n' is given
  } else {
    stop("'maturity_date' must be Date or numeric business days.")
  }
  if (n <= 0) stop("Number of business days to maturity must be positive.")

  # Rate sanity + optional snap to tick grid
  rates <- as.numeric(rates)
  if (any(!is.finite(rates) | rates <= -100)) stop("'rates' must be finite and > -100%.")
  if (snap_to_tick) rates <- .snap_rate_to_tick(rates, mm, basis_date, rule_change_date)

  # PU from rate
  pu <- 1e5 / (1 + rates/100)^(n/252)

  # Tick-size and tick-value (magnitude of dPU per one rate-tick)
  tick_size  <- .get_di_tick_size(mm, basis_date, rule_change_date)
  deriv_pp   <- -(n/252) * pu / (100 * (1 + rates/100))  # dPU/d(rate_pp)
  tick_value <- abs(deriv_pp) * tick_size

  if (round_pu) pu <- round(pu, 2L)

  list(
    valid_days = n,
    pu         = as.numeric(pu),
    tick_size  = tick_size,
    tick_value = as.numeric(tick_value)
  )
}

#' DI futures rate and tick value from PU
#'
#' Inverse of the notional function. Adds: explicit bizday convention, optional
#' snap of the OUTPUT rate to the tick grid, and PU rounding pre-check.
#'
#' @param pu DI futures price (PU).
#' @param maturity_date Date or number of business days to expiry.
#' @param basis_date Reference Date (trade date).
#' @param cal bizdays calendar; default "Brazil/ANBIMA".
#' @param include_basis_day Logical, include basis in day count (see .biz_n).
#' @param snap_to_tick Logical, snap resulting rate to DI tick grid.
#' @param rule_change_date Date, tick regime change cutoff.
#' @return list(valid_days, rates, tick_size, tick_value).
#' @export
.calculate_futures_di_rates <- function(
    pu,
    maturity_date,
    basis_date = Sys.Date(),
    cal = NULL,
    include_basis_day = TRUE,
    snap_to_tick = TRUE,
    rule_change_date = as.Date("2025-08-25")
) {
  if (is.null(cal)) {
    cal <- bizdays::create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = bizdays::holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  basis_date <- as.Date(basis_date)

  # Resolve n and mm
  if (inherits(maturity_date, "Date")) {
    md <- as.Date(maturity_date)
    n  <- .biz_n(basis_date, md, cal, include_basis_day = include_basis_day)
    mm <- .months_between_floor(basis_date, md)
  } else if (is.numeric(maturity_date)) {
    md <- NA
    n  <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    # Allow coercion to Date from character
    md <- try(as.Date(maturity_date), silent = TRUE)
    if (inherits(md, "try-error") || is.na(md)) {
      stop("'maturity_date' must be Date or numeric business days (or coercible to Date).")
    }
    n  <- .biz_n(basis_date, md, cal, include_basis_day = include_basis_day)
    mm <- .months_between_floor(basis_date, md)
  }
  if (n <= 0) stop("Number of business days to maturity must be positive.")

  # PU sanity
  pu <- as.numeric(pu)
  if (any(!is.finite(pu) | pu <= 0)) stop("'pu' must be positive and finite.")

  # Rate from PU
  rates <- 100 * ((1e5 / pu)^(252 / n) - 1)  # percentage

  # Optional snap of OUTPUT rate to tick grid
  if (snap_to_tick) rates <- .snap_rate_to_tick(rates, mm, basis_date, rule_change_date)

  # Tick-size and tick-value for information
  tick_size  <- .get_di_tick_size(mm, basis_date, rule_change_date)
  deriv_pp   <- -(n/252) * pu / (100 * (1 + rates/100))
  tick_value <- abs(deriv_pp) * tick_size

  list(
    valid_days = n,
    rates      = as.numeric(rates),
    tick_size  = tick_size,
    tick_value = as.numeric(tick_value)
  )
}

#' Estimate DI "settlement-like" PU from daily bars (no intraday)
#'
#' Strategy:
#'  1) Choose an anchor daily rate (prefer day VWAP if available, else close, else mid(H/L), else open).
#'  2) Snap the anchor to the valid DI tick grid for its tenor bucket.
#'  3) Optional bias correction in percentage points, learned from history (EMA of settle - anchor).
#'  4) Convert the adjusted rate to PU using correct business-day count.
#'
#' This will typically beat a naive 'close->PU' approach and stay stable for sizing.
#' @param open,high,low,close Numeric rates in percent (daily OHLC).
#' @param average_price Optional numeric (%), day VWAP if you have it.
#' @param maturity_date Date of maturity (preferred) or business days.
#' @param basis_date The daily bar date used for day-count.
#' @param cal bizdays calendar.
#' @param include_basis_day Include basis in day-count. See .biz_n.
#' @param prefer One of c("average_price","close","mid","open") for anchor priority.
#' @param bias_pp Numeric, fixed bias in percentage points to add to the anchor (default 0).
#' @param snap_to_tick Snap anchor (+bias) to tick grid before PU (default TRUE).
#' @param rule_change_date Tick regime cutoff.
#' @return list(rate_anchor, rate_adj, pu_est, valid_days, tick_size)
#' @export
estimate_pu_from_daily_ohlc <- function(
    open, high, low, close,
    average_price = NULL,
    maturity_date,
    basis_date,
    cal = NULL,
    include_basis_day = TRUE,
    prefer = c("average_price","close","mid","open"),
    bias_pp = 0,
    snap_to_tick = TRUE,
    rule_change_date = as.Date("2025-08-25")
) {
  if (is.null(cal)) {
    cal <- bizdays::create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = bizdays::holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  basis_date <- as.Date(basis_date)
  prefer <- match.arg(prefer)

  # Resolve maturity, n, mm
  if (inherits(maturity_date, "Date")) {
    md <- as.Date(maturity_date)
    n  <- .biz_n(basis_date, md, cal, include_basis_day = include_basis_day)
    mm <- .months_between_floor(basis_date, md)
  } else if (is.numeric(maturity_date)) {
    md <- NA
    n  <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    stop("'maturity_date' must be Date or numeric business days.")
  }
  if (n <= 0) stop("Number of business days to maturity must be positive.")
  tick_size <- .get_di_tick_size(mm, basis_date, rule_change_date)

  # Choose anchor
  pick <- function(...) {
    xs <- list(...)
    xs <- xs[!vapply(xs, function(z) is.null(z) || !is.finite(z), logical(1))]
    if (length(xs) == 0) stop("No valid OHLC values.")
    xs[[1]]
  }
  mid <- if (is.finite(high) && is.finite(low)) (high + low)/2 else NA_real_

  rate_anchor <- switch(
    prefer,
    average_price = pick(average_price, close, mid, open),
    close         = pick(close, average_price, mid, open),
    mid           = pick(mid, average_price, close, open),
    open          = pick(open, average_price, close, mid)
  )

  # Bias adjustment (percentage points, not basis points)
  rate_adj <- as.numeric(rate_anchor) + as.numeric(bias_pp)

  # Optional snap to tick grid
  if (snap_to_tick) rate_adj <- .snap_rate_to_tick(rate_adj, mm, basis_date, rule_change_date)

  # Convert to PU
  pu <- 1e5 / (1 + rate_adj/100)^(n/252)
  pu <- round(pu, 2L)

  list(
    rate_anchor = as.numeric(rate_anchor),
    rate_adj    = as.numeric(rate_adj),
    pu_est      = as.numeric(pu),
    valid_days  = n,
    tick_size   = tick_size
  )
}

# Learn a small bias (in percentage points) as EMA of (settle_rate - anchor_rate).
# Use yesterday's info only when projecting today's PU without intraday.
learn_bias_pp_ema <- function(settle_rate_hist, anchor_rate_hist, lambda = 0.2) {
  stopifnot(length(settle_rate_hist) == length(anchor_rate_hist))
  diffs <- settle_rate_hist - anchor_rate_hist
  # Simple recursive EMA
  ema <- 0
  for (d in diffs) ema <- lambda * d + (1 - lambda) * ema
  as.numeric(ema)
}

# Convert DI daily OHLC rates (%) to PU columns (PU_o, PU_h, PU_l, PU_c)
# - Assumes input xts has daily bars with columns named (case-insensitive):
#   "open", "high", "low", "close" (rates in percent per year).
# - Uses B3 convention: PU = 100000 / (1 + r/100)^(n/252)
# - 'n' = business days from basis date (row index) to maturity (see include_basis_day).
# - By default, does NOT snap rates to the DI tick grid (set snap_to_tick=TRUE to enable).
# - Returns a new xts with four columns: PU_o, PU_h, PU_l, PU_c.
# Convert DI daily OHLC rates (%) to PU columns (PU_o, PU_h, PU_l, PU_c)
# - Input: xts with columns open/high/low/close (rates in % p.a.)
# - PU = 100000 / (1 + r/100)^(n/252)
# - 'n' = business days from row date (basis) to maturity
ohlc_rates_to_pu_xts <- function(
    x,
    maturity_date,
    cal = NULL,
    include_basis_day = TRUE,
    snap_to_tick = FALSE,
    rule_change_date = as.Date("2025-08-25"),
    round_pu = TRUE
) {
  if (!xts::is.xts(x)) stop("'x' must be an xts object.")
  if (NROW(x) == 0) return(x)

  # Calendar
  if (is.null(cal)) {
    cal <- bizdays::create.calendar(
      name = "Brazil/ANBIMA",
      holidays = bizdays::holidays("Brazil/ANBIMA"),
      weekdays = c("saturday","sunday")
    )
  }

  # Column resolution (case-insensitive)
  nm <- tolower(colnames(x))
  find_col <- function(targets) {
    idx <- match(targets, nm, nomatch = 0L)
    idx <- idx[idx > 0L]
    if (length(idx) == 0L) return(NA_integer_)
    idx[1L]
  }
  i_open  <- find_col(c("open","o"))
  i_high  <- find_col(c("high","h"))
  i_low   <- find_col(c("low","l"))
  i_close <- find_col(c("close","c","last","settle","settlement_price"))
  if (any(is.na(c(i_open,i_high,i_low,i_close))))
    stop("Could not resolve OHLC columns (need open/high/low/close as rate %).")

  r_open  <- as.numeric(x[, i_open])
  r_high  <- as.numeric(x[, i_high])
  r_low   <- as.numeric(x[, i_low])
  r_close <- as.numeric(x[, i_close])

  # ---- Helpers ----
  # Integer months between (floor) for tick-bucket selection
  .months_between_floor <- function(basis_date, maturity_date) {
    as.integer(
      lubridate::interval(as.Date(basis_date), as.Date(maturity_date)) %/% months(1)
    )
  }
  .get_di_tick_size <- function(mm, basis_date, rule_change_date) {
    bd <- as.Date(basis_date)
    if (bd < rule_change_date) {
      if (mm <= 3) 0.001 else if (mm <= 60) 0.005 else 0.010
    } else {
      if (mm <= 3) 0.001 else 0.005
    }
  }
  .snap_one <- function(rate, mm, basis_date) {
    tick <- .get_di_tick_size(mm, basis_date, rule_change_date)
    round(rate / tick) * tick
  }
  .biz_n <- function(basis_date, maturity_date, cal, include_basis_day = TRUE) {
    n <- bizdays::bizdays(as.Date(basis_date), as.Date(maturity_date), cal)
    if (include_basis_day) n <- n + as.integer(bizdays::is.bizday(as.Date(basis_date), cal))
    if (n <= 0) stop("Non-positive valid-days. Check basis/maturity and calendar.")
    n
  }

  # Day index
  idx_dates <- zoo::index(x)

  # Resolve n (valid business days) and mm (integer months) per row
  if (inherits(maturity_date, "Date")) {
    md <- as.Date(maturity_date)
    n_vec <- vapply(
      idx_dates,
      function(d) .biz_n(d, md, cal, include_basis_day = include_basis_day),
      numeric(1)  # numeric is safer; we don't care about integer storage
    )
    mm_vec <- vapply(
      idx_dates,
      function(d) .months_between_floor(d, md),
      integer(1)  # now truly integer due to as.integer() in helper
    )
  } else if (is.numeric(maturity_date)) {
    n_const <- as.integer(maturity_date)
    if (n_const <= 0) stop("'maturity_date' as numeric must be positive business days.")
    n_vec <- rep.int(n_const, length(idx_dates))
    mm_vec <- as.numeric(n_vec) / 21  # coarse proxy (only for tick bucket)
  } else {
    stop("'maturity_date' must be a Date or numeric business days.")
  }

  # Optional tick snapping
  if (snap_to_tick) {
    r_open  <- mapply(.snap_one,  r_open,  mm_vec, idx_dates)
    r_high  <- mapply(.snap_one,  r_high,  mm_vec, idx_dates)
    r_low   <- mapply(.snap_one,  r_low,   mm_vec, idx_dates)
    r_close <- mapply(.snap_one,  r_close, mm_vec, idx_dates)
  }

  # Vectorized PU conversion
  pu_from_rate <- function(r, n) {
    bad <- !is.finite(r) | r <= -100
    out <- 1e5 / (1 + r/100)^(n/252)
    out[bad] <- NA_real_
    if (round_pu) out <- round(out, 2L)
    out
  }
  PU_o <- pu_from_rate(r_open,  n_vec)
  PU_h <- pu_from_rate(r_high,  n_vec)
  PU_l <- pu_from_rate(r_low,   n_vec)
  PU_c <- pu_from_rate(r_close, n_vec)

  res <- xts::xts(
    cbind(PU_o = as.numeric(PU_o),
          PU_h = as.numeric(PU_h),
          PU_l = as.numeric(PU_l),
          PU_c = as.numeric(PU_c)),
    order.by = idx_dates, tzone = attr(x, "tzone")
  )
  colnames(res) <- c("PU_o","PU_h","PU_l","PU_c")
  res
}

# Augment a daily DI OHLC xts (% p.a.) with PU columns and round-trip diagnostics
# Input:
#   x : xts with daily OHLC rate columns in percent per year (case-insensitive: open/high/low/close)
#       Optionally includes a "volume" column (or similar); if present, it is kept.
#   maturity_date : Date (preferred) or integer business days to maturity (constant for all rows)
#   cal : bizdays calendar (default Brazil/ANBIMA)
#   include_basis_day : include the basis day in valid-days count (see .biz_n)
#   snap_to_tick : snap input OHLC rates to DI tick grid before PU conversion
#   rule_change_date : tick regime change cutoff (kept configurable)
#   round_pu : round PUs to 2 decimals (B3 convention prints cents)
#   snap_rates_back : snap the round-tripped (backed-out) rates to the tick grid
#
# Output: xts with columns:
#   [open, high, low, close, (volume?)]
#   PU_o, PU_h, PU_l, PU_c
#   adjOpen, adjHigh, adjLow, adjClose
#   adjPU_o, adjPU_h, adjPU_l, adjPU_c
#   diffOpen_pp, diffHigh_pp, diffLow_pp, diffClose_pp  (adj - original, in percentage points)
#   diffPU_o, diffPU_h, diffPU_l, diffPU_c             (adjPU - PU, in PU units)
di_ohlc_to_pu_augmented_xts <- function(
    x,
    maturity_date,
    cal = NULL,
    include_basis_day = TRUE,
    snap_to_tick = FALSE,
    rule_change_date = as.Date("2025-08-25"),
    round_pu = FALSE,
    snap_rates_back = FALSE
) {
  # --- Basic checks ---
  if (!xts::is.xts(x)) stop("'x' must be an xts object.")
  if (NROW(x) == 0) return(x)

  # --- Calendar ---
  if (is.null(cal)) {
    cal <- bizdays::create.calendar(
      name = "Brazil/ANBIMA",
      holidays = bizdays::holidays("Brazil/ANBIMA"),
      weekdays = c("saturday","sunday")
    )
  }

  # --- Column resolution (case-insensitive, tolerant) ---
  nm <- tolower(colnames(x))
  find_col <- function(candidates) {
    idx <- match(candidates, nm, nomatch = 0L)
    idx <- idx[idx > 0L]
    if (length(idx) == 0L) return(NA_integer_)
    idx[1L]
  }
  i_open  <- find_col(c("open","o"))
  i_high  <- find_col(c("high","h"))
  i_low   <- find_col(c("low","l"))
  i_close <- find_col(c("close","c","last","settle","settlement_price"))
  if (any(is.na(c(i_open,i_high,i_low,i_close))))
    stop("Could not resolve OHLC columns (need open/high/low/close as rate %).")

  # Optional volume-like column (keep first match if any)
  i_vol <- find_col(c("volume","vol","contracts_traded","qty","contracts","volume_brl","volume_brls"))

  r_open  <- as.numeric(x[, i_open])
  r_high  <- as.numeric(x[, i_high])
  r_low   <- as.numeric(x[, i_low])
  r_close <- as.numeric(x[, i_close])

  idx_dates <- zoo::index(x)

  # --- Helpers (pure, local) ---
  .months_between_floor <- function(basis_date, maturity_date) {
    as.integer(
      lubridate::interval(as.Date(basis_date), as.Date(maturity_date)) %/% months(1)
    )
  }
  .get_di_tick_size <- function(mm, basis_date, rule_change_date) {
    bd <- as.Date(basis_date)
    if (bd < rule_change_date) {
      if (mm <= 3) 0.001 else if (mm <= 60) 0.005 else 0.010
    } else {
      if (mm <= 3) 0.001 else 0.005
    }
  }
  .snap_one_rate <- function(rate, mm, basis_date) {
    tick <- .get_di_tick_size(mm, basis_date, rule_change_date)
    round(rate / tick) * tick
  }
  .biz_n <- function(basis_date, maturity_date, cal, include_basis_day = TRUE) {
    # bizdays(a,b) counts business days excluding 'a' and including 'b'
    n <- bizdays::bizdays(as.Date(basis_date), as.Date(maturity_date), cal)
    if (include_basis_day) {
      n <- n + as.integer(bizdays::is.bizday(as.Date(basis_date), cal))
    }
    if (n <= 0) stop("Non-positive valid-days. Check basis/maturity and calendar.")
    n
  }
  .pu_from_rate <- function(r, n, round_pu) {
    bad <- !is.finite(r) | r <= -100
    out <- 1e5 / (1 + r/100)^(n/252)
    out[bad] <- NA_real_
    if (round_pu) out <- round(out, 2L)
    out
  }
  .rate_from_pu <- function(pu, n) {
    bad <- !is.finite(pu) | pu <= 0
    out <- 100 * ((1e5 / pu)^(252 / n) - 1)
    out[bad] <- NA_real_
    out
  }

  # --- Resolve n (valid business days) and mm (months) per row ---
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
      integer(1)
    )
  } else if (is.numeric(maturity_date)) {
    n_const <- as.integer(maturity_date)
    if (n_const <= 0) stop("'maturity_date' as numeric must be positive business days.")
    n_vec <- rep.int(n_const, length(idx_dates))
    mm_vec <- as.numeric(n_vec) / 21  # coarse proxy only for tick-bucket selection
    md <- NA
  } else {
    stop("'maturity_date' must be a Date or numeric business days.")
  }

  # --- Optional: snap input rates to DI tick grid (per row) ---
  if (snap_to_tick) {
    r_open  <- mapply(.snap_one_rate, r_open,  mm_vec, idx_dates)
    r_high  <- mapply(.snap_one_rate, r_high,  mm_vec, idx_dates)
    r_low   <- mapply(.snap_one_rate, r_low,   mm_vec, idx_dates)
    r_close <- mapply(.snap_one_rate, r_close, mm_vec, idx_dates)
  }

  # --- Forward: rates -> PU (vectorized) ---
  PU_o <- .pu_from_rate(r_open,  n_vec, round_pu)
  PU_h <- .pu_from_rate(r_high,  n_vec, round_pu)
  PU_l <- .pu_from_rate(r_low,   n_vec, round_pu)
  PU_c <- .pu_from_rate(r_close, n_vec, round_pu)

  # --- Backward: PU -> rates (round-trip using the *rounded* PUs) ---
  adjOpen  <- .rate_from_pu(PU_o, n_vec)
  adjHigh  <- .rate_from_pu(PU_h, n_vec)
  adjLow   <- .rate_from_pu(PU_l, n_vec)
  adjClose <- .rate_from_pu(PU_c, n_vec)

  # Optionally snap the round-tripped rates to the tick grid
  if (snap_rates_back) {
    adjOpen  <- mapply(.snap_one_rate, adjOpen,  mm_vec, idx_dates)
    adjHigh  <- mapply(.snap_one_rate, adjHigh,  mm_vec, idx_dates)
    adjLow   <- mapply(.snap_one_rate, adjLow,   mm_vec, idx_dates)
    adjClose <- mapply(.snap_one_rate, adjClose, mm_vec, idx_dates)
  }

  # --- Close the loop: adj rates -> adjPU (should match PU_* up to rounding) ---
  adjPU_o <- .pu_from_rate(adjOpen,  n_vec, round_pu)
  adjPU_h <- .pu_from_rate(adjHigh,  n_vec, round_pu)
  adjPU_l <- .pu_from_rate(adjLow,   n_vec, round_pu)
  adjPU_c <- .pu_from_rate(adjClose, n_vec, round_pu)

  # --- Diffs for diagnostics ---
  diffOpen_pp  <- adjOpen  - r_open
  diffHigh_pp  <- adjHigh  - r_high
  diffLow_pp   <- adjLow   - r_low
  diffClose_pp <- adjClose - r_close

  diffPU_o <- adjPU_o - PU_o
  diffPU_h <- adjPU_h - PU_h
  diffPU_l <- adjPU_l - PU_l
  diffPU_c <- adjPU_c - PU_c

  # --- Assemble result xts ---
  base_cols <- cbind(
    open  = r_open,
    high  = r_high,
    low   = r_low,
    close = r_close
  )
  if (!is.na(i_vol)) {
    base_cols <- cbind(base_cols, volume = as.numeric(x[, i_vol]))
  }

  res <- xts::xts(
    cbind(
      base_cols,
      PU_o = as.numeric(PU_o),
      PU_h = as.numeric(PU_h),
      PU_l = as.numeric(PU_l),
      PU_c = as.numeric(PU_c)
      # adjOpen  = as.numeric(adjOpen),
      # adjHigh  = as.numeric(adjHigh),
      # adjLow   = as.numeric(adjLow),
      # adjClose = as.numeric(adjClose),
      # adjPU_o = as.numeric(adjPU_o),
      # adjPU_h = as.numeric(adjPU_h),
      # adjPU_l = as.numeric(adjPU_l),
      # adjPU_c = as.numeric(adjPU_c),
      # diffOpen_pp  = as.numeric(diffOpen_pp),
      # diffHigh_pp  = as.numeric(diffHigh_pp),
      # diffLow_pp   = as.numeric(diffLow_pp),
      # diffClose_pp = as.numeric(diffClose_pp),
      # diffPU_o = as.numeric(diffPU_o),
      # diffPU_h = as.numeric(diffPU_h),
      # diffPU_l = as.numeric(diffPU_l),
      # diffPU_c = as.numeric(diffPU_c)
    ),
    order.by = idx_dates,
    tzone = attr(x, "tzone")
  )
  res
}


# calendar
cal <- bizdays::create.calendar("Brazil/ANBIMA",
                                holidays = bizdays::holidays("Brazil/ANBIMA"),
                                weekdays = c("saturday","sunday"))

# maturidade do DI1F11 (1º dia útil do mês)
di_maturity_from_ticker <- function(ticker, cal) {
  month_letter <- c(F=1,G=2,H=3,J=4,K=5,M=6,N=7,Q=8,U=9,V=10,X=11,Z=12)
  m <- month_letter[substr(ticker, 4, 4)]
  y2 <- as.integer(substr(ticker, 5, 6))
  y4 <- ifelse(y2 >= 90, 1900 + y2, 2000 + y2)
  first_day <- as.Date(sprintf("%04d-%02d-01", y4, m))
  bizdays::adjust.next(first_day, cal)
}
md <- di_maturity_from_ticker("DI1F11", cal)

pu_xts <- di_ohlc_to_pu_augmented_xts(
  tudo$DI1F11[, c("open","high","low","close","volume")],
  maturity_date = md,
  cal = cal,
  include_basis_day = FALSE,
  snap_to_tick = FALSE,    # set TRUE if your OHLC are off-grid (non-tick)
  round_pu = FALSE,
  snap_rates_back = FALSE  # set TRUE to see round-trip on the official tick lattice
)
head(pu_xts)

