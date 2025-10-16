# ============================================================
# Generic continuous series builder for non-DI futures
# (CCM, BGI, WIN, WDO, etc.) for trend following backtests.
#
# Methods:
#   - "panama"     : difference-based back-adjustment (preserves returns)
#   - "forward_rt" : multiplicative forward ratio adjustment (preserves early level)
#
# Selection & Roll:
#   - Active = nearest maturity with DTE > 0.
#   - Force roll when DTE <= roll_days_before_expiry (pick next maturity).
#
# Output:
#   mode="normal" -> adjusted OHLCV
#   mode="full"   -> adjusted OHLCV + DTE + DaysToRoll + RollFlag
#                    + raw OHLCV of the active contract (unadjusted)
#                    + attributes: ActiveSymbol_xts
#
# Timezone:
#   Output xts index in desired timezone (tz_out).
#
# Requirements: data.table, xts, zoo. 'bizdays' optional for business DTE.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")
if (!requireNamespace("xts", quietly = TRUE)) stop("Package 'xts' is required.")
if (!requireNamespace("zoo", quietly = TRUE)) stop("Package 'zoo' is required.")

DT_ <- data.table::data.table
setorderv <- data.table::setorderv
rleid <- data.table::rleid

# --------- DTE helpers (business if calendar provided; else calendar days) ----------
.compute_dte <- function(d, m, biz_cal = this_thing) {
  stopifnot(length(d) == length(m))
  if (!is.null(biz_cal) && requireNamespace("bizdays", quietly = TRUE)) {
    return(bizdays::bizdays(d, m, cal = biz_cal))
  }
  if (requireNamespace("bizdays", quietly = TRUE) &&
    !is.null(getOption("bizdays.default.calendar"))) {
    return(bizdays::bizdays(d, m))
  }
  as.integer(m - d)
}

# --------- Month filter by maturity_code first letter (e.g., "F","N") ----------
.filter_by_month_letters <- function(DT, maturities = "all", maturity_code_col = "maturity_code") {
  if (identical(maturities, "all")) {
    return(DT)
  }
  if (!maturity_code_col %in% names(DT)) stop("Missing column: ", maturity_code_col)
  m1 <- substr(DT[[maturity_code_col]], 1, 1)
  DT[m1 %in% maturities]
}

# --------- Active picker with early-roll by DTE threshold ----------
# Picks per date the nearest maturity with DTE >= dte_min; if none, pick smallest positive DTE.
# strict_dte_min=TRUE -> if none >= dte_min, mark NA (no trade day).
.pick_active_symbol_generic <- function(DT, dte_min = 5L, strict_dte_min = FALSE) {
  # DT needs: refdate, symbol, DTE, estimated_maturity
  setorderv(DT, c("refdate", "DTE", "estimated_maturity", "symbol"), c(1, 1, 1, 1), na.last = TRUE)
  DT[,
    {
      cand_ge <- .SD[DTE >= dte_min]
      if (nrow(cand_ge)) {
        cand_ge[1, .(symbol, DTE, estimated_maturity)]
      } else if (strict_dte_min) {
        data.table::data.table(symbol = NA_character_, DTE = NA_integer_, estimated_maturity = as.Date(NA))
      } else {
        .SD[DTE > 0][1, .(symbol, DTE, estimated_maturity)]
      }
    },
    by = refdate
  ]
}

# --------- Adjustment factors at roll boundaries ----------
# Boundary utility: get Close_old and Close_new on the roll date t_b.
.get_boundary_prices <- function(all_prices, t_b, old_sym, new_sym, close_col = "Close") {
  # all_prices: data.table(refdate, symbol, Open, High, Low, Close, Volume)
  po <- all_prices[refdate == t_b & symbol == old_sym, get(close_col)]
  pn <- all_prices[refdate == t_b & symbol == new_sym, get(close_col)]
  if (!length(po) || !is.finite(po)) po <- NA_real_
  if (!length(pn) || !is.finite(pn)) pn <- NA_real_
  list(P_old = po, P_new = pn)
}

# --------- Apply "panama" (difference) or "forward_rt" (ratio forward) ----------
# active_raw must have contiguous segments by symbol; columns: refdate, symbol, Open, High, Low, Close, Volume
# all_prices is the same universe to query boundary prices (by symbol, refdate).
.apply_adjustment <- function(active_raw, all_prices,
                              method = c("panama", "forward_rt"),
                              close_col = "Close") {
  method <- match.arg(method)
  if (nrow(active_raw) == 0) {
    return(active_raw)
  }
  # Segment id for constant symbol runs
  active_raw[, seg_id := rleid(symbol)]
  seg_meta <- active_raw[, .(
    start_date = min(refdate),
    end_date = max(refdate),
    symbol = first(symbol)
  ),
  by = seg_id
  ]
  setorderv(seg_meta, "seg_id")

  # If only 1 segment, nothing to do
  if (nrow(seg_meta) <= 1) {
    active_raw[, adj_add := 0.0][, adj_mul := 1.0]
  } else {
    # Compute boundary diffs/ratios between seg_k and seg_{k+1} at t_b = start_date of seg_{k+1}
    diffs <- numeric(nrow(seg_meta) - 1L)
    rats <- numeric(nrow(seg_meta) - 1L)
    for (k in seq_len(nrow(seg_meta) - 1L)) {
      t_b <- seg_meta$start_date[k + 1L]
      s_old <- seg_meta$symbol[k]
      s_new <- seg_meta$symbol[k + 1L]
      bp <- .get_boundary_prices(all_prices, t_b, s_old, s_new, close_col = close_col)
      P_old <- as.numeric(bp$P_old)
      P_new <- as.numeric(bp$P_new)
      if (!is.finite(P_old) || !is.finite(P_new) || P_new == 0) {
        diffs[k] <- 0
        rats[k] <- 1
      } else {
        diffs[k] <- P_new - P_old
        rats[k] <- P_old / P_new
      }
    }

    # Panama: additive adjustments applied backward;
    # For segment s: adj_add[s] = sum_{j=s}^{last-1} diffs[j]; last segment gets 0.
    adj_add <- rep(0, nrow(seg_meta))
    if (length(diffs)) {
      cs <- rev(cumsum(rev(diffs))) # reversed cumulative
      adj_add[seq_len(nrow(seg_meta) - 1)] <- cs[-1] # drop the last (which belongs to nothing before it)
    }

    # Forward RT: multiplicative adjustments applied forward from the first;
    # For segment s: adj_mul[s] = prod_{j=1}^{s-1} rats[j]; first segment gets 1.
    adj_mul <- rep(1, nrow(seg_meta))
    if (length(rats)) {
      adj_mul[-1] <- cumprod(rats)
    }

    active_raw <- merge(active_raw, seg_meta[, .(seg_id, adj_add, adj_mul)],
      by = "seg_id", all.x = TRUE, sort = FALSE
    )
  }

  # Apply chosen method to OHLC
  if (method == "panama") {
    active_raw[, `:=`(
      Open  = Open + adj_add,
      High  = High + adj_add,
      Low   = Low + adj_add,
      Close = Close + adj_add
    )]
  } else { # forward_rt
    active_raw[, `:=`(
      Open  = Open * adj_mul,
      High  = High * adj_mul,
      Low   = Low * adj_mul,
      Close = Close * adj_mul
    )]
  }
  active_raw[]
}

# ============================================================
# Main generic builder
# ============================================================
build_trend_follow_xts <- function(
  df,
  underlying = c("CCM", "BGI", "WIN", "WDO", "OTHER"),
  # column mapping
  date_col = "refdate",
  symbol_col = "symbol",
  maturity_col = "estimated_maturity",
  maturity_code_col = "maturity_code",
  ohlc_cols = c("open", "high", "low", "close"),
  volume_col = "volume",
  # method & roll
  method = c("panama", "forward_rt"),
  roll_days_before_expiry = 5L, # typical early roll
  strict_dte_min = FALSE, # if TRUE, days without DTE>=roll_days -> NA rows
  # filters & tz
  maturities = "all", # e.g., c("F","N") or "all"
  biz_cal = this_thing, # bizdays calendar or NULL
  tz_out = "America/Sao_Paulo",
  # output
  mode = c("normal", "full")
) {
  method <- match.arg(method)
  mode <- match.arg(mode)
  underlying <- match.arg(underlying)

  # --- Validate columns ---
  need <- c(date_col, symbol_col, maturity_col, maturity_code_col, ohlc_cols[1:4])
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))

  DT <- data.table::as.data.table(df)
  # Filter maturity months
  DT <- .filter_by_month_letters(DT, maturities = maturities, maturity_code_col = maturity_code_col)

  # Types + finite OHLC
  DT[, (date_col) := as.Date(get(date_col))]
  DT[, (maturity_col) := as.Date(get(maturity_col))]
  for (cc in ohlc_cols[1:4]) DT <- DT[is.finite(get(cc))]
  if (volume_col %in% names(DT)) {
    DT[!is.finite(get(volume_col)), (volume_col) := NA_real_]
  }

  # DTE and drop maturity-day rows (DTE > 0 only)
  DT[, DTE := .compute_dte(get(date_col), get(maturity_col), biz_cal = biz_cal)]
  DT <- DT[DTE > 0]

  # Universe (by symbol-date) with raw OHLCV (for boundary lookups)
  uni <- DT[, .(
    refdate = get(date_col),
    symbol = get(symbol_col),
    Open = get(ohlc_cols[1]),
    High = get(ohlc_cols[2]),
    Low = get(ohlc_cols[3]),
    Close = get(ohlc_cols[4]),
    Volume = if (volume_col %in% names(DT)) get(volume_col) else NA_real_,
    estimated_maturity = get(maturity_col),
    DTE = DTE
  )]

  # Active per day with early roll threshold
  act <- .pick_active_symbol_generic(
    DT[, .(
      refdate = get(date_col),
      symbol = get(symbol_col),
      DTE,
      estimated_maturity = get(maturity_col)
    )],
    dte_min = as.integer(roll_days_before_expiry),
    strict_dte_min = strict_dte_min
  )
  data.table::setorderv(act, "refdate")

  # ---- FIX: avoid DTE.x/DTE.y ----
  # Join raw OHLCV for ACTIVE contract, bringing DTE only from 'act'
  active_raw <- merge(
    act, # has refdate, symbol, DTE, estimated_maturity
    uni[, .(refdate, symbol, Open, High, Low, Close, Volume)], # no DTE here
    by = c("refdate", "symbol"),
    all.x = TRUE
  )
  data.table::setorderv(active_raw, "refdate")

  # Keep a copy for "full" mode
  active_raw_copy <- data.table::copy(active_raw)

  # DaysToRoll uses the single DTE column (from 'act')
  active_raw[, DaysToRoll := pmax(0L, DTE - as.integer(roll_days_before_expiry))]

  # Remove NA rows if strict_dte_min=TRUE created gaps
  active_raw <- active_raw[is.finite(Close)]

  # Apply adjustment method to build continuous OHLC
  adjusted <- .apply_adjustment(
    active_raw[, .(refdate, symbol, Open, High, Low, Close, Volume)],
    all_prices = uni,
    method = method,
    close_col = "Close"
  )

  # Build xts index with tz
  idx <- as.POSIXct(adjusted$refdate, tz = tz_out)

  # ---- FIX: define sym regardless of mode ----
  sym <- adjusted$symbol

  if (mode == "normal") {
    out <- xts::xts(
      as.matrix(adjusted[, .(Open, High, Low, Close, Volume = Volume)]),
      order.by = idx, tzone = tz_out
    )
  } else {
    # "full" mode: add DTE, DaysToRoll, RollFlag, and RAW active OHLCV
    rollflag <- c(0L, as.integer(sym[-1] != sym[-length(sym)]))

    # Merge back DTE/DaysToRoll (aligned by date)
    diag_dt <- merge(
      adjusted[, .(refdate, symbol)],
      active_raw[, .(refdate, DTE, DaysToRoll)],
      by = "refdate", all.x = TRUE
    )

    # Raw active OHLCV aligned
    raw_dt <- merge(
      adjusted[, .(refdate, symbol)],
      active_raw_copy[, .(refdate,
        RawOpen = Open,
        RawHigh = High,
        RawLow = Low,
        RawClose = Close,
        RawVolume = Volume
      )],
      by = "refdate", all.x = TRUE
    )

    full_mat <- cbind(
      adjusted[, .(Open, High, Low, Close, Volume = Volume)],
      DTE = diag_dt$DTE,
      DaysToRoll = diag_dt$DaysToRoll,
      RollFlag = rollflag,
      raw_dt[, .(RawOpen, RawHigh, RawLow, RawClose, RawVolume)]
    )
    out <- xts::xts(as.matrix(full_mat), order.by = idx, tzone = tz_out)
  }

  # Attributes
  ActiveSymbol_xts <- xts::xts(sym, order.by = idx)
  colnames(ActiveSymbol_xts) <- "ActiveSymbol"
  attr(out, "ActiveSymbol_xts") <- ActiveSymbol_xts
  attr(out, "method") <- method
  attr(out, "underlying") <- underlying
  attr(out, "roll_days_before_expiry") <- as.integer(roll_days_before_expiry)
  attr(out, "tz") <- tz_out

  out
}
