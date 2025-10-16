# DI continuous series for trend following:
# - Signals in Rate (OHLC of active contract)
# - P&L in PU (??PU of the held contract; roll at close)
# - Dynamic tick schedule (pre/post 2025-08-25)
# - Maturity month filter (e.g., c("F","N") or "all")
# - Modes: rates_only, pu_only, rates_pu_ohlc, full
# - Removes maturity-day rows (DU <= 0) and optional PU=K & Rate=0 rows

library(data.table)

# Short aliases
DT_ <- data.table::data.table
setorderv <- data.table::setorderv

# =============================== #
#   BUSINESS DAYS & DU HANDLING   #
# =============================== #
# Computes business-day distance (DU). If a 'bizdays' calendar is passed, use it.
.compute_du <- function(d, m, biz_cal = NULL) {
  stopifnot(length(d) == length(m))
  # Use bizdays calendar if provided and available
  if (!is.null(biz_cal) && requireNamespace("bizdays", quietly = TRUE)) {
    return(bizdays::bizdays(d, m, cal = biz_cal))
  }
  # If user configured default bizdays calendar
  if (requireNamespace("bizdays", quietly = TRUE) &&
    !is.null(getOption("bizdays.default.calendar"))) {
    return(bizdays::bizdays(d, m))
  }
  # Fallback: calendar days (approximation)
  as.integer(m - d)
}

# ====================================== #
#   TICK SCHEDULE (dynamic, by DU/date)  #
# ====================================== #
# Returns tick step in PERCENT points (e.g., 0.005 = 0.005%)
tick_step_percent <- function(refdate, DU,
                              change_date = as.Date("2025-08-25"),
                              du_3m = 63L, du_5y = 5L * 252L) {
  step <- rep(NA_real_, length(DU))
  before <- refdate < change_date
  after <- !before
  # PRE change: >5y=0.01%, 3m<DU<=5y=0.005%, DU<=3m=0.001%
  if (any(before)) {
    step[before & DU > du_5y] <- 0.01
    step[before & DU <= du_5y & DU > du_3m] <- 0.005
    step[before & DU <= du_3m] <- 0.001
  }
  # POST change: DU>3m=0.005%, DU<=3m=0.001%
  if (any(after)) {
    step[after & DU > du_3m] <- 0.005
    step[after & DU <= du_3m] <- 0.001
  }
  step
}

# Snap a rate (in PERCENT) to the schedule step for that (date, DU)
snap_rate_by_schedule <- function(rate_percent, refdate, DU,
                                  change_date = as.Date("2025-08-25"),
                                  du_3m = 63L, du_5y = 5L * 252L) {
  step <- tick_step_percent(refdate, DU, change_date, du_3m, du_5y)
  snapped <- rate_percent
  ok <- is.finite(rate_percent) & is.finite(step) & (DU > 0)
  snapped[ok] <- round(rate_percent[ok] / step[ok]) * step[ok]
  snapped[!ok] <- NA_real_
  snapped
}

# ====================================== #
#   PU <-> RATE CONVERTERS (r in %)      #
# ====================================== #
# include_basis_day=TRUE matches common B3 convention (adds +1 business day in exponent)
pu_from_rate <- function(rate_percent, DU, K = 100000, base = 252,
                         include_basis_day = TRUE) {
  r <- rate_percent / 100
  eff_DU <- DU + as.integer(include_basis_day)
  out <- K * (1 + r)^(-eff_DU / base)
  out[!is.finite(out) | eff_DU <= 0] <- NA_real_
  out
}

rate_from_pu <- function(PU, DU, refdate,
                         K = 100000, base = 252,
                         include_basis_day = TRUE,
                         change_date = as.Date("2025-08-25"),
                         du_3m = 63L, du_5y = 5L * 252L,
                         snap = TRUE) {
  eff_DU <- DU + as.integer(include_basis_day)
  r <- (K / PU)^(base / eff_DU) - 1
  rp <- 100 * r
  rp[!is.finite(rp) | eff_DU <= 0] <- NA_real_
  if (snap) {
    rp <- snap_rate_by_schedule(rp, refdate, DU, change_date, du_3m, du_5y)
  }
  rp
}

# ========================================== #
#   ACTIVE PICKER (DU_min + OI optional)     #
# ========================================== #
# Picks, for each refdate, the nearest maturity with DU >= du_min.
# If none and strict_du_min=TRUE: returns NA (no trade day). Else: pick smallest positive DU.
.pick_active_symbol <- function(DT, du_min = 63L, oi_col = NULL, strict_du_min = TRUE) {
  if (!is.null(oi_col) && oi_col %in% names(DT)) {
    setorderv(DT,
      c("refdate", "DU", oi_col, "estimated_maturity", "symbol"),
      c(1, 1, -1, 1, 1),
      na.last = TRUE
    )
  } else {
    setorderv(DT,
      c("refdate", "DU", "estimated_maturity", "symbol"),
      c(1, 1, 1, 1),
      na.last = TRUE
    )
  }
  DT[,
    {
      cand_ge <- .SD[DU >= du_min]
      if (nrow(cand_ge)) {
        cand_ge[1, .(symbol, DU, estimated_maturity)]
      } else if (strict_du_min) {
        data.table::data.table(symbol = NA_character_, DU = NA_integer_, estimated_maturity = as.Date(NA))
      } else {
        .SD[DU > 0][1, .(symbol, DU, estimated_maturity)]
      }
    },
    by = refdate
  ]
}

# ========================================== #
#   MONTH FILTER (maturities)                #
# ========================================== #
# maturities: "all" or c("F","G","H","J","K","M","N","Q","U","V","X","Z")
.filter_by_month_letters <- function(DT, maturities = "all", maturity_code_col = "maturity_code") {
  if (identical(maturities, "all")) {
    return(DT)
  }
  if (!maturity_code_col %in% names(DT)) stop("Missing column: ", maturity_code_col)
  m1 <- substr(DT[[maturity_code_col]], 1, 1)
  DT[m1 %in% maturities]
}

# ============================================================ #
#   MAIN: Trend-following xts (signal in Rate, P&L in PU)      #
# ============================================================ #
# mode:
#  "rates_only"     -> Rate_O/H/L/C (ACTIVE)
#  "pu_only"        -> PU_o/h/l/c   (ACTIVE)
#  "rates_pu_ohlc"  -> Rate_O/H/L/C + PU_o/h/l/c (ACTIVE)
#  "full"           -> ACTIVE: Rate_O/H/L/C + PU_o/h/l/c + DU + RollFlag
#                      HELD:   HeldRate_O/H/L/C + HeldPU_o/h/l/c
#                      PL, CumPL
# Attributes: ActiveSymbol_xts, HeldSymbol_xts (character xts)
di_build_trend_follow_xts <- function(
  df,
  rate_cols = c("open", "high", "low", "close"), # optional; if absent, derive from PU
  pu_cols = c("PU_o", "PU_h", "PU_l", "PU_c"),
  date_col = "refdate",
  symbol_col = "symbol",
  maturity_col = "estimated_maturity",
  maturity_code_col = "maturity_code",
  maturities = "all", # e.g., c("F","N") or "all"
  biz_cal = NULL,
  du_min = 63L, # you don't trade <= ~3m
  strict_du_min = TRUE,
  oi_col = "open_interest",
  K = 100000,
  base_days = 252,
  include_basis_day = TRUE, # IMPORTANT: match B3 PU convention
  change_date = as.Date("2025-08-25"),
  resnap_input_rates = FALSE, # if your source rates are already snapped, keep FALSE
  contract_multiplier = 1.0,
  mode = c("rates_pu_ohlc", "rates_only", "pu_only", "full"),
  drop_maturity_rows = TRUE # drop rows with PU???K & Rate=0 (redundant)
) {
  mode <- match.arg(mode)

  # ---- Validate required columns ----
  need <- c(symbol_col, date_col, maturity_col, pu_cols, maturity_code_col)
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))

  # ---- Prepare base DT ----
  DT <- data.table::as.data.table(df)
  # Filter maturity months
  DT <- .filter_by_month_letters(DT, maturities = maturities, maturity_code_col = maturity_code_col)

  # Types + keep rows with finite PU OHLC
  DT[, (date_col) := as.Date(get(date_col))]
  DT[, (maturity_col) := as.Date(get(maturity_col))]
  for (cc in pu_cols) DT <- DT[is.finite(get(cc))]

  # Compute DU and drop maturity-day rows
  DT[, DU := .compute_du(get(date_col), get(maturity_col), biz_cal = biz_cal)]
  DT <- DT[DU > 0]

  # Map DU per (date, symbol) for later joins (HELD DU)
  du_map <- DT[, .(
    refdate = get(date_col),
    symbol = get(symbol_col),
    DU
  )]

  # ??PU_c per symbol (for P&L of HELD)
  dpu_tbl <- DT[, .(
    symbol = get(symbol_col),
    refdate = get(date_col),
    PU_c = get(pu_cols[4])
  )]
  data.table::setorderv(dpu_tbl, c("symbol", "refdate"))
  dpu_tbl[, dPU := PU_c - data.table::shift(PU_c), by = symbol]
  dpu_tbl[is.na(dPU), dPU := 0]

  # Active picker (per date)
  act <- .pick_active_symbol(
    DT[, .(
      refdate = get(date_col),
      symbol = get(symbol_col),
      DU,
      estimated_maturity = get(maturity_col),
      open_interest = if (!is.null(oi_col) && oi_col %in% names(DT)) get(oi_col) else NA_real_
    )],
    du_min = du_min,
    oi_col = if (oi_col %in% names(DT)) oi_col else NULL,
    strict_du_min = strict_du_min
  )
  data.table::setorderv(act, "refdate")
  act[, RollFlag := as.integer(symbol != data.table::shift(symbol))]
  act[is.na(RollFlag), RollFlag := 0L]
  act[, HeldSymbol := data.table::shift(symbol, type = "lag")]

  # ACTIVE PU OHLC
  active_pu <- merge(
    act[, .(refdate, symbol, DU, RollFlag, HeldSymbol)],
    DT[, .(
      refdate = get(date_col), symbol = get(symbol_col),
      PU_o = get(pu_cols[1]), PU_h = get(pu_cols[2]),
      PU_l = get(pu_cols[3]), PU_c = get(pu_cols[4])
    )],
    by = c("refdate", "symbol"),
    all.x = TRUE
  )
  data.table::setorderv(active_pu, "refdate")

  # ACTIVE RATE OHLC (use provided or derive from PU)
  have_rates <- all(rate_cols %in% names(DT))
  if (have_rates) {
    active_rate <- merge(
      act[, .(refdate, symbol, DU)],
      DT[, .(
        refdate = get(date_col), symbol = get(symbol_col),
        Rate_O = get(rate_cols[1]), Rate_H = get(rate_cols[2]),
        Rate_L = get(rate_cols[3]), Rate_C = get(rate_cols[4])
      )],
      by = c("refdate", "symbol"),
      all.x = TRUE
    )
    if (isTRUE(resnap_input_rates)) {
      active_rate[, `:=`(
        Rate_O = snap_rate_by_schedule(Rate_O, refdate, DU, change_date = change_date),
        Rate_H = snap_rate_by_schedule(Rate_H, refdate, DU, change_date = change_date),
        Rate_L = snap_rate_by_schedule(Rate_L, refdate, DU, change_date = change_date),
        Rate_C = snap_rate_by_schedule(Rate_C, refdate, DU, change_date = change_date)
      )]
    }
  } else {
    # derive from PU using schedule snap and basis day
    active_rate <- active_pu[, .(refdate, symbol, DU,
      Rate_O = rate_from_pu(PU_o, DU, refdate,
        K = K, base = base_days, include_basis_day = include_basis_day,
        change_date = change_date, snap = TRUE
      ),
      Rate_H = rate_from_pu(PU_h, DU, refdate,
        K = K, base = base_days, include_basis_day = include_basis_day,
        change_date = change_date, snap = TRUE
      ),
      Rate_L = rate_from_pu(PU_l, DU, refdate,
        K = K, base = base_days, include_basis_day = include_basis_day,
        change_date = change_date, snap = TRUE
      ),
      Rate_C = rate_from_pu(PU_c, DU, refdate,
        K = K, base = base_days, include_basis_day = include_basis_day,
        change_date = change_date, snap = TRUE
      )
    )]
  }

  # HELD (yesterday's active) PU OHLC
  held_pu <- merge(
    act[, .(refdate, HeldSymbol)],
    DT[, .(
      refdate = get(date_col), symbol = get(symbol_col),
      PU_o, PU_h, PU_l, PU_c
    )],
    by.x = c("refdate", "HeldSymbol"),
    by.y = c("refdate", "symbol"),
    all.x = TRUE
  )
  data.table::setorderv(held_pu, "refdate")

  # HELD DU for rate snapping/conversion
  held_du <- merge(
    act[, .(refdate, HeldSymbol)],
    du_map,
    by.x = c("refdate", "HeldSymbol"),
    by.y = c("refdate", "symbol"),
    all.x = TRUE
  )
  data.table::setorderv(held_du, "refdate")
  setnames(held_du, "DU", "Held_DU")

  # HELD Rate OHLC (use provided rates if available; else derive from held PU using Held_DU)
  if (have_rates) {
    held_rate <- merge(
      act[, .(refdate, HeldSymbol)],
      DT[, .(
        refdate = get(date_col), symbol = get(symbol_col),
        Rate_O = get(rate_cols[1]), Rate_H = get(rate_cols[2]),
        Rate_L = get(rate_cols[3]), Rate_C = get(rate_cols[4])
      )],
      by.x = c("refdate", "HeldSymbol"),
      by.y = c("refdate", "symbol"),
      all.x = TRUE
    )
    held_rate <- merge(held_rate, held_du, by = "refdate", all.x = TRUE)
    if (isTRUE(resnap_input_rates)) {
      held_rate[, `:=`(
        Rate_O = snap_rate_by_schedule(Rate_O, refdate, Held_DU, change_date = change_date),
        Rate_H = snap_rate_by_schedule(Rate_H, refdate, Held_DU, change_date = change_date),
        Rate_L = snap_rate_by_schedule(Rate_L, refdate, Held_DU, change_date = change_date),
        Rate_C = snap_rate_by_schedule(Rate_C, refdate, Held_DU, change_date = change_date)
      )]
    }
  } else {
    held_rate <- merge(held_pu[, .(refdate, HeldSymbol, PU_o, PU_h, PU_l, PU_c)],
      held_du[, .(refdate, Held_DU)],
      by = "refdate", all.x = TRUE
    )
    held_rate[, `:=`(
      Rate_O = rate_from_pu(PU_o, Held_DU, refdate,
        K = K, base = base_days, include_basis_day = include_basis_day,
        change_date = change_date, snap = TRUE
      ),
      Rate_H = rate_from_pu(PU_h, Held_DU, refdate,
        K = K, base = base_days, include_basis_day = include_basis_day,
        change_date = change_date, snap = TRUE
      ),
      Rate_L = rate_from_pu(PU_l, Held_DU, refdate,
        K = K, base = base_days, include_basis_day = include_basis_day,
        change_date = change_date, snap = TRUE
      ),
      Rate_C = rate_from_pu(PU_c, Held_DU, refdate,
        K = K, base = base_days, include_basis_day = include_basis_day,
        change_date = change_date, snap = TRUE
      )
    )]
  }

  # P&L via ??PU_c of HELD
  held_pl <- merge(
    act[, .(refdate, HeldSymbol)],
    dpu_tbl[, .(refdate, symbol, dPU)],
    by.x = c("refdate", "HeldSymbol"),
    by.y = c("refdate", "symbol"),
    all.x = TRUE
  )
  data.table::setorderv(held_pl, "refdate")
  held_pl[is.na(dPU), dPU := 0]
  held_pl[, PL := contract_multiplier * dPU]
  held_pl[, CumPL := cumsum(PL)]

  # Assemble ACTIVE (rates + pu + DU)
  active_merged <- merge(
    active_rate[, .(refdate, Rate_O, Rate_H, Rate_L, Rate_C)],
    active_pu[, .(refdate, PU_o, PU_h, PU_l, PU_c, DU, RollFlag)],
    by = "refdate", all = TRUE
  )

  # Optional: drop rows with Rate=0 AND PU???K (redundant near maturity)
  if (isTRUE(drop_maturity_rows)) {
    eps <- 1e-6
    keep <- !((is.finite(active_merged$Rate_O) & active_merged$Rate_O == 0 &
      is.finite(active_merged$Rate_H) & active_merged$Rate_H == 0 &
      is.finite(active_merged$Rate_L) & active_merged$Rate_L == 0 &
      is.finite(active_merged$Rate_C) & active_merged$Rate_C == 0) &
      (is.finite(active_merged$PU_o) & abs(active_merged$PU_o - K) < eps &
        is.finite(active_merged$PU_h) & abs(active_merged$PU_h - K) < eps &
        is.finite(active_merged$PU_l) & abs(active_merged$PU_l - K) < eps &
        is.finite(active_merged$PU_c) & abs(active_merged$PU_c - K) < eps)
    )
    active_merged <- active_merged[keep]
    act <- act[refdate %in% active_merged$refdate]
    held_pl <- held_pl[refdate %in% active_merged$refdate]
    held_pu <- held_pu[refdate %in% active_merged$refdate]
    held_rate <- held_rate[refdate %in% active_merged$refdate]
  }

  # Build output per mode
  if (mode == "rates_only") {
    final_DT <- active_merged[, .(refdate, Rate_O, Rate_H, Rate_L, Rate_C)]
  } else if (mode == "pu_only") {
    final_DT <- active_merged[, .(refdate, PU_o, PU_h, PU_l, PU_c)]
  } else if (mode == "rates_pu_ohlc") {
    final_DT <- active_merged[, .(
      refdate, Rate_O, Rate_H, Rate_L, Rate_C,
      PU_o, PU_h, PU_l, PU_c
    )]
  } else { # "full"
    # Merge HELD OHLC (rate & PU) + PL
    full_list <- list(
      active_merged,
      held_pl[, .(refdate, PL, CumPL)]
    )
    # Held Rate
    if ("Held_DU" %in% names(held_rate)) {
      full_list[[length(full_list) + 1]] <- held_rate[, .(refdate,
        HeldRate_O = Rate_O, HeldRate_H = Rate_H, HeldRate_L = Rate_L, HeldRate_C = Rate_C
      )]
    } else {
      full_list[[length(full_list) + 1]] <- held_rate[, .(refdate,
        HeldRate_O = Rate_O, HeldRate_H = Rate_H, HeldRate_L = Rate_L, HeldRate_C = Rate_C
      )]
    }
    # Held PU
    full_list[[length(full_list) + 1]] <- held_pu[, .(refdate,
      HeldPU_o = PU_o, HeldPU_h = PU_h, HeldPU_l = PU_l, HeldPU_c = PU_c
    )]

    final_DT <- Reduce(function(x, y) merge(x, y, by = "refdate", all = TRUE), full_list)
  }

  data.table::setorderv(final_DT, "refdate")
  out_xts <- xts::xts(as.matrix(final_DT[, -1]), order.by = final_DT$refdate)

  # Attach symbols as xts attributes
  ActiveSymbol_xts <- xts::xts(act$symbol, order.by = act$refdate)
  colnames(ActiveSymbol_xts) <- "ActiveSymbol"
  HeldSymbol_xts <- xts::xts(act$HeldSymbol, order.by = act$refdate)
  colnames(HeldSymbol_xts) <- "HeldSymbol"
  attr(out_xts, "ActiveSymbol_xts") <- ActiveSymbol_xts
  attr(out_xts, "HeldSymbol_xts") <- HeldSymbol_xts

  out_xts
}
