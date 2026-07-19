#' Build DI futures continuous series with an explicit contract-selection rule
#'
#' DI futures are quoted in annualized rates, while fills and risk are often
#' represented through the contract's notional price (PU). For integer yearly
#' tenors restricted to January contracts, the default `"auto"` mode selects
#' the January maturity `N` calendar years ahead (for example, 3Y in 2024 uses
#' DI1F27 and rolls to DI1F28 on the first observed 2025 session). Other
#' configurations retain the historical strict business-day-floor rule.
#' Contract maturities are monotonic after the series starts: once the series
#' rolls forward, a disappearing/reappearing contract can never make it roll
#' backward. By default, days without a contract eligible under the resolved
#' selection rule are discarded only before the first selected row and fail
#' closed thereafter. Duplicate `(date, ticker, maturity)` quotes are collapsed only
#' when every economic field agrees; conflicting quotes fail closed before
#' contract selection. Textual source/provenance differences alone do not make
#' otherwise identical quotes conflict.
#' It augments the selected rows with raw PU and real-contract identity for
#' research backtests. It is not a synthetic liquidable contract: an executor
#' must keep a per-contract ledger and model each roll as closing the old
#' contract and opening the new one. When `include_pnl = TRUE`,
#' the added `PU_pnl`/return fields are continuous-series approximations based
#' on PU marks and roll adjustments; they are not official B3 DI variation
#' margin. Use `get_brfut_di_adjustments()` or the `di_adjustment_points`
#' treatment when settled DI cash PnL is required.
#'
#' @param data Data frame returned by `get_brfut_agg()` (must contain `date`,
#'   `root`, `ticker`, `maturity`, and OHLC rate columns). Use
#'   `treatment = "di_adjustments"` when official settlement fields and
#'   contracts-traded volume must be preserved.
#' @param target_tenor Desired time-to-maturity. Interpreted according to
#'   `tenor_unit`. Accepts a numeric scalar or vector (e.g. `c(1, 3)`).
#' @param tenor_unit Unit for `target_tenor`: `"years"` (default), `"months"`,
#'   or `"business_days"`.
#' @param root Futures root to keep (defaults to `"DI1"`).
#' @param allowed_maturities Either `"all"` (default) or a character vector of
#'   month codes (e.g. `c("F", "G", "H")`) to restrict eligible contracts.
#' @param cal Optional `bizdays` calendar; defaults to the ANBIMA national
#'   financial-market calendar used for DI1 DU. This is intentionally separate
#'   from the B3 trading-session calendar.
#' @param include_pnl When `TRUE`, adds PU-mark approximation P&L, return,
#'   adjusted OHLC, and index columns.
#' @param add_attrs When `TRUE` (default), enrich the series with futures
#'   metadata via `.brf_add_futures_attrs()`. Set to `FALSE` to skip.
#' @param add_globalenv When `TRUE` (default), assigns the resulting series into
#'   the global environment. Set to `FALSE` to skip.
#' @param strict_target In `"strict_du_floor"` mode, `TRUE` (default) never
#'   selects a contract below the requested tenor. An ineligible prefix may be
#'   discarded before the series starts; any later day without an eligible
#'   monotonic contract aborts the build. Set to `FALSE` only to explicitly
#'   allow the longest available monotonic contract below target. Calendar
#'   horizon selection requires `TRUE`.
#' @param selection_mode Contract-selection rule. `"auto"` (default) resolves
#'   to `"calendar_horizon"` only for positive integer `"years"` tenors with
#'   January-only (`"F"`) maturities; otherwise it resolves to
#'   `"strict_du_floor"`. The explicit legacy mode is useful for reproducing
#'   pre-calendar-horizon research.
#' @param coverage_mode Coverage behavior after the first eligible row.
#'   `"first_eligible"` (default) preserves fail-closed behavior and aborts on
#'   any later selection gap. `"restart_strict_suffix"` is an explicit
#'   `strict_target = TRUE` mode for sparse historical universes: it discards
#'   the earlier prefix at each internal selection gap, resets monotonic
#'   maturity state, and returns the final greedy strict suffix. The gap day is
#'   re-evaluated as a fresh start; a day with no contract at or above target is
#'   dropped. Missing roll bridges still abort.
#' @return An `xts` object when a single tenor is supplied, or a named list of
#'   `xts` objects when `target_tenor` has length > 1. Attributes include the
#'   roll schedule, active contracts, and the ordinal-to-symbol contract map.
#' @export
build_continuous_di <- function(data,
                                target_tenor = 1,
                                tenor_unit = c("years", "months", "business_days"),
                                root = "DI1",
                                allowed_maturities = "all",
                                cal = NULL,
                                include_pnl = FALSE,
                                add_attrs = TRUE,
                                add_globalenv = TRUE,
                                strict_target = TRUE,
                                selection_mode = c("auto", "calendar_horizon", "strict_du_floor"),
                                coverage_mode = c("first_eligible", "restart_strict_suffix")) {
  .brf_di_require_bizdays()
  tenor_unit <- match.arg(tenor_unit)
  selection_mode <- match.arg(selection_mode)
  coverage_mode <- match.arg(coverage_mode)
  if (!is.logical(strict_target) || length(strict_target) != 1L || is.na(strict_target)) {
    stop("'strict_target' must be TRUE or FALSE.", call. = FALSE)
  }
  if (identical(coverage_mode, "restart_strict_suffix") && !isTRUE(strict_target)) {
    stop("'coverage_mode = restart_strict_suffix' requires 'strict_target = TRUE'.", call. = FALSE)
  }
  if (length(target_tenor) > 1L) {
    series_list <- lapply(target_tenor, function(tenor_val) {
      .brf_di_build_single_tenor(
        data = data,
        target_tenor = tenor_val,
        tenor_unit = tenor_unit,
        root = root,
        allowed_maturities = allowed_maturities,
        cal = cal,
        include_pnl = include_pnl,
        add_attrs = add_attrs,
        add_globalenv = add_globalenv,
        strict_target = strict_target,
        selection_mode = selection_mode,
        coverage_mode = coverage_mode
      )
    })
    names(series_list) <- .brf_di_label_tenor(target_tenor, tenor_unit)
    return(series_list)
  }
  .brf_di_build_single_tenor(
    data = data,
    target_tenor = target_tenor,
    tenor_unit = tenor_unit,
    root = root,
    allowed_maturities = allowed_maturities,
    cal = cal,
    include_pnl = include_pnl,
    add_attrs = add_attrs,
    add_globalenv = add_globalenv,
    strict_target = strict_target,
    selection_mode = selection_mode,
    coverage_mode = coverage_mode
  )
}

.brf_di_label_tenor <- function(target_tenor, tenor_unit) {
  suffix <- switch(tenor_unit,
    years = "Y",
    months = "M",
    business_days = "BD"
  )
  vals <- gsub("\\.0+$", "", format(target_tenor))
  paste0(vals, suffix)
}

.brf_di_resolve_target_days <- function(target_tenor, tenor_unit) {
  stopifnot(length(target_tenor) == 1L, is.numeric(target_tenor), is.finite(target_tenor))
  if (target_tenor <= 0) {
    stop("'target_tenor' must be positive.", call. = FALSE)
  }
  switch(tenor_unit,
    years = as.integer(round(target_tenor * 252)),
    months = as.integer(round(target_tenor * 21)),
    business_days = as.integer(round(target_tenor)),
    stop("Unsupported 'tenor_unit'.", call. = FALSE)
  )
}

.brf_di_calendar_horizon_eligible <- function(target_tenor,
                                               tenor_unit,
                                               allowed_maturities) {
  target <- suppressWarnings(as.numeric(target_tenor))
  allowed <- toupper(trimws(as.character(allowed_maturities)))
  identical(tenor_unit, "years") &&
    length(target) == 1L && is.finite(target) && target > 0 &&
    abs(target - round(target)) < sqrt(.Machine$double.eps) &&
    identical(allowed, "F")
}

.brf_di_resolve_selection_mode <- function(selection_mode,
                                            target_tenor,
                                            tenor_unit,
                                            allowed_maturities) {
  mode <- match.arg(
    selection_mode,
    c("auto", "calendar_horizon", "strict_du_floor")
  )
  calendar_eligible <- .brf_di_calendar_horizon_eligible(
    target_tenor,
    tenor_unit,
    allowed_maturities
  )
  if (identical(mode, "auto")) {
    return(if (calendar_eligible) "calendar_horizon" else "strict_du_floor")
  }
  if (identical(mode, "calendar_horizon") && !calendar_eligible) {
    stop(
      "'selection_mode = calendar_horizon' requires a positive integer years tenor and allowed_maturities = 'F'.",
      call. = FALSE
    )
  }
  mode
}

.brf_di_selection_version <- function(selection_mode, strict_target = TRUE) {
  if (identical(selection_mode, "calendar_horizon")) {
    return("calendar_year_january_contract_v1")
  }
  if (isTRUE(strict_target)) "strict_du_floor_v1" else "du_floor_with_fallback_v1"
}

.brf_di_calendar_target_year <- function(basis_date, target_tenor) {
  as.integer(format(as.Date(basis_date), "%Y")) + as.integer(round(target_tenor))
}

.brf_di_duplicate_economic_fields <- c(
  "open_interest",
  "close_interest",
  "trade_count",
  "contracts_traded",
  "volume",
  "volume_qty",
  "open",
  "high",
  "low",
  "average_price",
  "close",
  "settlement_price",
  "previous_settlement",
  "corrected_settlement",
  "change_percent",
  "change_points",
  "last_bid",
  "last_ask",
  "reference_settlement",
  "reference_price",
  "adjustment_value"
)

.brf_di_collapse_duplicate_quotes <- function(data) {
  economic_fields <- intersect(.brf_di_duplicate_economic_fields, names(data))
  if (length(economic_fields)) {
    data[economic_fields] <- lapply(data[economic_fields], function(value) {
      if (is.numeric(value)) {
        return(value)
      }
      suppressWarnings(as.numeric(as.character(value)))
    })
  }

  key <- interaction(
    data$date,
    data$ticker,
    data$maturity,
    drop = TRUE,
    lex.order = TRUE
  )
  groups <- split(seq_len(nrow(data)), key, drop = TRUE)
  groups <- groups[lengths(groups) > 1L]
  if (!length(groups)) {
    return(data)
  }

  keep <- rep(TRUE, nrow(data))
  metadata_fields <- setdiff(names(data), economic_fields)
  for (rows in groups) {
    conflicting <- vapply(economic_fields, function(field) {
      observed <- unique(data[[field]][rows][!is.na(data[[field]][rows])])
      length(observed) > 1L
    }, logical(1))
    if (any(conflicting)) {
      first <- rows[[1L]]
      stop(
        "Conflicting duplicate DI quotes for key (date=", format(data$date[[first]]),
        ", ticker=", data$ticker[[first]],
        ", maturity=", format(data$maturity[[first]]),
        "): economic fields differ: ",
        paste(names(conflicting)[conflicting], collapse = ", "),
        ". Continuous selection requires one economic quote per contract/session.",
        call. = FALSE
      )
    }

    # Pick the same representative regardless of input order. Textual source
    # or provenance fields do not create an economic conflict; they only make
    # the otherwise-equivalent representative ordering canonical.
    signatures <- vapply(rows, function(row) {
      values <- vapply(metadata_fields, function(field) {
        value <- data[[field]][row]
        if (!length(value) || is.na(value)) "<NA>" else as.character(value)
      }, character(1))
      paste(values, collapse = "\u001f")
    }, character(1))
    canonical <- rows[order(signatures, method = "radix")][[1L]]

    # Missing fields may be complemented by an economically identical copy.
    # This is deterministic because every non-missing value was proved equal.
    for (field in economic_fields) {
      observed <- data[[field]][rows][!is.na(data[[field]][rows])]
      if (length(observed)) {
        data[[field]][canonical] <- observed[[1L]]
      }
    }
    keep[setdiff(rows, canonical)] <- FALSE
  }

  data[keep, , drop = FALSE]
}

.brf_di_prepare_continuous_data <- function(data,
                                            root,
                                            allowed_maturities,
                                            cal) {
  if (!is.data.frame(data) || !nrow(data)) {
    stop("`data` must be a non-empty data frame with DI futures quotes.", call. = FALSE)
  }
  root_main <- .brf_normalize_root(root)
  data <- data[toupper(trimws(as.character(data$root))) == root_main, , drop = FALSE]
  if (!nrow(data)) {
    stop("No data available for root ", root_main, ".", call. = FALSE)
  }
  data$date <- as.Date(data$date)
  data$maturity <- as.Date(data$maturity)
  data$ticker <- toupper(trimws(as.character(data$ticker)))
  data <- data[!is.na(data$date) & !is.na(data$maturity), , drop = FALSE]
  if (!nrow(data)) {
    stop("No valid rows after parsing dates/maturities.", call. = FALSE)
  }
  if (!"volume_qty" %in% names(data) && "contracts_traded" %in% names(data)) {
    data$volume_qty <- data$contracts_traded
  }
  numeric_cols <- intersect(c("open", "high", "low", "close", "volume", "volume_qty"), names(data))
  if (length(numeric_cols)) {
    data[numeric_cols] <- lapply(data[numeric_cols], function(col) {
      if (is.numeric(col)) {
        col
      } else {
        suppressWarnings(as.numeric(col))
      }
    })
  }
  rate_cols <- intersect(c("open", "high", "low", "close"), names(data))
  if (length(rate_cols) != 4L) {
    stop("DI continuous data requires open/high/low/close rate columns.", call. = FALSE)
  }
  # Normalize against the complete session cross-section before quote
  # eligibility or maturity filters remove contracts. The bounded legacy
  # HTML regime needs at least three maturities to establish its common carry
  # factor, including maturities that are not themselves selectable here.
  data <- .brf_di_collapse_duplicate_quotes(data)
  data <- .brf_repair_di_traded_zero_ohlc(data)
  data <- .brf_add_di_adjustment_columns(data)
  rate_matrix <- do.call(cbind, lapply(rate_cols, function(rate_col) {
    suppressWarnings(as.numeric(data[[rate_col]]))
  }))
  valid_quote <- rowSums(is.finite(rate_matrix)) == length(rate_cols) &
    rowSums(rate_matrix != 0, na.rm = TRUE) == length(rate_cols)
  data <- data[valid_quote, , drop = FALSE]
  if (!nrow(data)) {
    stop("No valid DI OHLC quotes available for the continuous series.", call. = FALSE)
  }
  # Recompute from the canonical rate OHLC even when legacy PU columns are
  # present, because older caches stored PU_high/PU_low with inverted meaning.
  data <- .brf_di_add_pu_columns(data)
  cal <- .brf_di_resolve_calendar(cal)
  data$valid_days <- .brf_di_safe_valid_days(data$date, data$maturity, cal, include_basis_day = FALSE)
  data$month_code <- .brf_extract_month_code(data$ticker, root_main)
  allowed <- allowed_maturities
  if (!(is.character(allowed) && length(allowed) == 1L && toupper(allowed) == "ALL")) {
    allowed <- unique(toupper(trimws(as.character(allowed))))
    allowed <- allowed[nzchar(allowed)]
    data <- data[data$month_code %in% allowed, , drop = FALSE]
    if (!nrow(data)) {
      stop("No rows left after filtering by allowed maturities.", call. = FALSE)
    }
  } else {
    allowed <- "ALL"
  }
  data <- data[data$valid_days > 0, , drop = FALSE]
  if (!nrow(data)) {
    stop("No rows with positive business days to maturity.", call. = FALSE)
  }
  data <- data[order(data$date, data$maturity, data$ticker), , drop = FALSE]
  rownames(data) <- NULL
  contract_map <- .brf_di_contract_map(data)
  data$contract_ordinal <- contract_map$contract_ordinal[
    match(data$ticker, contract_map$contract_symbol)
  ]
  list(
    data = data,
    root = root_main,
    allowed_maturities = allowed,
    contract_map = contract_map
  )
}

.brf_di_contract_map <- function(data) {
  pairs <- unique(data.frame(
    contract_symbol = as.character(data$ticker),
    actual_maturity = as.Date(data$maturity),
    stringsAsFactors = FALSE
  ))
  maturity_counts <- lengths(split(pairs$actual_maturity, pairs$contract_symbol))
  if (any(maturity_counts != 1L)) {
    bad <- names(maturity_counts)[maturity_counts != 1L]
    stop(
      "Each DI contract symbol must map to exactly one maturity; invalid: ",
      paste(bad, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  pairs <- pairs[order(pairs$actual_maturity, pairs$contract_symbol), , drop = FALSE]
  rownames(pairs) <- NULL
  pairs$contract_ordinal <- seq_len(nrow(pairs))
  pairs[, c("contract_ordinal", "contract_symbol", "actual_maturity"), drop = FALSE]
}

.brf_di_safe_valid_days <- function(basis_date, maturity_date, cal, include_basis_day = FALSE) {
  basis <- as.Date(basis_date)
  maturity <- as.Date(maturity_date)
  ok <- !is.na(basis) & !is.na(maturity)
  if (!any(ok)) {
    return(rep(NA_integer_, length(basis)))
  }
  raw <- bizdays::bizdays(basis[ok], maturity[ok], cal)
  if (isTRUE(include_basis_day)) {
    raw <- raw + as.integer(bizdays::is.bizday(basis[ok], cal))
  }
  out <- rep(NA_integer_, length(basis))
  out[ok] <- as.integer(raw)
  out
}

.brf_di_pick_contract <- function(rows,
                                  target_days,
                                  minimum_maturity = as.Date(NA),
                                  strict_target = TRUE,
                                  selection_mode = "strict_du_floor",
                                  basis_date = as.Date(NA),
                                  target_tenor = NA_real_) {
  if (!nrow(rows)) {
    return(rows)
  }
  rows <- rows[order(rows$valid_days, rows$maturity, rows$ticker), , drop = FALSE]
  minimum_maturity <- as.Date(minimum_maturity)[1L]
  if (!is.na(minimum_maturity)) {
    rows <- rows[as.Date(rows$maturity) >= minimum_maturity, , drop = FALSE]
  }
  if (!nrow(rows)) {
    return(rows)
  }
  if (identical(selection_mode, "calendar_horizon")) {
    basis_date <- as.Date(basis_date)[1L]
    if (is.na(basis_date)) {
      stop("Calendar-horizon DI selection requires a valid session date.", call. = FALSE)
    }
    target_year <- .brf_di_calendar_target_year(basis_date, target_tenor)
    maturity_year <- suppressWarnings(as.integer(format(as.Date(rows$maturity), "%Y")))
    candidates <- rows[
      rows$month_code == "F" & maturity_year == target_year &
        as.Date(rows$maturity) > basis_date,
      ,
      drop = FALSE
    ]
    if (!nrow(candidates)) {
      return(rows[0, , drop = FALSE])
    }
    symbols <- unique(as.character(candidates$ticker))
    if (length(symbols) != 1L) {
      stop(
        "Calendar-horizon DI selection is ambiguous for session ",
        format(basis_date), " and target year ", target_year, ": ",
        paste(symbols, collapse = ", "), ".",
        call. = FALSE
      )
    }
    candidates <- candidates[order(candidates$maturity, candidates$ticker), , drop = FALSE]
    return(candidates[1L, , drop = FALSE])
  }
  forward <- rows[
    is.finite(rows$valid_days) & rows$valid_days >= target_days,
    ,
    drop = FALSE
  ]
  if (nrow(forward)) {
    return(forward[1, , drop = FALSE])
  }
  if (isTRUE(strict_target)) {
    return(rows[0, , drop = FALSE])
  }
  rows[which.max(rows$valid_days), , drop = FALSE]
}

.brf_di_find_bridge_data <- function(from_df, to_df, switch_date) {
  if (is.null(from_df) || is.null(to_df) || !nrow(from_df) || !nrow(to_df)) {
    return(list(
      bridge_date = as.Date(NA),
      from_pu = NA_real_,
      to_pu = NA_real_,
      from_rate = NA_real_,
      to_rate = NA_real_
    ))
  }
  from_dates <- as.Date(from_df$date)
  to_dates <- as.Date(to_df$date)
  common <- intersect(from_dates, to_dates)
  common <- common[common <= switch_date]
  if (!length(common)) {
    return(list(
      bridge_date = as.Date(NA),
      from_pu = NA_real_,
      to_pu = NA_real_,
      from_rate = NA_real_,
      to_rate = NA_real_
    ))
  }
  bridge_date <- max(common)

  from_row <- from_df[as.Date(from_df$date) == bridge_date, , drop = FALSE]
  to_row <- to_df[as.Date(to_df$date) == bridge_date, , drop = FALSE]

  from_pu <- NA_real_
  to_pu <- NA_real_
  from_rate <- NA_real_
  to_rate <- NA_real_

  if (nrow(from_row)) {
    if ("PU_close" %in% names(from_row)) {
      from_pu <- suppressWarnings(as.numeric(from_row$PU_close[1]))
    }
    if ("close" %in% names(from_row)) {
      from_rate <- suppressWarnings(as.numeric(from_row$close[1]))
    }
  }
  if (nrow(to_row)) {
    if ("PU_close" %in% names(to_row)) {
      to_pu <- suppressWarnings(as.numeric(to_row$PU_close[1]))
    }
    if ("close" %in% names(to_row)) {
      to_rate <- suppressWarnings(as.numeric(to_row$close[1]))
    }
  }

  list(
    bridge_date = bridge_date,
    from_pu = from_pu,
    to_pu = to_pu,
    from_rate = from_rate,
    to_rate = to_rate
  )
}

.brf_di_roll_schedule <- function(selected, per_ticker) {
  n <- nrow(selected)
  if (n <= 1L) {
    return(data.frame(
      from_ticker = character(),
      to_ticker = character(),
      switch_date = as.Date(character()),
      bridge_date = as.Date(character()),
      from_pu_close = numeric(),
      to_pu_close = numeric(),
      pu_ratio = numeric(),
      pu_diff = numeric(),
      from_rate_close = numeric(),
      to_rate_close = numeric(),
      rate_diff = numeric(),
      switch_position = integer(),
      stringsAsFactors = FALSE
    ))
  }
  tickers <- selected$ticker
  changes <- which(tickers[-1L] != head(tickers, -1L)) + 1L
  if (!length(changes)) {
    return(data.frame(
      from_ticker = character(),
      to_ticker = character(),
      switch_date = as.Date(character()),
      bridge_date = as.Date(character()),
      from_pu_close = numeric(),
      to_pu_close = numeric(),
      pu_ratio = numeric(),
      pu_diff = numeric(),
      from_rate_close = numeric(),
      to_rate_close = numeric(),
      rate_diff = numeric(),
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
    bridge <- .brf_di_find_bridge_data(per_ticker[[from_ticker]], per_ticker[[to_ticker]], switch_date)

    bridge_values <- c(
      from_pu = bridge$from_pu,
      to_pu = bridge$to_pu,
      from_rate = bridge$from_rate,
      to_rate = bridge$to_rate
    )
    invalid_bridge <- is.na(bridge$bridge_date) ||
      any(!is.finite(bridge_values)) ||
      bridge$from_pu <= 0 ||
      bridge$to_pu <= 0
    if (invalid_bridge) {
      stop(
        "Cannot roll DI continuous series from ", from_ticker,
        " to ", to_ticker, " on ", format(as.Date(switch_date)),
        ": no finite common rate/PU bridge exists on or before the switch date.",
        call. = FALSE
      )
    }

    pu_ratio <- bridge$to_pu / bridge$from_pu
    rate_diff <- bridge$to_rate - bridge$from_rate
    if (!is.finite(pu_ratio) || pu_ratio <= 0 || !is.finite(rate_diff)) {
      stop(
        "Cannot roll DI continuous series from ", from_ticker,
        " to ", to_ticker, " on ", format(as.Date(switch_date)),
        ": bridge adjustment is non-finite.",
        call. = FALSE
      )
    }

    schedule[[i]] <- data.frame(
      from_ticker = from_ticker,
      to_ticker = to_ticker,
      switch_date = switch_date,
      bridge_date = bridge$bridge_date,
      from_pu_close = bridge$from_pu,
      to_pu_close = bridge$to_pu,
      pu_ratio = pu_ratio,
      pu_diff = bridge$to_pu - bridge$from_pu,
      from_rate_close = bridge$from_rate,
      to_rate_close = bridge$to_rate,
      rate_diff = rate_diff,
      switch_position = pos,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, schedule)
}

.brf_di_prev_pu_close <- function(df, before_date) {
  if (is.null(df) || !nrow(df) || !("PU_close" %in% names(df))) {
    return(NA_real_)
  }
  df$date <- as.Date(df$date)
  subset <- df[df$date < as.Date(before_date), , drop = FALSE]
  if (!nrow(subset)) {
    return(NA_real_)
  }
  vals <- suppressWarnings(as.numeric(subset$PU_close))
  if (!length(vals)) {
    return(NA_real_)
  }
  idx <- tail(which(!is.na(vals)), 1L)
  if (length(idx)) {
    return(vals[idx])
  }
  NA_real_
}

.brf_di_compute_backtest_columns <- function(selected, per_ticker, roll_schedule) {
  n <- nrow(selected)
  if (!n || !("PU_close" %in% names(selected))) {
    return(NULL)
  }

  # Initialize P&L vectors
  pnl <- rep(NA_real_, n)
  cum_pnl <- rep(NA_real_, n)
  ret <- rep(NA_real_, n)
  idx <- rep(NA_real_, n)
  is_roll <- rep(FALSE, n)

  # Initialize unadjusted OHLC vectors for rates
  open_unadj <- rep(NA_real_, n)
  high_unadj <- rep(NA_real_, n)
  low_unadj <- rep(NA_real_, n)
  close_unadj <- rep(NA_real_, n)

  # Initialize unadjusted OHLC vectors for PU
  pu_open_unadj <- rep(NA_real_, n)
  pu_high_unadj <- rep(NA_real_, n)
  pu_low_unadj <- rep(NA_real_, n)
  pu_close_unadj <- rep(NA_real_, n)

  # Initialize adjusted OHLC vectors for rates (additive adjustment)
  open_adj <- rep(NA_real_, n)
  high_adj <- rep(NA_real_, n)
  low_adj <- rep(NA_real_, n)
  close_adj <- rep(NA_real_, n)

  # Initialize adjusted OHLC vectors for PU (multiplicative adjustment)
  pu_open_adj <- rep(NA_real_, n)
  pu_high_adj <- rep(NA_real_, n)
  pu_low_adj <- rep(NA_real_, n)
  pu_close_adj <- rep(NA_real_, n)

  # Mark roll positions
  if (nrow(roll_schedule)) {
    is_roll[roll_schedule$switch_position] <- TRUE
  }

  # Build cumulative adjustments for backward-adjusted series
  # PU: multiplicative ratio
  # Rates: additive difference
  cumulative_pu_ratio <- rep(1, n)
  cumulative_rate_diff <- rep(0, n)

  if (nrow(roll_schedule)) {
    for (r in seq_len(nrow(roll_schedule))) {
      pos <- roll_schedule$switch_position[r]
      pu_ratio <- roll_schedule$pu_ratio[r]
      rate_diff <- roll_schedule$rate_diff[r]

      if (pos > 1) {
        if (is.finite(pu_ratio)) {
          cumulative_pu_ratio[1:(pos - 1)] <- cumulative_pu_ratio[1:(pos - 1)] * pu_ratio
        }
        if (is.finite(rate_diff)) {
          cumulative_rate_diff[1:(pos - 1)] <- cumulative_rate_diff[1:(pos - 1)] + rate_diff
        }
      }
    }
  }

  # First day initialization
  pnl[1] <- 0
  cum_pnl[1] <- 0
  ret[1] <- 0
  idx[1] <- 100

  # Process all rows
  for (i in seq_len(n)) {
    # Store unadjusted values
    if ("open" %in% names(selected)) {
      open_unadj[i] <- suppressWarnings(as.numeric(selected$open[i]))
      open_adj[i] <- open_unadj[i] + cumulative_rate_diff[i]
    }
    if ("high" %in% names(selected)) {
      high_unadj[i] <- suppressWarnings(as.numeric(selected$high[i]))
      high_adj[i] <- high_unadj[i] + cumulative_rate_diff[i]
    }
    if ("low" %in% names(selected)) {
      low_unadj[i] <- suppressWarnings(as.numeric(selected$low[i]))
      low_adj[i] <- low_unadj[i] + cumulative_rate_diff[i]
    }
    if ("close" %in% names(selected)) {
      close_unadj[i] <- suppressWarnings(as.numeric(selected$close[i]))
      close_adj[i] <- close_unadj[i] + cumulative_rate_diff[i]
    }
    if ("PU_open" %in% names(selected)) {
      pu_open_unadj[i] <- suppressWarnings(as.numeric(selected$PU_open[i]))
      pu_open_adj[i] <- pu_open_unadj[i] * cumulative_pu_ratio[i]
    }
    if ("PU_high" %in% names(selected)) {
      pu_high_unadj[i] <- suppressWarnings(as.numeric(selected$PU_high[i]))
      pu_high_adj[i] <- pu_high_unadj[i] * cumulative_pu_ratio[i]
    }
    if ("PU_low" %in% names(selected)) {
      pu_low_unadj[i] <- suppressWarnings(as.numeric(selected$PU_low[i]))
      pu_low_adj[i] <- pu_low_unadj[i] * cumulative_pu_ratio[i]
    }
    if ("PU_close" %in% names(selected)) {
      pu_close_unadj[i] <- suppressWarnings(as.numeric(selected$PU_close[i]))
      pu_close_adj[i] <- pu_close_unadj[i] * cumulative_pu_ratio[i]
    }

    # Calculate P&L for rows after the first
    if (i > 1) {
      cur_price <- pu_close_unadj[i]

      if (selected$ticker[i] == selected$ticker[i - 1L]) {
        prev_price <- pu_close_unadj[i - 1L]
      } else {
        prev_price <- .brf_di_prev_pu_close(per_ticker[[selected$ticker[i]]], selected$date[i])
      }

      if (is.finite(cur_price) && is.finite(prev_price) && prev_price != 0) {
        pnl[i] <- cur_price - prev_price
        cum_pnl[i] <- cum_pnl[i - 1L] + pnl[i]
        ret[i] <- (cur_price - prev_price) / prev_price
        idx[i] <- idx[i - 1L] * (1 + ret[i])
      } else {
        pnl[i] <- NA_real_
        cum_pnl[i] <- cum_pnl[i - 1L]
        ret[i] <- NA_real_
        idx[i] <- idx[i - 1L]
      }
    }
  }

  list(
    pnl = pnl,
    cum_pnl = cum_pnl,
    ret = ret,
    idx = idx,
    is_roll = is_roll,
    # Unadjusted (raw) values
    open_unadj = open_unadj,
    high_unadj = high_unadj,
    low_unadj = low_unadj,
    close_unadj = close_unadj,
    pu_open_unadj = pu_open_unadj,
    pu_high_unadj = pu_high_unadj,
    pu_low_unadj = pu_low_unadj,
    pu_close_unadj = pu_close_unadj,
    # Adjusted (continuous) values
    open_adj = open_adj,
    high_adj = high_adj,
    low_adj = low_adj,
    close_adj = close_adj,
    pu_open_adj = pu_open_adj,
    pu_high_adj = pu_high_adj,
    pu_low_adj = pu_low_adj,
    pu_close_adj = pu_close_adj
  )
}

.brf_di_numeric_field <- function(data, field) {
  if (!(field %in% names(data))) {
    return(rep(NA_real_, nrow(data)))
  }
  suppressWarnings(as.numeric(data[[field]]))
}

.brf_di_logical_field <- function(data, field) {
  if (!(field %in% names(data))) {
    return(rep(NA_real_, nrow(data)))
  }
  value <- data[[field]]
  if (is.logical(value)) {
    return(as.numeric(value))
  }
  if (is.numeric(value)) {
    out <- rep(NA_real_, length(value))
    ok <- is.finite(value)
    out[ok] <- as.numeric(value[ok] != 0)
    return(out)
  }
  key <- tolower(trimws(as.character(value)))
  out <- rep(NA_real_, length(key))
  out[key %in% c("true", "t", "1", "yes")] <- 1
  out[key %in% c("false", "f", "0", "no")] <- 0
  out
}

.brf_di_row_contract_matrix <- function(selected, target_days) {
  cbind(
    ContractOrdinal = .brf_di_numeric_field(selected, "contract_ordinal"),
    ActualMaturity = as.numeric(as.Date(selected$maturity)),
    ValidDays = as.numeric(selected$valid_days),
    TenorDiff = as.numeric(selected$valid_days - target_days),
    RateOpenRaw = .brf_di_numeric_field(selected, "open"),
    RateHighRaw = .brf_di_numeric_field(selected, "high"),
    RateLowRaw = .brf_di_numeric_field(selected, "low"),
    RateCloseRaw = .brf_di_numeric_field(selected, "close"),
    PUOpenRaw = .brf_di_numeric_field(selected, "PU_open"),
    PUHighRaw = .brf_di_numeric_field(selected, "PU_high"),
    PULowRaw = .brf_di_numeric_field(selected, "PU_low"),
    PUCloseRaw = .brf_di_numeric_field(selected, "PU_close"),
    Settlement = .brf_di_numeric_field(selected, "settlement_price"),
    PreviousSettlement = .brf_di_numeric_field(selected, "previous_settlement"),
    AdjustmentBase = .brf_di_numeric_field(selected, "di_adjustment_base"),
    OfficialAdjustment = .brf_di_numeric_field(selected, "di_adjustment_points"),
    AdjustmentOfficial = .brf_di_logical_field(selected, "di_adjustment_is_official")
  )
}

.brf_di_selected_to_xts <- function(selected, target_days, include_pnl, per_ticker, roll_schedule) {
  n <- nrow(selected)
  row_contract <- .brf_di_row_contract_matrix(selected, target_days)

  if (isTRUE(include_pnl) && "PU_close" %in% names(selected)) {
    # Calculate all backtest columns including adjusted OHLC
    bt_info <- .brf_di_compute_backtest_columns(selected, per_ticker, roll_schedule)

    if (!is.null(bt_info)) {
      # Build matrix with adjusted values as primary OHLC columns
      mat <- cbind(
        # Adjusted rates as main OHLC
        Open = as.numeric(bt_info$open_adj),
        High = as.numeric(bt_info$high_adj),
        Low = as.numeric(bt_info$low_adj),
        Close = as.numeric(bt_info$close_adj),
        # Unadjusted rates
        Open_unadj = as.numeric(bt_info$open_unadj),
        High_unadj = as.numeric(bt_info$high_unadj),
        Low_unadj = as.numeric(bt_info$low_unadj),
        Close_unadj = as.numeric(bt_info$close_unadj),
        # Adjusted PU as main PU columns
        PU_open = as.numeric(bt_info$pu_open_adj),
        PU_high = as.numeric(bt_info$pu_high_adj),
        PU_low = as.numeric(bt_info$pu_low_adj),
        PU_close = as.numeric(bt_info$pu_close_adj),
        # Unadjusted PU
        PU_open_unadj = as.numeric(bt_info$pu_open_unadj),
        PU_high_unadj = as.numeric(bt_info$pu_high_unadj),
        PU_low_unadj = as.numeric(bt_info$pu_low_unadj),
        PU_close_unadj = as.numeric(bt_info$pu_close_unadj),
        # Volume
        Volume = if ("volume" %in% names(selected)) as.numeric(selected$volume) else rep(NA_real_, n),
        Volume_Qty = if ("volume_qty" %in% names(selected)) as.numeric(selected$volume_qty) else rep(NA_real_, n),
        # Tick info
        TickSize = if ("TickSize" %in% names(selected)) as.numeric(selected$TickSize) else rep(NA_real_, n),
        TickValue = if ("TickValue" %in% names(selected)) as.numeric(selected$TickValue) else rep(NA_real_, n),
        # Row-level executable contract identity and raw/official fields
        row_contract,
        # P&L columns
        PU_pnl = as.numeric(bt_info$pnl),
        PU_cum_pnl = as.numeric(bt_info$cum_pnl),
        PU_return = as.numeric(bt_info$ret),
        PU_index = as.numeric(bt_info$idx),
        IsRoll = as.integer(bt_info$is_roll)
      )

      # Optional transport columns that do not exist in the source may be
      # dropped, but the explicit contract/raw/official contract is stable
      # even when a particular official field is all NA for the requested
      # period.
      all_na <- apply(mat, 2, function(x) all(is.na(x)))
      required <- colnames(row_contract)
      mat <- mat[, !(all_na & !(colnames(mat) %in% required)), drop = FALSE]

      return(xts::xts(mat, order.by = selected$date))
    }
  }

  # Fallback: if include_pnl is FALSE or no PU_close, just return raw data
  col_map <- c(
    Open = "open",
    High = "high",
    Low = "low",
    Close = "close",
    Volume = "volume",
    Volume_Qty = "volume_qty",
    PU_open = "PU_open",
    PU_high = "PU_high",
    PU_low = "PU_low",
    PU_close = "PU_close",
    TickSize = "TickSize",
    TickValue = "TickValue"
  )
  xts_cols <- lapply(col_map, function(src) {
    if (src %in% names(selected)) {
      vals <- selected[[src]]
      if (!is.numeric(vals)) {
        suppressWarnings(vals <- as.numeric(vals))
      }
      vals
    } else {
      NULL
    }
  })
  keep <- lengths(xts_cols) > 0
  if (!any(keep)) {
    stop("No usable columns found to build the DI continuous series.", call. = FALSE)
  }
  mat <- do.call(cbind, xts_cols[keep])
  colnames(mat) <- names(col_map)[keep]
  mat <- cbind(mat, row_contract)

  xts::xts(mat, order.by = selected$date)
}

.brf_di_build_single_tenor <- function(data,
                                       target_tenor,
                                       tenor_unit,
                                       root,
                                       allowed_maturities,
                                       cal,
                                       include_pnl,
                                       add_attrs,
                                       add_globalenv,
                                       strict_target,
                                       selection_mode,
                                       coverage_mode) {
  prepared <- .brf_di_prepare_continuous_data(
    data = data,
    root = root,
    allowed_maturities = allowed_maturities,
    cal = cal
  )
  df <- prepared$data
  target_days <- .brf_di_resolve_target_days(target_tenor, tenor_unit)
  selection_mode <- .brf_di_resolve_selection_mode(
    selection_mode,
    target_tenor,
    tenor_unit,
    prepared$allowed_maturities
  )
  if (identical(selection_mode, "calendar_horizon") && !isTRUE(strict_target)) {
    stop(
      "'strict_target = FALSE' is not supported with calendar_horizon selection.",
      call. = FALSE
    )
  }
  by_date <- split(df, df$date)
  trading_days <- sort(unique(df$date))
  selected_list <- list()
  minimum_maturity <- as.Date(NA)
  series_started <- FALSE
  gap_resets <- list()
  for (i in seq_along(trading_days)) {
    day <- trading_days[i]
    rows <- by_date[[as.character(day)]]
    selected_row <- .brf_di_pick_contract(
      rows,
      target_days,
      minimum_maturity = minimum_maturity,
      strict_target = strict_target,
      selection_mode = selection_mode,
      basis_date = day,
      target_tenor = target_tenor
    )
    if (!nrow(selected_row)) {
      if (!series_started && isTRUE(strict_target)) {
        next
      }
      if (identical(coverage_mode, "restart_strict_suffix")) {
        prior_minimum <- minimum_maturity
        selected_list <- list()
        minimum_maturity <- as.Date(NA)
        series_started <- FALSE
        selected_row <- .brf_di_pick_contract(
          rows,
          target_days,
          minimum_maturity = minimum_maturity,
          strict_target = strict_target,
          selection_mode = selection_mode,
          basis_date = day,
          target_tenor = target_tenor
        )
        gap_resets[[length(gap_resets) + 1L]] <- data.frame(
          gap_date = as.Date(day),
          prior_minimum_maturity = as.Date(prior_minimum),
          fresh_start_eligible = nrow(selected_row) > 0L,
          reset_reason = "no_eligible_monotonic_contract",
          stringsAsFactors = FALSE
        )
        if (!nrow(selected_row)) {
          next
        }
      } else {
        current_text <- if (is.na(minimum_maturity)) {
          "none"
        } else {
          format(minimum_maturity)
        }
        stop(
          "DI continuous series has no eligible monotonic contract on ",
          format(as.Date(day)), " after the series started; selection_mode=",
          selection_mode, ", target_days=", target_days,
          ", minimum_maturity=", current_text,
          ", strict_target=", isTRUE(strict_target), ".",
          call. = FALSE
        )
      }
    }
    selected_list[[length(selected_list) + 1L]] <- selected_row
    minimum_maturity <- as.Date(selected_row$maturity[[1L]])
    series_started <- TRUE
  }
  if (!length(selected_list)) {
    if (identical(selection_mode, "strict_du_floor")) {
      stop(
        "No DI contract at or above target_days=", target_days,
        " was available in the requested period.",
        call. = FALSE
      )
    }
    stop(
      "No January DI contract at the requested calendar horizon was available in the requested period; target_tenor=",
      target_tenor, " years.",
      call. = FALSE
    )
  }
  selected <- do.call(rbind, selected_list)
  rownames(selected) <- NULL
  initial_dates_dropped <- trading_days[trading_days < min(selected$date)]
  gap_resets <- if (length(gap_resets)) {
    do.call(rbind, gap_resets)
  } else {
    data.frame(
      gap_date = as.Date(character()),
      prior_minimum_maturity = as.Date(character()),
      fresh_start_eligible = logical(),
      reset_reason = character(),
      stringsAsFactors = FALSE
    )
  }
  maturity_steps <- diff(as.numeric(as.Date(selected$maturity)))
  if (any(maturity_steps < 0, na.rm = TRUE)) {
    stop("Internal DI continuous invariant failed: maturity moved backward.", call. = FALSE)
  }
  coverage_start <- min(selected$date)
  coverage_df <- df[df$date >= coverage_start, , drop = FALSE]
  per_ticker <- split(coverage_df, coverage_df$ticker)
  roll_schedule <- .brf_di_roll_schedule(selected, per_ticker)
  series <- .brf_di_selected_to_xts(selected, target_days, include_pnl, per_ticker, roll_schedule)
  # Force POSIXct index in America/Sao_Paulo to mirror build_continuous
  tz_out <- "America/Sao_Paulo"
  new_index <- lubridate::force_tz(zoo::index(series), tz_out)
  attr(series, "index") <- as.numeric(new_index)
  attr(series, "tzone") <- tz_out
  attr(series, ".indexCLASS") <- c("POSIXct", "POSIXt")
  attr(series, ".indexTZ") <- tz_out
  attr(attr(series, "index"), "tclass") <- c("POSIXct", "POSIXt")
  attr(attr(series, "index"), "tzone") <- tz_out
  cal_use <- .brf_di_resolve_calendar(cal)
  last_basis <- max(selected$date, na.rm = TRUE)
  est_maturity <- if (identical(selection_mode, "calendar_horizon")) {
    as.Date(tail(selected$maturity, 1L))
  } else {
    bizdays::add.bizdays(last_basis, target_days, cal_use)
  }
  continuous_spec <- list(
    method = if (identical(selection_mode, "calendar_horizon")) {
      "di_calendar_horizon"
    } else {
      "di_constant_tenor"
    },
    root = prepared$root,
    target_tenor = target_tenor,
    target_days = target_days,
    tenor_unit = tenor_unit,
    allowed_maturities = prepared$allowed_maturities,
    strict_target = strict_target,
    selection_mode = selection_mode,
    selection_version = .brf_di_selection_version(selection_mode, strict_target),
    roll_trigger = "contract_symbol_change_on_observed_session",
    coverage_mode = coverage_mode,
    coverage_start = coverage_start,
    coverage_end = max(selected$date),
    gap_resets = gap_resets,
    include_pnl = include_pnl,
    pnl_basis = if (isTRUE(include_pnl)) "approximate_pu_mark" else NULL,
    initial_dates_dropped = initial_dates_dropped,
    actual_maturity_column = "ActualMaturity",
    actual_maturity_encoding = "days_since_1970-01-01",
    contract_ordinal_column = "ContractOrdinal"
  )
  active_contracts <- data.frame(
    date = selected$date,
    root = selected$root,
    ticker = selected$ticker,
    contract_symbol = selected$ticker,
    contract_ordinal = selected$contract_ordinal,
    actual_maturity = selected$maturity,
    valid_days = selected$valid_days,
    month_code = selected$month_code,
    stringsAsFactors = FALSE
  )
  if (nrow(roll_schedule)) {
    roll_export <- roll_schedule
    roll_export$switch_position <- NULL
  } else {
    roll_export <- roll_schedule
  }
  # attr(series, "renda") <- "Trading"
  #  attr(series, "categoria") <- "Futuro"
  # attr(series, "subcategoria") <- "Juros Brasil"
  #  attr(series, "risk_parity") <- "Azul"
  #  attr(series, "fees") <- 10
  #  attr(series, "slippage") <- 0.01
  #  attr(series, "multiplier") <- \1
  # attr(series, "fonte") <- "Obter_b3_fut_cont"
  series_name <- paste0("DI1FUT_1D_", target_tenor, substr(toupper(tenor_unit), 1, 1), "BR_B")
  # Mark the object before generic futures enrichment so DI metadata does not
  # recompute already-correct row-level PU with a scalar estimated maturity.
  attr(series, "continuous_spec") <- continuous_spec
  if (isTRUE(add_attrs)) {
    series <- .brf_add_futures_attrs(series, series_name)
  }
  # Re-attach the continuous contract after enrichment/cbind operations.
  attr(series, "continuous_spec") <- continuous_spec
  attr(series, "active_contracts") <- active_contracts
  attr(series, "contract_map") <- prepared$contract_map
  attr(series, "roll_schedule") <- roll_export
  # Kept for backwards compatibility only. Row-level execution must use
  # ActualMaturity plus contract_map/active_contracts, not this estimate.
  attr(series, "maturity") <- est_maturity
  attr(series, "PU_pnl_is_approximate") <- isTRUE(include_pnl)
  if (isTRUE(add_globalenv)) {
    assign(series_name, series, envir = .GlobalEnv)
  }
  return(series)
}
