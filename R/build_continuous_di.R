#' Build DI futures continuous series at a constant time-to-maturity
#'
#' DI futures are quoted in annualized rates, but daily variation margin is
#' applied to the contract's notional (PU). This helper stitches a continuous
#' DI series by always selecting the contract whose business-days-to-maturity
#' is closest **above** a target horizon (e.g. 1 year ??? 252 business days).
#' It augments the selected rows with PU columns so the resulting object can be
#' used directly for backtesting in notional terms.
#'
#' @param data Data frame returned by `get_brfut_agg()` (must contain `date`,
#'   `root`, `ticker`, `maturity`, and OHLC rate columns).
#' @param target_tenor Desired time-to-maturity. Interpreted according to
#'   `tenor_unit`. Accepts a numeric scalar or vector (e.g. `c(1, 3)`).
#' @param tenor_unit Unit for `target_tenor`: `"years"` (default), `"months"`,
#'   or `"business_days"`.
#' @param root Futures root to keep (defaults to `"DI1"`).
#' @param allowed_maturities Either `"all"` (default) or a character vector of
#'   month codes (e.g. `c("F", "G", "H")`) to restrict eligible contracts.
#' @param cal Optional `bizdays` calendar; defaults to the ANBIMA calendar used
#'   by the DI helpers.
#' @param include_pnl When `TRUE`, adds P&L, return, adjusted OHLC, and index columns.
#' @return An `xts` object when a single tenor is supplied, or a named list of
#'   `xts` objects when `target_tenor` has length > 1. Attributes include the
#'   roll schedule and active contracts.
#' @export
build_continuous_di <- function(data,
                                target_tenor = 1,
                                tenor_unit = c("years", "months", "business_days"),
                                root = "DI1",
                                allowed_maturities = "all",
                                cal = NULL,
                                include_pnl = TRUE) {
  .brf_di_require_bizdays()
  tenor_unit <- match.arg(tenor_unit)
  if (length(target_tenor) > 1L) {
    series_list <- lapply(target_tenor, function(tenor_val) {
      .brf_di_build_single_tenor(
        data = data,
        target_tenor = tenor_val,
        tenor_unit = tenor_unit,
        root = root,
        allowed_maturities = allowed_maturities,
        cal = cal,
        include_pnl = include_pnl
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
    include_pnl = include_pnl
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
  if (!("PU_close" %in% names(data))) {
    data <- .brf_di_add_pu_columns(data)
  }
  cal <- .brf_di_resolve_calendar(cal)
  data$valid_days <- .brf_di_safe_valid_days(data$date, data$maturity, cal, include_basis_day = TRUE)
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
  list(
    data = data,
    root = root_main,
    allowed_maturities = allowed
  )
}

.brf_di_safe_valid_days <- function(basis_date, maturity_date, cal, include_basis_day = TRUE) {
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

.brf_di_pick_contract <- function(rows, target_days) {
  if (!nrow(rows)) {
    return(rows)
  }
  rows <- rows[order(rows$valid_days, rows$maturity, rows$ticker), , drop = FALSE]
  forward <- rows[rows$valid_days >= target_days, , drop = FALSE]
  if (nrow(forward)) {
    return(forward[1, , drop = FALSE])
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

    pu_ratio <- if (is.finite(bridge$from_pu) && bridge$from_pu != 0) {
      bridge$to_pu / bridge$from_pu
    } else {
      NA_real_
    }

    rate_diff <- if (is.finite(bridge$from_rate) && is.finite(bridge$to_rate)) {
      bridge$to_rate - bridge$from_rate
    } else {
      NA_real_
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

.brf_di_selected_to_xts <- function(selected, target_days, include_pnl, per_ticker, roll_schedule) {
  n <- nrow(selected)

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
        # Tenor info
        ValidDays = as.numeric(selected$valid_days),
        TenorDiff = as.numeric(selected$valid_days - target_days),
        # P&L columns
        PU_pnl = as.numeric(bt_info$pnl),
        PU_cum_pnl = as.numeric(bt_info$cum_pnl),
        PU_return = as.numeric(bt_info$ret),
        PU_index = as.numeric(bt_info$idx),
        IsRoll = as.integer(bt_info$is_roll)
      )

      # Remove columns that are all NA
      all_na <- apply(mat, 2, function(x) all(is.na(x)))
      mat <- mat[, !all_na, drop = FALSE]

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
    TickValue = "TickValue",
    ValidDays = "valid_days"
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
  mat <- cbind(mat, TenorDiff = as.numeric(selected$valid_days - target_days))

  xts::xts(mat, order.by = selected$date)
}

.brf_di_build_single_tenor <- function(data,
                                       target_tenor,
                                       tenor_unit,
                                       root,
                                       allowed_maturities,
                                       cal,
                                       include_pnl) {
  prepared <- .brf_di_prepare_continuous_data(
    data = data,
    root = root,
    allowed_maturities = allowed_maturities,
    cal = cal
  )
  df <- prepared$data
  target_days <- .brf_di_resolve_target_days(target_tenor, tenor_unit)
  by_date <- split(df, df$date)
  trading_days <- sort(unique(df$date))
  selected_list <- vector("list", length(trading_days))
  for (i in seq_along(trading_days)) {
    day <- trading_days[i]
    rows <- by_date[[as.character(day)]]
    selected_list[[i]] <- .brf_di_pick_contract(rows, target_days)
  }
  selected <- do.call(rbind, selected_list)
  rownames(selected) <- NULL
  if (!nrow(selected)) {
    stop("Unable to determine an active contract for the selected period.", call. = FALSE)
  }
  per_ticker <- split(df, df$ticker)
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
  est_maturity <- bizdays::add.bizdays(last_basis, target_days - 1L, cal_use)
  attr(series, "continuous_spec") <- list(
    method = "di_constant_tenor",
    root = prepared$root,
    target_tenor = target_tenor,
    target_days = target_days,
    tenor_unit = tenor_unit,
    allowed_maturities = prepared$allowed_maturities,
    include_pnl = include_pnl
  )
  attr(series, "active_contracts") <- data.frame(
    date = selected$date,
    root = selected$root,
    ticker = selected$ticker,
    actual_maturity = selected$maturity,
    valid_days = selected$valid_days,
    month_code = selected$month_code,
    stringsAsFactors = FALSE
  )
  attr(series, "maturity") <- est_maturity
  if (nrow(roll_schedule)) {
    roll_export <- roll_schedule
    roll_export$switch_position <- NULL
    attr(series, "roll_schedule") <- roll_export
  } else {
    attr(series, "roll_schedule") <- roll_schedule
  }
  # attr(series, "renda") <- "Trading"
  #  attr(series, "categoria") <- "Futuro"
  # attr(series, "subcategoria") <- "Juros Brasil"
  #  attr(series, "risk_parity") <- "Azul"
  #  attr(series, "fut_fees") <- 10
  #  attr(series, "fut_slippage") <- 0.01
  #  attr(series, "fut_multiplier") <- \1
  # attr(series, "fonte") <- "Obter_b3_fut_cont"
  root <- paste0("DI1FUT_1D_", target_tenor, substr(toupper(tenor_unit), 1, 1), "BR_B")
  series <- .brf_add_futures_attrs(series, root)
  assign(root, series, envir = .GlobalEnv)
  return(series)
}
