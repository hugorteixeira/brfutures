#' Load official DI1 daily adjustment data
#'
#' This helper returns B3 settlement fields in a DI1-safe shape. Legacy HTML
#' rows normally use the exchange-reported `change_points`. Between
#' 2018-09-24 and 2019-06-28, however, the legacy bulletin exposed the
#' uncorrected prior settlement in `previous_settlement` and the carry-adjusted
#' current base in `corrected_settlement`; that interval is normalized
#' separately. When `change_points` is absent, the adjustment is
#' `settlement_price - previous_settlement`.
#'
#' @param ticker Optional DI1 ticker vector such as `"DI1F26"`.
#' @inheritParams get_brfut_agg
#' @param as_tibble Logical; when `TRUE`, return a tibble.
#'
#' @return A data frame with official settlement fields and DI adjustment
#'   columns. Rows whose legacy B3 bulletin reported an all-zero OHLC despite
#'   proven trading can also carry versioned `ohlc_repair_*` provenance.
#' @export
get_brfut_di_adjustments <- function(ticker = NULL,
                                     start = NULL,
                                     end = NULL,
                                     rebuild_agg = FALSE,
                                     as_tibble = FALSE) {
  out <- get_brfut_agg(
    start = start,
    end = end,
    root = "DI1",
    treatment = "di_adjustments",
    rebuild_agg = rebuild_agg
  )

  if (!is.null(ticker)) {
    ticker_norm <- toupper(trimws(as.character(ticker)))
    ticker_norm <- ticker_norm[nzchar(ticker_norm)]
    if (length(ticker_norm)) {
      out <- out[out$ticker %in% ticker_norm, , drop = FALSE]
    }
  }

  keep <- intersect(c(
    "date",
    "root",
    "contract_code",
    "ticker",
    "source",
    "maturity",
    "open",
    "low",
    "high",
    "close",
    "settlement_price",
    "previous_settlement",
    "corrected_settlement",
    "change_points",
    "di_adjustment_base",
    "di_adjustment_points",
    "di_adjustment_quality",
    "di_adjustment_is_official",
    "di_adjustment_available_at",
    "adjustment_available_at",
    "publication_timestamp",
    "published_at",
    "ohlc_repaired",
    "ohlc_repair_method",
    "ohlc_repair_status",
    "ohlc_repair_source_contracts",
    "ohlc_repair_neighbor_mode",
    "ohlc_repair_prior_session_date",
    "ohlc_original_open",
    "ohlc_original_high",
    "ohlc_original_low",
    "ohlc_original_close"
  ), names(out))
  out <- out[, keep, drop = FALSE]

  if (isTRUE(as_tibble)) {
    return(tibble::as_tibble(out))
  }
  out
}

#' Normalize official DI1 settlement and adjustment fields
#'
#' Applies the historical B3 bulletin-regime rules to a complete cross-section
#' of DI1 rows. The function is pure: it does not read or update the package
#' cache. Keep every maturity for each supplied session so the bounded legacy
#' corrected-base regime can establish its cross-sectional carry factor and
#' fail closed when no consensus exists.
#'
#' @param data A data frame containing raw or standardised B3 futures bulletin
#'   rows, including `date`, contract identity, `settlement_price`,
#'   `previous_settlement`, `corrected_settlement`, `change_points`, and
#'   `source` when available. When `source` is absent, rows inside the bounded
#'   2018-09-24 through 2019-06-28 interval are inferred to use the observed
#'   legacy HTML layout; an explicit non-HTML source is never reclassified.
#' @return `data` with canonical `di_adjustment_base`,
#'   `di_adjustment_points`, `di_adjustment_quality`, and
#'   `di_adjustment_is_official` columns.
#' @export
normalize_brfut_di_adjustments <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  .brf_add_di_adjustment_columns(data)
}

.brf_di_ohlc_repair_method <-
  "prior_session_cross_sectional_vwap_settlement_v1"

.brf_initialize_di_ohlc_repair_columns <- function(df) {
  n <- nrow(df)
  if (!"ohlc_repaired" %in% names(df)) {
    df$ohlc_repaired <- rep(FALSE, n)
  } else {
    df$ohlc_repaired <- as.logical(df$ohlc_repaired)
    df$ohlc_repaired[is.na(df$ohlc_repaired)] <- FALSE
  }

  text_cols <- c(
    "ohlc_repair_method",
    "ohlc_repair_status",
    "ohlc_repair_source_contracts",
    "ohlc_repair_neighbor_mode"
  )
  for (col in text_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- rep(NA_character_, n)
    } else {
      df[[col]] <- as.character(df[[col]])
    }
  }

  if (!"ohlc_repair_prior_session_date" %in% names(df)) {
    df$ohlc_repair_prior_session_date <- as.Date(rep(NA_character_, n))
  } else {
    df$ohlc_repair_prior_session_date <- suppressWarnings(
      as.Date(df$ohlc_repair_prior_session_date)
    )
  }

  original_cols <- paste0("ohlc_original_", c("open", "high", "low", "close"))
  for (col in original_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- rep(NA_real_, n)
    } else {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  df
}

.brf_first_numeric_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) {
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[hit[[1L]]]]))
}

.brf_repair_di_traded_zero_ohlc <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }
  df <- .brf_initialize_di_ohlc_repair_columns(df)

  required <- c(
    "date", "ticker", "maturity", "open", "high", "low", "close",
    "average_price", "trade_count", "settlement_price"
  )
  if (length(setdiff(required, names(df)))) {
    return(df)
  }

  observed <- df
  dates <- suppressWarnings(as.Date(observed$date))
  maturities <- suppressWarnings(as.Date(observed$maturity))
  tickers <- toupper(trimws(as.character(observed$ticker)))
  roots <- if ("root" %in% names(observed)) {
    toupper(trimws(as.character(observed$root)))
  } else {
    rep(NA_character_, nrow(observed))
  }
  sources <- if ("source" %in% names(observed)) {
    tolower(trimws(as.character(observed$source)))
  } else {
    rep(NA_character_, nrow(observed))
  }
  is_di1 <- roots == "DI1" | startsWith(tickers, "DI1")
  is_di1[is.na(is_di1)] <- FALSE

  rate_cols <- c("open", "high", "low", "close")
  rates <- do.call(cbind, lapply(rate_cols, function(col) {
    suppressWarnings(as.numeric(observed[[col]]))
  }))
  average_price <- suppressWarnings(as.numeric(observed$average_price))
  trade_count <- suppressWarnings(as.numeric(observed$trade_count))
  volume_qty <- .brf_first_numeric_col(
    observed,
    c("volume_qty", "contracts_traded", "contr_negoc", "contract_negoc")
  )
  settlement <- suppressWarnings(as.numeric(observed$settlement_price))
  last_bid <- .brf_numeric_col(observed, "last_bid")
  last_ask <- .brf_numeric_col(observed, "last_ask")

  # This is deliberately narrower than a generic missing-data fill. A row is
  # eligible only when the legacy HTML bulletin itself proves that the contract
  # traded, reports a VWAP and settlement PU, yet publishes four literal zeros
  # for OHLC. Rows with no trades, partial fields, or non-HTML sources fail
  # closed and remain untouched.
  literal_zero_ohlc <- rowSums(is.finite(rates) & rates == 0) == length(rate_cols)
  targets <- which(
    is_di1 & sources == "html" & literal_zero_ohlc &
      is.finite(trade_count) & trade_count > 0 &
      is.finite(volume_qty) & volume_qty > 0 &
      is.finite(average_price) & average_price > 0 &
      is.finite(settlement) & settlement > 0 &
      !is.na(dates) & !is.na(maturities) & maturities > dates
  )
  if (!length(targets)) {
    return(df)
  }

  valid_observed_quote <-
    rowSums(is.finite(rates) & rates > 0) == length(rate_cols) &
    is.finite(average_price) & average_price > 0 &
    is.finite(trade_count) & trade_count > 0 &
    is.finite(volume_qty) & volume_qty > 0 &
    !is.na(dates) & !is.na(maturities)

  session_cal <- tryCatch(
    .brf_di_resolve_session_calendar(),
    error = function(e) NULL
  )
  financial_cal <- tryCatch(
    .brf_di_resolve_calendar(),
    error = function(e) NULL
  )
  if (is.null(session_cal) || is.null(financial_cal)) {
    return(df)
  }

  for (target in targets) {
    prior_date <- tryCatch(
      bizdays::add.bizdays(dates[[target]], -1L, session_cal),
      error = function(e) as.Date(NA)
    )
    if (is.na(prior_date)) {
      next
    }

    prior_target <- which(
      tickers == tickers[[target]] & dates == prior_date &
        maturities == maturities[[target]] & valid_observed_quote
    )
    if (length(prior_target) != 1L) {
      next
    }
    prior_target <- prior_target[[1L]]

    minimum_trades <- max(10, ceiling(trade_count[[target]] * 0.01))
    current_candidates <- which(
      is_di1 & dates == dates[[target]] &
        tickers != tickers[[target]] & valid_observed_quote &
        trade_count >= minimum_trades
    )
    if (!length(current_candidates)) {
      next
    }
    current_candidates <- current_candidates[!duplicated(tickers[current_candidates])]

    prior_candidates <- which(is_di1 & dates == prior_date & valid_observed_quote)
    prior_match <- match(tickers[current_candidates], tickers[prior_candidates])
    shared <- !is.na(prior_match)
    if (!any(shared)) {
      next
    }
    current_candidates <- current_candidates[shared]
    prior_candidates <- prior_candidates[prior_match[shared]]
    same_maturity <- maturities[current_candidates] == maturities[prior_candidates]
    same_maturity[is.na(same_maturity)] <- FALSE
    current_candidates <- current_candidates[same_maturity]
    prior_candidates <- prior_candidates[same_maturity]
    if (length(current_candidates) < 2L) {
      next
    }

    target_maturity_num <- as.numeric(maturities[[target]])
    candidate_maturity_num <- as.numeric(maturities[current_candidates])
    lower <- which(candidate_maturity_num < target_maturity_num)
    upper <- which(candidate_maturity_num > target_maturity_num)
    if (length(lower) && length(upper)) {
      chosen <- c(
        lower[[which.max(candidate_maturity_num[lower])]],
        upper[[which.min(candidate_maturity_num[upper])]]
      )
      neighbor_mode <- "bracket_interpolation"
    } else {
      chosen <- order(abs(candidate_maturity_num - target_maturity_num))[seq_len(2L)]
      neighbor_mode <- "one_sided_extrapolation"
    }
    chosen <- chosen[order(candidate_maturity_num[chosen])]
    current_neighbors <- current_candidates[chosen]
    prior_neighbors <- prior_candidates[chosen]
    x <- candidate_maturity_num[chosen]
    if (length(unique(x)) != 2L) {
      next
    }
    weight <- (target_maturity_num - x[[1L]]) / (x[[2L]] - x[[1L]])
    # Permit only a short, two-neighbour extrapolation. The longest historical
    # eligible case is DI1F31 on 2021-06-10, bracketed one-sided by F28/F29.
    if (!is.finite(weight) || weight < -2.5 || weight > 3.5) {
      next
    }

    interpolate_move <- function(values) {
      moves <- values[current_neighbors] - values[prior_neighbors]
      moves[[1L]] + weight * (moves[[2L]] - moves[[1L]])
    }
    average_move <- interpolate_move(average_price)
    predicted_average <- average_price[[prior_target]] + average_move
    calibration <- average_price[[target]] - predicted_average
    modelled <- vapply(seq_along(rate_cols), function(field_index) {
      values <- rates[, field_index]
      values[[prior_target]] + interpolate_move(values) + calibration
    }, numeric(1))
    names(modelled) <- rate_cols

    valid_days <- tryCatch(
      bizdays::bizdays(dates[[target]], maturities[[target]], financial_cal),
      error = function(e) NA_real_
    )
    settlement_rate <- .brf_di_rate_from_pu(settlement[[target]], valid_days)
    month_bucket <- .brf_di_months_between_floor(
      dates[[target]],
      maturities[[target]]
    )
    tick_size <- .brf_di_get_tick_size(month_bucket, dates[[target]])
    if (!is.finite(valid_days) || valid_days <= 0 ||
        !is.finite(settlement_rate) || settlement_rate <= 0 ||
        !is.finite(tick_size) || tick_size <= 0 ||
        any(!is.finite(modelled)) || any(modelled <= 0) ||
        max(abs(c(modelled, settlement_rate) - average_price[[target]])) > 2.5) {
      next
    }

    snap_nearest <- function(value) round(value / tick_size) * tick_size
    snap_floor <- function(value) floor(value / tick_size) * tick_size
    snap_ceiling <- function(value) ceiling(value / tick_size) * tick_size
    repaired_open <- snap_nearest(modelled[["open"]])
    repaired_close <- snap_nearest(settlement_rate)
    range_anchors <- c(
      repaired_open,
      repaired_close,
      average_price[[target]],
      last_bid[[target]],
      last_ask[[target]]
    )
    range_anchors <- range_anchors[is.finite(range_anchors) & range_anchors > 0]
    repaired_high <- max(
      snap_nearest(modelled[["high"]]),
      snap_ceiling(max(range_anchors))
    )
    repaired_low <- min(
      snap_nearest(modelled[["low"]]),
      snap_floor(min(range_anchors))
    )
    if (!all(is.finite(c(repaired_open, repaired_high, repaired_low, repaired_close))) ||
        repaired_low <= 0 || repaired_high < max(repaired_open, repaired_close) ||
        repaired_low > min(repaired_open, repaired_close)) {
      next
    }

    for (field in rate_cols) {
      df[[paste0("ohlc_original_", field)]][[target]] <-
        suppressWarnings(as.numeric(observed[[field]][[target]]))
    }
    df$open[[target]] <- repaired_open
    df$high[[target]] <- repaired_high
    df$low[[target]] <- repaired_low
    df$close[[target]] <- repaired_close
    df$ohlc_repaired[[target]] <- TRUE
    df$ohlc_repair_method[[target]] <- .brf_di_ohlc_repair_method
    df$ohlc_repair_status[[target]] <- "modelled_traded_bulletin_zero_ohlc"
    df$ohlc_repair_source_contracts[[target]] <- paste(
      tickers[current_neighbors],
      collapse = ","
    )
    df$ohlc_repair_neighbor_mode[[target]] <- neighbor_mode
    df$ohlc_repair_prior_session_date[[target]] <- prior_date
  }
  df
}

.brf_repair_di_settlement_scale <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }

  root <- if ("root" %in% names(df)) toupper(as.character(df$root)) else rep(NA_character_, nrow(df))
  ticker <- if ("ticker" %in% names(df)) toupper(as.character(df$ticker)) else rep(NA_character_, nrow(df))
  contract_code <- if ("contract_code" %in% names(df)) toupper(as.character(df$contract_code)) else rep(NA_character_, nrow(df))
  is_di1 <- root == "DI1" | startsWith(ticker, "DI1") | startsWith(contract_code, "DI1")
  is_di1[is.na(is_di1)] <- FALSE

  repair_one <- function(target_col, reference_cols) {
    if (!target_col %in% names(df)) {
      return(invisible(NULL))
    }
    value <- suppressWarnings(as.numeric(df[[target_col]]))
    reference <- rep(NA_real_, nrow(df))
    for (reference_col in reference_cols) {
      if (!reference_col %in% names(df)) {
        next
      }
      candidate <- suppressWarnings(as.numeric(df[[reference_col]]))
      fill <- (!is.finite(reference) | reference <= 0) & is.finite(candidate) & candidate > 0
      reference[fill] <- candidate[fill]
    }

    ratio <- value / reference
    power <- suppressWarnings(round(log10(ratio)))
    divisor <- 10^power
    repaired <- value / divisor
    repaired_ratio <- repaired / reference
    fix <- is_di1 &
      is.finite(value) & value > 0 &
      is.finite(reference) & reference > 0 &
      is.finite(ratio) & ratio >= 5 &
      is.finite(power) & power >= 1 & power <= 3 &
      is.finite(repaired_ratio) & repaired_ratio >= 0.5 & repaired_ratio <= 2
    value[fix] <- repaired[fix]
    df[[target_col]] <<- value
    invisible(NULL)
  }

  # Settlement fields for the same DI contract and date must be on the same PU
  # scale. This repairs source typos such as the B3 HTML bulletin of 2018-05-25,
  # where every corrected settlement was published ten times too large, while
  # leaving legitimate values slightly above 100,000 near maturity untouched.
  repair_one("previous_settlement", c("settlement_price", "corrected_settlement"))
  repair_one("corrected_settlement", c("settlement_price", "previous_settlement"))
  repair_one("settlement_price", c("previous_settlement", "corrected_settlement"))
  df
}

.brf_add_di_adjustment_columns <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }

  df <- .brf_repair_di_settlement_scale(df)

  root <- if ("root" %in% names(df)) toupper(as.character(df$root)) else rep(NA_character_, nrow(df))
  ticker <- if ("ticker" %in% names(df)) toupper(as.character(df$ticker)) else rep(NA_character_, nrow(df))
  contract_code <- if ("contract_code" %in% names(df)) toupper(as.character(df$contract_code)) else rep(NA_character_, nrow(df))
  is_di1 <- root == "DI1" | startsWith(ticker, "DI1") | startsWith(contract_code, "DI1")
  is_di1[is.na(is_di1)] <- FALSE

  settlement <- .brf_numeric_col(df, "settlement_price")
  previous <- .brf_numeric_col(df, "previous_settlement")
  corrected <- .brf_numeric_col(df, "corrected_settlement")
  reported_points <- .brf_numeric_col(df, "change_points")
  dates <- if ("date" %in% names(df)) {
    suppressWarnings(as.Date(df$date))
  } else {
    rep(as.Date(NA), nrow(df))
  }
  source <- if ("source" %in% names(df)) {
    tolower(trimws(as.character(df$source)))
  } else {
    rep(NA_character_, nrow(df))
  }

  has_settlement <- is_di1 & is.finite(settlement)
  has_previous <- is_di1 & is.finite(previous) & previous > 0
  has_reported_points <- is_di1 & is.finite(reported_points)

  # The legacy HTML bulletin changed the row meaning without changing the
  # column labels. In this bounded interval, `previous_settlement` is the raw
  # prior PAt and `corrected_settlement` is the carry-adjusted base used by the
  # current variation margin. `change_points` remains the raw PAt change and
  # must not be treated as variation margin.
  legacy_html_layout <- source == "html" | is.na(source) | !nzchar(source)
  corrected_base_regime <- is_di1 & legacy_html_layout &
    !is.na(dates) & dates >= as.Date("2018-09-24") &
    dates <= as.Date("2019-06-28")
  corrected_base_regime[is.na(corrected_base_regime)] <- FALSE
  corrected_base <- .brf_di_corrected_regime_base(
    eligible = corrected_base_regime,
    settlement = settlement,
    previous = previous,
    corrected = corrected,
    dates = dates
  )
  has_corrected_base <- corrected_base_regime &
    is.finite(corrected_base) & corrected_base > 0

  base <- rep(NA_real_, nrow(df))
  base[has_corrected_base] <- corrected_base[has_corrected_base]

  # Outside the bounded corrected-base regime, B3's reported point variation
  # is authoritative. Deriving the base from the same row both preserves that
  # reported value and repairs isolated malformed previous-settlement fields.
  reported_regime <- !corrected_base_regime & has_settlement & has_reported_points
  base[reported_regime] <- settlement[reported_regime] - reported_points[reported_regime]
  previous_regime <- !corrected_base_regime & !has_reported_points & has_previous
  base[previous_regime] <- previous[previous_regime]

  points <- rep(NA_real_, nrow(df))
  points[has_corrected_base & has_settlement] <-
    settlement[has_corrected_base & has_settlement] -
    corrected_base[has_corrected_base & has_settlement]
  points[reported_regime] <- reported_points[reported_regime]
  calculate_points <- previous_regime & has_settlement
  points[calculate_points] <- settlement[calculate_points] - previous[calculate_points]

  quality <- rep(NA_character_, nrow(df))
  quality[has_corrected_base & has_settlement] <-
    "official_corrected_adjusted_quote"
  quality[reported_regime] <- "official_reported_change_points"
  quality[calculate_points] <- "official_previous_adjusted_quote"
  quality[is_di1 & !has_settlement] <- "missing_settlement"
  quality[is_di1 & has_settlement & !is.finite(base)] <- "missing_base"

  df$di_adjustment_base <- ifelse(is_di1, base, NA_real_)
  df$di_adjustment_points <- points
  df$di_adjustment_quality <- quality
  df$di_adjustment_is_official <- quality %in% c(
    "official_reported_change_points",
    "official_previous_adjusted_quote",
    "official_corrected_adjusted_quote"
  )
  df
}

.brf_di_corrected_regime_base <- function(eligible,
                                          settlement,
                                          previous,
                                          corrected,
                                          dates) {
  out <- rep(NA_real_, length(eligible))
  eligible <- eligible & is.finite(settlement) & settlement > 0 &
    is.finite(previous) & previous > 0 &
    is.finite(corrected) & corrected > 0
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(out)
  }

  # A few contract-launch and expiry rows in this historical layout put the
  # next-session corrected quote in the same column. The daily DI carry factor
  # is common to every maturity, so recover it deterministically from the
  # cross-sectional median and use the raw corrected value whenever it agrees.
  # Normal package flows retain the full DI1 bulletin cross-section. With fewer
  # than three usable maturities there is no safe consensus and the row fails
  # closed instead of guessing which semantic variant was supplied.
  date_key <- as.character(dates)
  for (key in unique(date_key[eligible])) {
    idx <- which(eligible & date_key == key)
    ratios <- corrected[idx] / previous[idx]
    plausible <- is.finite(ratios) & ratios > 0.99 & ratios < 1.01
    plausible_ratios <- ratios[plausible]
    if (length(plausible_ratios) < 3L) {
      next
    }
    factor_seed <- stats::median(plausible_ratios)
    if (!is.finite(factor_seed) || factor_seed <= 0) {
      next
    }

    # The bulletin values are rounded to cents. A real common carry factor
    # therefore produces a tight PU cluster, while the historical layout
    # outliers are separated by multiple PU points. Three observations alone
    # are not consensus: require at least three cent-level inliers and a strict
    # majority of every otherwise eligible maturity on the session.
    seed_error <- abs(corrected[idx] - previous[idx] * factor_seed)
    consensus <- plausible & is.finite(seed_error) & seed_error <= 0.02
    if (sum(consensus) < 3L || 2L * sum(consensus) <= length(idx)) {
      next
    }
    factor <- stats::median(ratios[consensus])
    final_error <- abs(corrected[idx] - previous[idx] * factor)
    consensus <- plausible & is.finite(final_error) & final_error <= 0.02
    if (!is.finite(factor) || factor <= 0 ||
        sum(consensus) < 3L || 2L * sum(consensus) <= length(idx)) {
      next
    }
    factor <- stats::median(ratios[consensus])
    expected <- previous[idx] * factor
    raw_matches <- abs(corrected[idx] - expected) <= 0.10
    out[idx[raw_matches]] <- corrected[idx[raw_matches]]

    fallback <- idx[!raw_matches]
    if (length(fallback)) {
      out[fallback] <- round(previous[fallback] * factor, 2)
    }
  }
  out
}

.brf_numeric_col <- function(df, col) {
  if (!col %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[col]]))
}
