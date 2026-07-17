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
#'   columns.
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
    "di_adjustment_is_official"
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
