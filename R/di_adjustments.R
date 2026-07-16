#' Load official DI1 daily adjustment data
#'
#' This helper returns B3 settlement fields in a DI1-safe shape. Legacy HTML
#' rows use the exchange-reported `change_points`; when that field is absent,
#' the adjustment is `settlement_price - previous_settlement`. The same-row
#' `corrected_settlement` is the same-row carry-corrected settlement reference
#' and is never the base of the current day's adjustment.
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
  reported_points <- .brf_numeric_col(df, "change_points")

  has_settlement <- is_di1 & is.finite(settlement)
  has_previous <- is_di1 & is.finite(previous) & previous > 0
  has_reported_points <- is_di1 & is.finite(reported_points)

  base <- rep(NA_real_, nrow(df))
  base[has_previous] <- previous[has_previous]
  derive_base <- !has_previous & has_settlement & has_reported_points
  base[derive_base] <- settlement[derive_base] - reported_points[derive_base]

  points <- rep(NA_real_, nrow(df))
  points[has_reported_points] <- reported_points[has_reported_points]
  calculate_points <- !has_reported_points & has_settlement & has_previous
  points[calculate_points] <- settlement[calculate_points] - previous[calculate_points]

  quality <- rep(NA_character_, nrow(df))
  quality[has_reported_points] <- "official_reported_change_points"
  quality[!has_reported_points & has_settlement & has_previous] <- "official_previous_adjusted_quote"
  quality[is_di1 & !has_settlement & !has_reported_points] <- "missing_settlement"
  quality[is_di1 & has_settlement & !has_reported_points & !has_previous] <- "missing_base"

  df$di_adjustment_base <- ifelse(is_di1, base, NA_real_)
  df$di_adjustment_points <- points
  df$di_adjustment_quality <- quality
  df$di_adjustment_is_official <- quality %in% c(
    "official_reported_change_points",
    "official_previous_adjusted_quote"
  )
  df
}

.brf_numeric_col <- function(df, col) {
  if (!col %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[col]]))
}
