#' Load official DI1 daily adjustment data
#'
#' This helper returns B3 settlement fields in a DI1-safe shape. For legacy HTML
#' rows, `di_adjustment_base` uses `corrected_settlement`. For BVBG XML rows,
#' where the parser maps `PrvsAdjstdQt` to `previous_settlement`,
#' `di_adjustment_base` uses `previous_settlement` directly and does not apply a
#' second carry/correction.
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

.brf_add_di_adjustment_columns <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }

  root <- if ("root" %in% names(df)) toupper(as.character(df$root)) else rep(NA_character_, nrow(df))
  ticker <- if ("ticker" %in% names(df)) toupper(as.character(df$ticker)) else rep(NA_character_, nrow(df))
  contract_code <- if ("contract_code" %in% names(df)) toupper(as.character(df$contract_code)) else rep(NA_character_, nrow(df))
  is_di1 <- root == "DI1" | startsWith(ticker, "DI1") | startsWith(contract_code, "DI1")

  settlement <- .brf_numeric_col(df, "settlement_price")
  previous <- .brf_numeric_col(df, "previous_settlement")
  corrected <- .brf_numeric_col(df, "corrected_settlement")

  base <- corrected
  use_previous <- is.na(base) & !is.na(previous)
  base[use_previous] <- previous[use_previous]

  quality <- rep(NA_character_, nrow(df))
  quality[is_di1 & !is.na(corrected)] <- "official_corrected_settlement"
  quality[is_di1 & is.na(corrected) & !is.na(previous)] <- "official_previous_adjusted_quote"
  quality[is_di1 & is.na(settlement)] <- "missing_settlement"
  quality[is_di1 & !is.na(settlement) & is.na(base)] <- "missing_base"

  points <- settlement - base
  points[!is_di1 | is.na(settlement) | is.na(base)] <- NA_real_

  df$di_adjustment_base <- ifelse(is_di1, base, NA_real_)
  df$di_adjustment_points <- points
  df$di_adjustment_quality <- quality
  df$di_adjustment_is_official <- quality %in% c(
    "official_corrected_settlement",
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
