library(testthat)
library(brfutures)

.di_continuous_rows <- function(date,
                                ticker,
                                maturity,
                                rate = 10,
                                settlement = 95000) {
  n <- length(date)
  rate <- rep_len(as.numeric(rate), n)
  settlement <- rep_len(as.numeric(settlement), n)
  data.frame(
    date = as.Date(date),
    root = rep("DI1", n),
    contract_code = as.character(ticker),
    ticker = as.character(ticker),
    maturity = as.Date(maturity),
    open = rate + 0.02,
    high = rate + 0.05,
    low = rate - 0.05,
    close = rate,
    volume = seq_len(n) * 100,
    volume_qty = seq_len(n) * 10,
    settlement_price = settlement,
    previous_settlement = settlement - 1,
    change_points = rep(1, n),
    stringsAsFactors = FALSE
  )
}

.build_synthetic_di <- function(data,
                                target_days = 5,
                                include_pnl = FALSE,
                                coverage_mode = "first_eligible") {
  build_continuous_di(
    data = data,
    target_tenor = target_days,
    tenor_unit = "business_days",
    include_pnl = include_pnl,
    add_attrs = FALSE,
    add_globalenv = FALSE,
    coverage_mode = coverage_mode
  )
}

test_that("constant-tenor DI selection is deterministic and never rolls backward", {
  data <- .di_continuous_rows(
    date = c(
      "2024-01-02", "2024-01-02",
      "2024-01-03",
      "2024-01-04", "2024-01-04"
    ),
    ticker = c("DI1G24", "DI1H24", "DI1H24", "DI1G24", "DI1H24"),
    maturity = c(
      "2024-02-01", "2024-03-01",
      "2024-03-01",
      "2024-02-01", "2024-03-01"
    ),
    rate = c(10.00, 10.20, 10.15, 10.05, 10.10),
    settlement = c(95000, 94000, 94010, 95005, 94020)
  )

  ordered <- .build_synthetic_di(data, include_pnl = TRUE)
  shuffled <- .build_synthetic_di(data[nrow(data):1L, , drop = FALSE], include_pnl = TRUE)

  active <- attr(ordered, "active_contracts")
  active_shuffled <- attr(shuffled, "active_contracts")
  expect_equal(active$contract_symbol, c("DI1G24", "DI1H24", "DI1H24"))
  expect_equal(active$ticker, active$contract_symbol)
  expect_true(all(diff(as.numeric(active$actual_maturity)) >= 0))
  expect_equal(active, active_shuffled)
  expect_equal(attr(ordered, "contract_map"), attr(shuffled, "contract_map"))
  expect_equal(unclass(ordered), unclass(shuffled))

  required <- c(
    "ContractOrdinal", "ActualMaturity", "ValidDays",
    "RateOpenRaw", "RateHighRaw", "RateLowRaw", "RateCloseRaw",
    "PUOpenRaw", "PUHighRaw", "PULowRaw", "PUCloseRaw",
    "Settlement", "PreviousSettlement", "AdjustmentBase",
    "OfficialAdjustment", "AdjustmentOfficial"
  )
  expect_true(all(required %in% colnames(ordered)))
  expect_equal(as.numeric(ordered[, "ContractOrdinal"]), active$contract_ordinal)
  expect_equal(
    as.Date(as.numeric(ordered[, "ActualMaturity"]), origin = "1970-01-01"),
    active$actual_maturity
  )
  expect_equal(as.numeric(ordered[, "OfficialAdjustment"]), rep(1, NROW(ordered)))
  expect_true(isTRUE(attr(ordered, "PU_pnl_is_approximate")))
})

test_that("economically identical duplicate DI quotes collapse independently of order", {
  quote <- .di_continuous_rows(
    date = "2024-01-02",
    ticker = "DI1G24",
    maturity = "2024-02-01",
    rate = 10,
    settlement = 95000
  )
  duplicates <- rbind(quote, quote)
  duplicates$source <- c("z_provider", "a_provider")

  forward <- .build_synthetic_di(duplicates)
  reverse <- .build_synthetic_di(duplicates[2:1, , drop = FALSE])

  expect_equal(NROW(forward), 1L)
  expect_equal(unclass(forward), unclass(reverse))
  expect_equal(attr(forward, "active_contracts"), attr(reverse, "active_contracts"))
})

test_that("conflicting duplicate DI quotes fail closed", {
  quote <- .di_continuous_rows(
    date = "2024-01-02",
    ticker = "DI1G24",
    maturity = "2024-02-01",
    rate = 10,
    settlement = 95000
  )
  conflicting <- rbind(quote, quote)
  conflicting$close[[2L]] <- conflicting$close[[2L]] + 0.01

  expect_error(
    .build_synthetic_di(conflicting),
    "Conflicting duplicate DI quotes.*economic fields differ: close"
  )
})

test_that("strict target drops only an ineligible prefix and fails on an internal gap", {
  prefix <- .di_continuous_rows(
    date = c("2024-01-02", "2024-01-03", "2024-01-04"),
    ticker = c("DI1F24", "DI1G24", "DI1G24"),
    maturity = c("2024-01-15", "2024-02-01", "2024-02-01"),
    rate = c(10.0, 10.1, 10.2)
  )
  series <- .build_synthetic_di(prefix, target_days = 15)
  expect_equal(
    format(as.Date(zoo::index(series)), "%Y-%m-%d"),
    c("2024-01-03", "2024-01-04")
  )
  expect_equal(
    attr(series, "continuous_spec")$initial_dates_dropped,
    as.Date("2024-01-02")
  )
  expect_true(isTRUE(attr(series, "continuous_spec")$strict_target))

  internal_gap <- rbind(
    prefix,
    .di_continuous_rows(
      date = "2024-01-05",
      ticker = "DI1F24",
      maturity = "2024-01-15",
      rate = 10.3
    )
  )
  expect_error(
    .build_synthetic_di(internal_gap, target_days = 15),
    "no eligible monotonic contract on 2024-01-05 after the series started"
  )

  restarted <- rbind(
    internal_gap,
    .di_continuous_rows(
      date = c("2024-01-08", "2024-01-09"),
      ticker = c("DI1H24", "DI1H24"),
      maturity = c("2024-03-01", "2024-03-01"),
      rate = c(10.4, 10.5)
    )
  )
  suffix <- .build_synthetic_di(
    restarted,
    target_days = 15,
    coverage_mode = "restart_strict_suffix"
  )
  expect_equal(
    format(as.Date(zoo::index(suffix)), "%Y-%m-%d"),
    c("2024-01-08", "2024-01-09")
  )
  suffix_spec <- attr(suffix, "continuous_spec")
  expect_identical(suffix_spec$coverage_mode, "restart_strict_suffix")
  expect_equal(suffix_spec$coverage_start, as.Date("2024-01-08"))
  expect_equal(suffix_spec$gap_resets$gap_date, as.Date("2024-01-05"))
  expect_false(suffix_spec$gap_resets$fresh_start_eligible)

  same_day_restart <- .di_continuous_rows(
    date = c("2024-01-02", "2024-01-03", "2024-01-04"),
    ticker = c("DI1H24", "DI1G24", "DI1G24"),
    maturity = c("2024-03-01", "2024-02-01", "2024-02-01"),
    rate = c(10.0, 10.1, 10.2)
  )
  same_day_suffix <- .build_synthetic_di(
    same_day_restart,
    target_days = 15,
    coverage_mode = "restart_strict_suffix"
  )
  expect_equal(
    format(as.Date(zoo::index(same_day_suffix)), "%Y-%m-%d"),
    c("2024-01-03", "2024-01-04")
  )
  expect_true(attr(same_day_suffix, "continuous_spec")$gap_resets$fresh_start_eligible)
})

test_that("constant-tenor DI aborts a roll without a finite common bridge", {
  no_bridge <- .di_continuous_rows(
    date = c("2024-01-02", "2024-01-03"),
    ticker = c("DI1G24", "DI1H24"),
    maturity = c("2024-02-01", "2024-03-01"),
    rate = c(10.0, 10.2)
  )
  expect_error(
    .build_synthetic_di(no_bridge),
    "no finite common rate/PU bridge exists"
  )
})

test_that("restart suffix cannot borrow a roll bridge from discarded history", {
  bridge_only_before_restart <- .di_continuous_rows(
    date = c(
      "2024-01-02", "2024-01-02", "2024-01-02",
      "2024-01-03", "2024-01-04", "2024-01-05"
    ),
    ticker = c(
      "DI1G24", "DI1J24", "DI1K24",
      "DI1K24", "DI1G24", "DI1J24"
    ),
    maturity = c(
      "2024-02-01", "2024-04-01", "2024-05-02",
      "2024-05-02", "2024-02-01", "2024-04-01"
    ),
    rate = c(10.00, 10.10, 10.20, 10.25, 10.30, 10.35)
  )

  expect_error(
    .build_synthetic_di(
      bridge_only_before_restart,
      target_days = 15,
      coverage_mode = "restart_strict_suffix"
    ),
    "no finite common rate/PU bridge exists"
  )
})

test_that("strict target uses exact official DU without an extra basis day", {
  cal <- brfutures:::`.brf_di_resolve_calendar`()
  basis <- as.Date("2024-01-02")
  maturity_251 <- bizdays::add.bizdays(basis, 251L, cal)
  maturity_252 <- bizdays::add.bizdays(basis, 252L, cal)
  short_row <- .di_continuous_rows(
    date = basis,
    ticker = "DI1F25",
    maturity = maturity_251,
    rate = 10
  )

  expect_error(
    .build_synthetic_di(short_row, target_days = 252),
    "No DI contract at or above target_days=252"
  )

  eligible_row <- .di_continuous_rows(
    date = basis,
    ticker = "DI1G25",
    maturity = maturity_252,
    rate = 10.1
  )
  series <- .build_synthetic_di(rbind(short_row, eligible_row), target_days = 252)
  expect_equal(as.numeric(series[, "ValidDays"]), 252)
  expect_equal(attr(series, "active_contracts")$contract_symbol, "DI1G25")
  expect_equal(as.Date(attr(series, "maturity")), maturity_252)
})

test_that("constant-tenor DI selection uses financial DU, not session count", {
  basis <- as.Date("2018-01-24")
  maturity <- as.Date("2018-01-26")
  row <- .di_continuous_rows(
    date = basis,
    ticker = "DI1G18",
    maturity = maturity,
    rate = 10
  )

  series <- .build_synthetic_di(row, target_days = 2)
  expect_equal(as.numeric(series[, "ValidDays"]), 2)
  expect_equal(attr(series, "active_contracts")$actual_maturity, maturity)
})

test_that("DI adjustment treatment remains volume-compatible with the continuous builder", {
  raw <- .di_continuous_rows(
    date = "2024-01-02",
    ticker = "DI1G24",
    maturity = "2024-03-01",
    rate = 10,
    settlement = 95000
  )
  raw$source <- "html"
  raw$contracts_traded <- raw$volume_qty
  raw$volume_qty <- NULL

  normalized <- brfutures:::`.brf_agg_di_adjustments_treatment`(raw)
  series <- .build_synthetic_di(normalized)

  expect_true(all(c(
    "di_adjustment_base", "di_adjustment_points",
    "di_adjustment_quality", "di_adjustment_is_official"
  ) %in% names(normalized)))
  expect_equal(as.numeric(series[, "Volume_Qty"]), raw$contracts_traded)
  expect_equal(as.numeric(series[, "OfficialAdjustment"]), raw$change_points)
})

test_that("continuous DI preserves legacy consensus before filtering invalid OHLC", {
  rows <- data.frame(
    date = rep(as.Date("2019-04-01"), 4L),
    root = "DI1",
    contract_code = c("DI1F23", "DI1F24", "DI1F25", "DI1J24"),
    ticker = c("DI1F23", "DI1F24", "DI1F25", "DI1J24"),
    source = "html",
    maturity = as.Date(c("2023-01-02", "2024-01-02", "2025-01-02", "2024-04-01")),
    open = c(10.00, NA, NA, 10.10),
    high = c(10.05, NA, NA, 10.15),
    low = c(9.95, NA, NA, 10.05),
    close = c(10.00, NA, NA, 10.10),
    volume = c(100, 200, 300, 400),
    volume_qty = c(10, 20, 30, 40),
    settlement_price = c(74593.63, 68089.32, 62082.90, 66596.99),
    previous_settlement = c(74286.41, 67800.18, 61735.04, 66280.00),
    corrected_settlement = c(74304.70, 67816.87, 61750.24, 66613.39),
    change_points = c(307.22, 289.14, 347.86, 316.99),
    stringsAsFactors = FALSE
  )
  normalized <- normalize_brfut_di_adjustments(rows)
  expect_true(all(normalized$di_adjustment_is_official))

  series <- .build_synthetic_di(normalized, target_days = 1)

  expect_equal(attr(series, "active_contracts")$contract_symbol, "DI1F23")
  expect_equal(as.numeric(series[, "AdjustmentBase"]), 74304.70)
  expect_equal(as.numeric(series[, "OfficialAdjustment"]), 288.93, tolerance = 1e-8)
  expect_equal(as.numeric(series[, "AdjustmentOfficial"]), 1)
})
