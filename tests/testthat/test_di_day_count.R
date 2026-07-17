library(testthat)
library(brfutures)

test_that("DI Date pricing uses operation-inclusive maturity-exclusive DU", {
  basis <- as.Date("2026-06-17")
  maturity <- as.Date("2026-06-18")
  rate <- 12
  expected_pu <- 100000 / (1 + rate / 100)^(1 / 252)

  priced <- calculate_futures_di_notional(
    rate,
    maturity_date = maturity,
    basis_date = basis,
    snap_to_tick = FALSE,
    round_pu = FALSE
  )
  expect_equal(priced$valid_days, 1L)
  expect_equal(priced$pu, expected_pu, tolerance = 1e-10)

  roundtrip <- calculate_futures_di_rates(
    priced$pu,
    maturity_date = maturity,
    basis_date = basis,
    snap_to_tick = FALSE
  )
  expect_equal(roundtrip$valid_days, 1L)
  expect_equal(roundtrip$rates, rate, tolerance = 1e-10)
})

test_that("DI DU uses ANBIMA financial days and keeps B3 sessions separate", {
  bizdays::load_builtin_calendars()
  basis <- as.Date("2018-01-24")
  maturity <- as.Date("2018-01-26")

  financial <- calculate_futures_di_notional(
    12,
    maturity_date = maturity,
    basis_date = basis,
    snap_to_tick = FALSE,
    round_pu = FALSE
  )
  sessions <- calculate_futures_di_notional(
    12,
    maturity_date = maturity,
    basis_date = basis,
    cal = "Brazil/B3",
    snap_to_tick = FALSE,
    round_pu = FALSE
  )

  expect_true(bizdays::is.bizday(as.Date("2018-01-25"), "Brazil/ANBIMA"))
  expect_false(bizdays::is.bizday(as.Date("2018-01-25"), "Brazil/B3"))
  expect_equal(financial$valid_days, 2L)
  expect_equal(sessions$valid_days, 1L)
  expect_equal(di_maturity_from_ticker("DI1F18"), as.Date("2018-01-02"))
  expect_equal(di_maturity_from_ticker("DI1K12"), as.Date("2012-05-02"))

  estimated <- brfutures:::`.brf_estimate_maturity`(data.frame(
    date = as.Date(c("2012-04-27", "2012-04-30", "2012-05-02")),
    root = rep("DI1", 3L),
    ticker = rep("DI1K12", 3L),
    stringsAsFactors = FALSE
  ))
  expect_equal(estimated$maturity, rep(as.Date("2012-05-02"), 3L))
})

test_that("DI data-frame and xts augmentation honor the official default", {
  basis <- as.Date("2026-06-17")
  maturity <- as.Date("2026-06-18")
  rate <- 12
  expected_one_day <- round(100000 / (1 + rate / 100)^(1 / 252), 2)
  expected_two_days <- round(100000 / (1 + rate / 100)^(2 / 252), 2)

  rows <- data.frame(
    date = basis,
    maturity = maturity,
    open = rate,
    high = rate,
    low = rate,
    close = rate
  )
  augmented_rows <- brfutures:::`.brf_di_add_pu_columns`(rows)
  expect_equal(augmented_rows$PU_close, expected_one_day)

  rates <- xts::xts(
    cbind(Open = rate, High = rate, Low = rate, Close = rate),
    order.by = basis
  )
  official <- brfutures:::`.brf_di_add_pu_xts`(
    rates,
    "DI1M26",
    maturity_date = maturity,
    include_basis_day = FALSE
  )
  legacy <- brfutures:::`.brf_di_add_pu_xts`(
    rates,
    "DI1M26",
    maturity_date = maturity,
    include_basis_day = TRUE
  )
  expect_equal(as.numeric(official[, "PU_close"]), expected_one_day)
  expect_equal(as.numeric(legacy[, "PU_close"]), expected_two_days)
})
