di_bundle_rows <- function(with_availability = TRUE) {
  out <- data.frame(
    date = as.Date(c(
      "2024-01-02", "2024-01-02", "2024-01-03", "2024-01-04"
    )),
    root = "DI1",
    contract_code = c("DI1G24", "DI1H24", "DI1H24", "DI1H24"),
    ticker = c("DI1G24", "DI1H24", "DI1H24", "DI1H24"),
    maturity = as.Date(c(
      "2024-02-01", "2024-03-01", "2024-03-01", "2024-03-01"
    )),
    open = c(10.02, 10.22, 10.17, 10.12),
    high = c(10.05, 10.25, 10.20, 10.15),
    low = c(9.95, 10.15, 10.10, 10.05),
    close = c(10.00, 10.20, 10.15, 10.10),
    volume = c(100, 200, 220, 240),
    volume_qty = c(10, 20, 22, 24),
    settlement_price = c(95000, 94000, 94010, 94020),
    previous_settlement = c(94999, 93999, 94009, 94019),
    corrected_settlement = c(94999, 93999, 94009, 94019),
    change_points = 1,
    di_adjustment_base = c(94999, 93999, 94009, 94019),
    di_adjustment_points = 1,
    di_adjustment_is_official = TRUE,
    di_adjustment_quality = "official_reported_change_points",
    source = c("html", "xml", "xml", "xml"),
    stringsAsFactors = FALSE
  )
  if (isTRUE(with_availability)) {
    out$publication_timestamp <- c(
      "2024-01-02T20:00:00Z", "2024-01-02T20:00:00Z",
      "2024-01-03T20:00:00Z", "2024-01-04T20:00:00Z"
    )
  }
  out
}

build_di_bundle_fixture <- function(with_availability = TRUE) {
  build_continuous_di_bundle(
    data = di_bundle_rows(with_availability),
    target_tenor = 5,
    tenor_unit = "business_days",
    include_pnl = TRUE,
    synthetic_ticker = "DI1FUT_5BD"
  )
}

test_that("daily DI bundle separates signal and real-contract execution", {
  bundle <- build_di_bundle_fixture()

  expect_s3_class(bundle, "brf_di_continuous_bundle")
  expect_identical(
    names(bundle),
    c(
      "signal_series", "execution_series", "di_continuous_contracts",
      "di_roll_events", "official_sessions", "contract_specs",
      "cost_models", "provenance", "manifest"
    )
  )
  expect_identical(
    bundle$execution_series$contract_symbol,
    c("DI1G24", "DI1H24", "DI1H24")
  )
  expect_identical(
    bundle$di_continuous_contracts$active_real_contract,
    bundle$execution_series$contract_symbol
  )
  expect_true(all(c(
    "rate_open", "rate_high", "rate_low", "rate_close",
    "pu_open", "pu_high", "pu_low", "pu_close"
  ) %in% names(bundle$execution_series)))
  expect_true(all(bundle$execution_series$adjustment_final))
  expect_true(all(bundle$execution_series$availability_observed))
  expect_true(bundle$manifest$execution_supported)
  expect_identical(bundle$manifest$pnl_formula_id, "di1_official_pu")
  expect_identical(bundle$manifest$quote_currency, "BRL")
  expect_identical(bundle$manifest$settlement_currency, "BRL")
  expect_identical(bundle$manifest$pnl_currency, "BRL")
  expect_true(all(bundle$contract_specs$multiplier == 1))
  expect_true(all(
    bundle$contract_specs$settlement_function ==
      "positionsizer::ps_di_session_settlement"
  ))
})

test_that("DI roll event is complete without fabricating an intraday clock", {
  bundle <- build_di_bundle_fixture()
  event <- bundle$di_roll_events

  expect_equal(nrow(event), 1L)
  expect_identical(event$from_contract_symbol, "DI1G24")
  expect_identical(event$to_contract_symbol, "DI1H24")
  expect_equal(event$from_settlement_session, as.Date("2024-01-02"))
  expect_equal(event$execution_session, as.Date("2024-01-03"))
  expect_true(is.na(event$effective_at))
  expect_equal(
    event$available_at,
    as.POSIXct("2024-01-03 20:00:00", tz = "UTC")
  )
  expect_identical(
    event$timing_quality,
    "daily_session_order_no_observed_fill_timestamp"
  )
  expect_equal(event$roll_gap_pnl, 0)
  expect_equal(event$close_from_quantity_per_unit, -1)
  expect_equal(event$open_to_quantity_per_unit, 1)
  expect_true(event$event_execution_supported)
})

test_that("missing observed DI availability fails capability closed", {
  bundle <- build_di_bundle_fixture(with_availability = FALSE)

  expect_false(bundle$manifest$execution_supported)
  expect_equal(
    bundle$manifest$missing_availability_count,
    nrow(bundle$execution_series)
  )
  expect_true(all(is.na(
    bundle$execution_series$adjustment_available_at
  )))
  expect_true(all(is.na(bundle$di_roll_events$available_at)))
  expect_false(any(bundle$di_continuous_contracts$session_execution_supported))
  expect_match(
    paste(bundle$manifest$execution_blockers, collapse = " "),
    "availability_timestamp"
  )
})

test_that("DI bundle fingerprints survive RDS and reject mutation", {
  bundle <- build_di_bundle_fixture()
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(bundle, path)
  restored <- readRDS(path)

  expect_s3_class(
    validate_continuous_di_bundle(restored),
    "brf_di_continuous_bundle"
  )
  expect_identical(
    restored$manifest$bundle_fingerprint,
    bundle$manifest$bundle_fingerprint
  )

  broken <- restored
  broken$execution_series$settlement_pu[[1L]] <-
    broken$execution_series$settlement_pu[[1L]] + 1
  expect_error(
    validate_continuous_di_bundle(broken),
    "official adjustment does not reconcile|fingerprint mismatch"
  )
})

test_that("official DI PU kernel reconciles gross PnL and excludes roll gap", {
  old_carry <- positionsizer::ps_di_session_settlement(
    q_carry = 1,
    session_delta_qty = -1,
    session_delta_qty_pu = -1 * 90000,
    settlement_pu = 90000,
    official_adjustment_points = -20,
    previous_settlement_pu = 89980,
    multiplier = 1
  )
  new_open <- positionsizer::ps_di_session_settlement(
    q_carry = 0,
    session_delta_qty = 1,
    session_delta_qty_pu = 1 * 80000,
    settlement_pu = 79990,
    official_adjustment_points = -10,
    previous_settlement_pu = 80000,
    multiplier = 1
  )

  expect_equal(old_carry$settlement_total, 20)
  expect_equal(new_open$settlement_total, 10)
  expect_equal(
    old_carry$settlement_total + new_open$settlement_total,
    30
  )
  expect_equal(80000 - 90000, -10000)
  expect_false(
    isTRUE(all.equal(
      old_carry$settlement_total + new_open$settlement_total,
      80000 - 90000
    ))
  )
  expect_equal(build_di_bundle_fixture()$di_roll_events$roll_gap_pnl, 0)
})

test_that("DI bundle remains daily-only", {
  expect_error(
    build_continuous_di_bundle(
      data = di_bundle_rows(),
      target_tenor = 5,
      tenor_unit = "business_days",
      synthetic_ticker = "DI1FUT_5BD_1H"
    ),
    "daily-only"
  )
})
