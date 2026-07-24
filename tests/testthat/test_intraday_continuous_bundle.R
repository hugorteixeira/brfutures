.intraday_daily_fixture <- function() {
  dates <- as.Date(c("2024-01-10", "2024-01-11", "2024-01-12"))
  old <- data.frame(
    date = dates,
    root = "TST",
    ticker = "TSTF24",
    maturity = as.Date("2024-01-13"),
    last_trade_date = as.Date("2024-01-13"),
    open = c(99, 101, 102),
    high = c(101, 103, 104),
    low = c(98, 100, 101),
    close = c(100, 102, 103),
    settlement_price = c(100, 102, 103),
    volume = c(10, 20, 30),
    source = "fixture",
    stringsAsFactors = FALSE
  )
  new <- data.frame(
    date = dates,
    root = "TST",
    ticker = "TSTG24",
    maturity = as.Date("2024-02-15"),
    last_trade_date = as.Date("2024-02-15"),
    open = c(107, 109, 110),
    high = c(109, 111, 112),
    low = c(106, 108, 109),
    close = c(108, 110, 111),
    settlement_price = c(108, 110, 111),
    volume = c(100, 200, 300),
    source = "fixture",
    stringsAsFactors = FALSE
  )
  build_continuous_bundle(
    rbind(old, new),
    root = "TST",
    days_before_roll = 1L,
    roll_grace_sessions = 0L
  )
}

.intraday_bars_fixture <- function() {
  timestamp <- as.POSIXct(
    rep(c(
      "2024-01-10 10:00:00",
      "2024-01-11 10:00:00",
      "2024-01-12 10:00:00"
    ), each = 2L),
    tz = "America/Sao_Paulo"
  )
  contract <- rep(c("TSTF24", "TSTG24"), 3L)
  base <- c(100, 108, 102, 110, 103, 111)
  data.frame(
    timestamp = timestamp,
    root = "TST",
    contract = contract,
    open = base - 1,
    high = base + 1,
    low = base - 2,
    close = base,
    source_volume = seq_along(base) * 10,
    source_volume_semantics = "contracts",
    source_series_id = paste0(contract, "_1H"),
    stringsAsFactors = FALSE
  )
}

test_that("intraday bundle separates adjusted signal and raw execution domains", {
  daily <- .intraday_daily_fixture()
  bars <- .intraday_bars_fixture()
  bundle <- build_intraday_continuous_bundle(
    daily,
    bars,
    timeframe = "1h",
    source = "mt5"
  )

  expect_s3_class(bundle, "brf_intraday_continuous_bundle")
  expect_named(
    bundle,
    c(
      "signal_series", "bar_map", "session_map", "roll_schedule",
      "execution_data", "settlement_data", "manifest"
    )
  )
  expect_equal(bundle$manifest$schema_id, "b3_intraday_continuous_v1")
  expect_equal(bundle$manifest$parent_schema_version, 3L)
  expect_equal(bundle$manifest$synthetic_ticker, "TSTFUT_B1_1H")
  expect_equal(bundle$manifest$timestamp_semantics, "bar_open")
  expect_equal(
    bundle$manifest$settlement_availability,
    "end_of_session_phase_no_invented_timestamp"
  )
  expect_true(bundle$manifest$execution_supported)

  expect_equal(nrow(bundle$signal_series), 3L)
  old_factor <- daily$contract_map$signal_adjustment_factor[[1L]]
  expect_equal(bundle$signal_series$close[[1L]], 100 * old_factor)
  expect_equal(
    bundle$signal_series$available_at[[1L]],
    bundle$signal_series$timestamp[[1L]] + 3600
  )
  expect_equal(
    unique(bundle$signal_series$source_volume_semantics),
    "mt5_tick_or_trade_count_unverified"
  )

  raw_first <- bundle$execution_data[
    bundle$execution_data$contract == "TSTF24",
    ,
    drop = FALSE
  ][1L, ]
  expect_equal(raw_first$close, 100)
  expect_false("factor" %in% names(bundle$execution_data))
  expect_true(all(bundle$execution_data$contract %in% c("TSTF24", "TSTG24")))
  expect_true(all(!grepl("FUT|\\$|_OLD|_AGG", bundle$execution_data$contract)))

  expect_s3_class(bundle$execution_data$timestamp, "POSIXct")
  expect_s3_class(bundle$bar_map$timestamp, "POSIXct")
  expect_s3_class(bundle$settlement_data$date, "Date")
  expect_false("timestamp" %in% names(bundle$settlement_data))
  expect_true(all(
    bundle$settlement_data$availability_phase ==
      "end_of_session_official_settlement"
  ))
  expect_equal(bundle$roll_schedule$from_contract, "TSTF24")
  expect_equal(bundle$roll_schedule$to_contract, "TSTG24")
})

test_that("bar map preserves sparse signal and execution clocks without inner join", {
  daily <- .intraday_daily_fixture()
  middle <- daily$contract_map$date == as.Date("2024-01-11")
  daily$contract_map$signal_contract[middle] <- "TSTG24"
  daily$contract_map$signal_adjustment_factor[middle] <- 1
  daily$contract_map$signal_inverse_factor[middle] <- 1

  bars <- .intraday_bars_fixture()
  new_middle <- bars$contract == "TSTG24" &
    as.Date(bars$timestamp, tz = "America/Sao_Paulo") == as.Date("2024-01-11")
  bars$timestamp[new_middle] <- bars$timestamp[new_middle] + 3600

  bundle <- build_intraday_continuous_bundle(
    daily,
    bars,
    timeframe = "1h",
    source = "barchart"
  )
  middle_map <- bundle$bar_map[
    bundle$bar_map$session_date == as.Date("2024-01-11"),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(middle_map), 2L)
  expect_equal(middle_map$signal_bar_available, c(FALSE, TRUE))
  expect_equal(middle_map$execution_bar_available, c(TRUE, FALSE))
  expect_equal(
    bundle$manifest$sparse_clock_policy,
    "union_signal_and_execution_events_no_inner_join"
  )
})

test_that("order-transform factor is never exposed from the same session", {
  bundle <- build_intraday_continuous_bundle(
    .intraday_daily_fixture(),
    .intraday_bars_fixture(),
    timeframe = "1h",
    source = "mt5"
  )
  available <- bundle$bar_map$order_transform_available
  expect_true(all(
    bundle$bar_map$order_transform_asof_date[available] <
      bundle$bar_map$session_date[available]
  ))
})

test_that("official no-bar sessions remain markable but cannot create fills", {
  bars <- .intraday_bars_fixture()
  bars <- bars[
    as.Date(bars$timestamp, tz = "America/Sao_Paulo") != as.Date("2024-01-11"),
    ,
    drop = FALSE
  ]
  bundle <- build_intraday_continuous_bundle(
    .intraday_daily_fixture(),
    bars,
    timeframe = "1h",
    source = "mt5"
  )

  expect_true(as.Date("2024-01-11") %in% bundle$session_map$date)
  expect_true(as.Date("2024-01-11") %in% bundle$settlement_data$date)
  expect_false(as.Date("2024-01-11") %in% bundle$bar_map$session_date)
  expect_true(bundle$manifest$execution_supported)
})

test_that("intraday exact mode rejects synthetic symbols and missing settlement", {
  daily <- .intraday_daily_fixture()
  synthetic <- .intraday_bars_fixture()
  synthetic$contract[[1L]] <- "TSTFUT"
  expect_error(
    build_intraday_continuous_bundle(
      daily, synthetic, timeframe = "1h", source = "mt5"
    ),
    "raw dated-contract symbols"
  )

  old_alias <- .intraday_bars_fixture()
  old_alias$contract[[1L]] <- "TSTF24_OLD"
  expect_error(
    build_intraday_continuous_bundle(
      daily, old_alias, timeframe = "1h", source = "mt5"
    ),
    "raw dated-contract symbols"
  )

  missing_pa <- daily
  missing_pa$execution_data$settlement_price[
    missing_pa$execution_data$date == as.Date("2024-01-11") &
      missing_pa$execution_data$contract == "TSTF24"
  ] <- NA_real_
  expect_error(
    build_intraday_continuous_bundle(
      missing_pa,
      .intraday_bars_fixture(),
      timeframe = "1h",
      source = "mt5"
    ),
    "Missing positive official settlement"
  )
})

test_that("intraday validation enforces bar-close availability and official sessions", {
  daily <- .intraday_daily_fixture()
  bars <- .intraday_bars_fixture()
  bars$available_at <- bars$timestamp
  expect_error(
    build_intraday_continuous_bundle(
      daily, bars, timeframe = "1h", source = "mt5"
    ),
    "at or after bar close"
  )

  bars <- .intraday_bars_fixture()
  bars$session_date <- as.Date(bars$timestamp, tz = "America/Sao_Paulo") + 1L
  expect_error(
    build_intraday_continuous_bundle(
      daily, bars, timeframe = "1h", source = "mt5"
    ),
    "disagrees"
  )
})

test_that("intraday execution cannot exceed parent daily support", {
  daily <- .intraday_daily_fixture()
  daily$manifest$execution_supported <- FALSE
  bundle <- build_intraday_continuous_bundle(
    daily,
    .intraday_bars_fixture(),
    timeframe = "1h",
    source = "mt5"
  )

  expect_false(bundle$manifest$parent_execution_supported)
  expect_false(bundle$manifest$execution_supported)
})

test_that("observed sparse child grids require explicit manifest-recorded opt in", {
  bars <- .intraday_bars_fixture()
  bars$observed_child_bars <- 2L
  bars$expected_child_bars <- 12L
  bars$observed_grid_policy <- "allow_observed_sparse"
  bars$data_quality <- "sparse_observed_child_grid_closed_bucket"

  require_policy <- bars
  require_policy$observed_grid_policy <- "require_complete_grid"
  expect_error(
    build_intraday_continuous_bundle(
      .intraday_daily_fixture(),
      require_policy,
      timeframe = "1h",
      source = "mt5"
    ),
    "require explicit"
  )

  bundle <- build_intraday_continuous_bundle(
    .intraday_daily_fixture(),
    bars,
    timeframe = "1h",
    source = "mt5",
    grid_policy = "allow_observed_sparse"
  )
  expect_equal(
    bundle$manifest$observed_grid_policy,
    "allow_observed_sparse"
  )
  expect_true(bundle$manifest$sparse_observed_opt_in)
  expect_equal(bundle$manifest$sparse_observed_bar_count, nrow(bars))
  expect_equal(
    bundle$manifest$execution_quality,
    "exact_real_contract_bars_observed_sparse_opt_in"
  )
  expect_equal(
    unique(bundle$execution_data$observed_grid_policy),
    "allow_observed_sparse"
  )
})
