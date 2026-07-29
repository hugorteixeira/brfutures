.continuous_bundle_with_observed_clocks <- function(data) {
  observed <- as.POSIXct(
    paste(as.character(data$date), "22:30:00"),
    tz = "UTC"
  )
  data$available_at <- observed
  data$settlement_available_at <- observed
  data
}

.continuous_bundle_fixture <- function() {
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
    volume = c(1000, 2000, 3000),
    contracts_traded = c(10, 20, 30),
    open_interest = c(100, 90, 80),
    close_interest = c(90, 80, 70),
    trade_count = c(5, 6, 7),
    source = "fixture",
    auxiliary_fx = 1,
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
    volume = c(10000, 20000, 30000),
    contracts_traded = c(100, 200, 300),
    open_interest = c(1000, 1100, 1200),
    close_interest = c(1100, 1200, 1300),
    trade_count = c(50, 60, 70),
    source = "fixture",
    auxiliary_fx = 1,
    stringsAsFactors = FALSE
  )
  .continuous_bundle_with_observed_clocks(rbind(old, new))
}

.continuous_bundle_causal_bridge_fixture <- function(first_new_ohlc_valid = FALSE,
                                                     current_new_settlement = 110) {
  dates <- as.Date(c("2022-02-25", "2022-03-02", "2022-03-03", "2022-03-04"))
  old_settlement <- c(100, 102, 105, 106)
  old <- data.frame(
    date = dates,
    root = "BGI",
    ticker = "BGIK22",
    maturity = as.Date("2022-05-31"),
    last_trade_date = as.Date("2022-05-31"),
    open = old_settlement - 1,
    high = old_settlement + 1,
    low = old_settlement - 2,
    close = old_settlement,
    settlement_price = old_settlement,
    volume = 100,
    stringsAsFactors = FALSE
  )
  first_new_ohlc <- if (isTRUE(first_new_ohlc_valid)) {
    c(open = 107, high = 109, low = 106, close = 108)
  } else {
    c(open = 0, high = 0, low = 0, close = 0)
  }
  new <- data.frame(
    date = dates[-1L],
    root = "BGI",
    ticker = "BGIM22",
    maturity = as.Date("2022-06-30"),
    last_trade_date = as.Date("2022-06-30"),
    open = c(first_new_ohlc[["open"]], 109, 110),
    high = c(first_new_ohlc[["high"]], 111, 112),
    low = c(first_new_ohlc[["low"]], 108, 109),
    close = c(first_new_ohlc[["close"]], 110, 111),
    settlement_price = c(108, current_new_settlement, 111),
    volume = c(if (isTRUE(first_new_ohlc_valid)) 10 else 0, 10, 20),
    stringsAsFactors = FALSE
  )
  .continuous_bundle_with_observed_clocks(rbind(old, new))
}

.continuous_bundle_no_bridge_before_expiry_fixture <- function() {
  raw <- .continuous_bundle_fixture()
  after_expiry <- raw[
    raw$ticker == "TSTG24" &
      raw$date == as.Date("2024-01-12"),
    ,
    drop = FALSE
  ]
  after_expiry$date <- as.Date("2024-01-15")
  raw <- rbind(raw, after_expiry)
  raw$settlement_price[
    raw$ticker == "TSTG24" &
      raw$date %in% as.Date(c("2024-01-11", "2024-01-12"))
  ] <- NA_real_
  .continuous_bundle_with_observed_clocks(raw)
}

.continuous_bundle_coalesced_target_fixture <- function() {
  dates <- as.Date("2024-01-01") + 0:5
  make_contract <- function(contract, last_trade_date, settlement, tradable) {
    .continuous_bundle_with_observed_clocks(data.frame(
      date = dates,
      root = "TST",
      ticker = contract,
      maturity = as.Date(last_trade_date),
      last_trade_date = as.Date(last_trade_date),
      open = ifelse(tradable, settlement - 1, 0),
      high = ifelse(tradable, settlement + 1, 0),
      low = ifelse(tradable, settlement - 2, 0),
      close = ifelse(tradable, settlement, 0),
      settlement_price = settlement,
      volume = ifelse(tradable, 10, 0),
      stringsAsFactors = FALSE
    ))
  }
  rbind(
    make_contract("TSTH24", "2024-01-03", 100:105, TRUE),
    make_contract("TSTJ24", "2024-01-05", 105:110, FALSE),
    make_contract("TSTK24", "2024-01-20", 110:115, TRUE)
  )
}

.continuous_bundle_successor_fallback_fixture <- function() {
  dates <- as.Date("2024-01-01") + 0:8
  make_contract <- function(contract, last_trade_date, settlement, tradable) {
    .continuous_bundle_with_observed_clocks(data.frame(
      date = dates,
      root = "TST",
      ticker = contract,
      maturity = as.Date(last_trade_date),
      last_trade_date = as.Date(last_trade_date),
      open = ifelse(tradable, settlement - 1, 0),
      high = ifelse(tradable, settlement + 1, 0),
      low = ifelse(tradable, settlement - 2, 0),
      close = ifelse(tradable, settlement, 0),
      settlement_price = settlement,
      volume = ifelse(tradable, 10, 0),
      stringsAsFactors = FALSE
    ))
  }
  rbind(
    make_contract("TSTH24", "2024-01-03", 100:108, TRUE),
    make_contract("TSTJ24", "2024-01-15", 105:113, FALSE),
    make_contract("TSTK24", "2024-01-20", 110:118, TRUE)
  )
}

test_that("continuous bundle v4 maps real contracts and observed settlement clocks", {
  bundle <- build_continuous_bundle(
    .continuous_bundle_fixture(),
    root = "TST",
    days_before_roll = 1,
    roll_grace_sessions = 0L
  )

  expect_s3_class(bundle, "brf_continuous_bundle")
  expect_named(
    bundle,
    c("signal_series", "contract_map", "roll_schedule", "execution_data", "manifest")
  )
  expect_equal(bundle$manifest$schema_version, 4L)
  expect_false(bundle$manifest$available_at_required)
  expect_false(bundle$manifest$settlement_available_at_required)
  expect_true(bundle$manifest$observed_clock_complete)
  expect_true(bundle$manifest$heterogeneous_same_close_supported)
  expect_true(bundle$manifest$daily_session_phase_execution_supported)
  expect_s3_class(bundle$contract_map$available_at, "POSIXct")
  expect_s3_class(bundle$contract_map$settlement_available_at, "POSIXct")
  expect_equal(
    bundle$contract_map$available_at,
    bundle$contract_map$settlement_available_at
  )
  expect_s3_class(bundle$execution_data$available_at, "POSIXct")
  expect_s3_class(
    bundle$execution_data$settlement_available_at,
    "POSIXct"
  )
  expect_equal(bundle$manifest$adjustment_method, "multiplicative_ratio")
  expect_equal(bundle$manifest$adjustment_anchor, "official_settlement")
  expect_equal(bundle$manifest$roll_rule, "calendar_days_to_last_trade")
  expect_equal(bundle$manifest$roll_grace_sessions, 0L)
  expect_equal(
    bundle$manifest$roll_timing_policy,
    "causal_prebuffer_first_executable"
  )
  expect_equal(
    bundle$manifest$roll_grace_policy,
    "symmetric_prebuffer_then_causal_extension"
  )
  expect_equal(
    bundle$manifest$signal_coordinate_policy,
    "last_observed_signal_contract_causal_carry"
  )
  expect_equal(bundle$manifest$delayed_roll_count, 0L)
  expect_true(bundle$manifest$execution_supported)
  expect_equal(bundle$manifest$synthetic_ticker, "TSTFUT_B1")

  expect_equal(
    bundle$contract_map$active_contract,
    c("TSTF24", "TSTF24", "TSTG24")
  )
  expect_equal(bundle$contract_map$days_to_last_trade[1:2], c(3L, 2L))
  expect_true(bundle$contract_map$is_roll_effective[3])
  expect_equal(bundle$roll_schedule$execution_date, as.Date("2024-01-11"))
  expect_equal(bundle$roll_schedule$decision_date, as.Date("2024-01-11"))
  expect_equal(bundle$roll_schedule$nominal_effective_date, as.Date("2024-01-12"))
  expect_equal(bundle$roll_schedule$execution_offset_sessions, 0L)
  expect_equal(bundle$roll_schedule$early_sessions_used, 0L)
  expect_equal(bundle$roll_schedule$grace_sessions_used, 0L)
  expect_false(bundle$roll_schedule$roll_early)
  expect_false(bundle$roll_schedule$roll_delayed)
  expect_equal(bundle$roll_schedule$effective_date, as.Date("2024-01-12"))
  expect_equal(bundle$roll_schedule$from_contract, "TSTF24")
  expect_equal(bundle$roll_schedule$to_contract, "TSTG24")
  expect_equal(bundle$roll_schedule$requested_nominal_contract, "TSTG24")
  expect_equal(bundle$roll_schedule$target_selection_reason, "nominal_target")
  expect_equal(bundle$roll_schedule$from_settlement_price, 102)
  expect_equal(bundle$roll_schedule$to_settlement_price, 110)
  expect_true(bundle$roll_schedule$validated)
})

test_that("backward bundle adjusts every price field but no activity field", {
  raw <- .continuous_bundle_fixture()
  bundle <- build_continuous_bundle(
    raw, "TST", 1, "backward", roll_grace_sessions = 0L
  )
  ratio <- 110 / 102
  signal <- bundle$signal_series

  expect_equal(signal$factor, c(ratio, ratio, 1))
  expect_equal(signal$inverse_factor, 1 / signal$factor)
  expect_equal(signal$open, c(99, 101, 110) * c(ratio, ratio, 1))
  expect_equal(signal$high, c(101, 103, 112) * c(ratio, ratio, 1))
  expect_equal(signal$low, c(98, 100, 109) * c(ratio, ratio, 1))
  expect_equal(signal$close, c(100, 102, 111) * c(ratio, ratio, 1))
  expect_equal(signal$settlement_price, c(100, 102, 111) * c(ratio, ratio, 1))

  expect_equal(signal$volume, c(1000, 2000, 30000))
  expect_equal(signal$volume_qty, c(10, 20, 300))
  expect_equal(signal$open_interest, c(100, 90, 1200))
  expect_equal(signal$trade_count, c(5, 6, 70))
  expect_equal(bundle$execution_data$settlement_price, c(100, 108, 102, 110, 103, 111))
  expect_equal(bundle$execution_data$auxiliary_fx, rep(1, 6))
})

test_that("forward and backward coordinates are globally proportional", {
  raw <- .continuous_bundle_fixture()
  backward <- build_continuous_bundle(
    raw, "TST", 1, "backward", roll_grace_sessions = 0L
  )
  forward <- build_continuous_bundle(
    raw, "TST", 1, "forward", roll_grace_sessions = 0L
  )
  ratio <- 110 / 102

  expect_equal(forward$manifest$synthetic_ticker, "TSTFUT_F1")
  expect_equal(forward$signal_series$factor, c(1, 1, 1 / ratio))
  proportionality <- backward$signal_series$close / forward$signal_series$close
  expect_equal(proportionality, rep(ratio, 3))
  expect_equal(
    diff(log(backward$signal_series$close)),
    diff(log(forward$signal_series$close))
  )
  expect_equal(
    backward$contract_map$active_contract,
    forward$contract_map$active_contract
  )
  expect_equal(
    backward$roll_schedule$adjustment_ratio,
    forward$roll_schedule$adjustment_ratio
  )
})

test_that("multiple official-settlement rolls compose factors by segment", {
  dates <- as.Date("2024-01-01") + 0:4
  make_contract <- function(contract, last_trade_date, settlement, keep) {
    settlement <- settlement[keep]
    data.frame(
      date = dates[keep],
      root = "TST",
      ticker = contract,
      maturity = as.Date(last_trade_date),
      last_trade_date = as.Date(last_trade_date),
      open = settlement - 1,
      high = settlement + 1,
      low = settlement - 2,
      close = settlement,
      settlement_price = settlement,
      volume = seq_along(settlement),
      stringsAsFactors = FALSE
    )
  }
  raw <- .continuous_bundle_with_observed_clocks(rbind(
    make_contract("TSTF24", "2024-01-04", 100:104, 1:4),
    make_contract("TSTG24", "2024-01-06", 110:114, 1:5),
    make_contract("TSTH24", "2024-02-15", 120:124, 1:5)
  ))
  backward <- build_continuous_bundle(
    raw, "TST", 1, "backward", roll_grace_sessions = 0L
  )
  forward <- build_continuous_bundle(
    raw, "TST", 1, "forward", roll_grace_sessions = 0L
  )
  ratio_1 <- 111 / 101
  ratio_2 <- 123 / 113

  expect_equal(
    backward$contract_map$active_contract,
    c("TSTF24", "TSTF24", "TSTG24", "TSTG24", "TSTH24")
  )
  expect_equal(
    backward$signal_series$factor,
    c(ratio_1 * ratio_2, ratio_1 * ratio_2, ratio_2, ratio_2, 1)
  )
  expect_equal(
    forward$signal_series$factor,
    c(1, 1, 1 / ratio_1, 1 / ratio_1, 1 / (ratio_1 * ratio_2))
  )
  expect_equal(
    backward$signal_series$close / forward$signal_series$close,
    rep(ratio_1 * ratio_2, 5)
  )
})

test_that("the raw inter-contract gap is informational, not an adjustment failure", {
  bundle <- build_continuous_bundle(
    .continuous_bundle_fixture(), "TST", 1, roll_grace_sessions = 0L
  )
  roll <- bundle$roll_schedule
  gap <- roll$to_settlement_price - roll$from_settlement_price

  expect_equal(gap, 8)
  expect_equal(
    roll$from_settlement_price * roll$adjustment_ratio,
    roll$to_settlement_price
  )
  expect_equal(
    bundle$signal_series$settlement_price[2],
    roll$to_settlement_price
  )
})

test_that("exact roll rejects a missing new-contract official settlement", {
  expect_error(
    build_continuous_bundle(
      .continuous_bundle_no_bridge_before_expiry_fixture(),
      "TST",
      1,
      roll_grace_sessions = 0L
    ),
    "no executable bridge before last_trade_date"
  )
})

test_that("roll grace carries the old contract to the first causal executable bridge", {
  dates <- as.Date(c("2022-02-25", "2022-03-02", "2022-03-03", "2022-03-04"))
  old_settlement <- c(100, 102, 105, 106)
  old <- data.frame(
    date = dates,
    root = "BGI",
    ticker = "BGIK22",
    maturity = as.Date("2022-05-31"),
    last_trade_date = as.Date("2022-05-31"),
    open = old_settlement - 1,
    high = old_settlement + 1,
    low = old_settlement - 2,
    close = old_settlement,
    settlement_price = old_settlement,
    volume = 100,
    stringsAsFactors = FALSE
  )
  new <- data.frame(
    date = dates[-1L],
    root = "BGI",
    ticker = "BGIM22",
    maturity = as.Date("2022-06-30"),
    last_trade_date = as.Date("2022-06-30"),
    open = c(0, 109, 110),
    high = c(0, 111, 112),
    low = c(0, 108, 109),
    close = c(0, 110, 111),
    settlement_price = c(108, 110, 111),
    volume = c(0, 10, 20),
    stringsAsFactors = FALSE
  )

  expect_warning(
    bundle <- build_continuous_bundle(
      .continuous_bundle_with_observed_clocks(rbind(old, new)),
      root = "BGI",
      days_before_roll = 90,
      roll_grace_sessions = 3L,
      synthetic_ticker = "BGIFUT_B90"
    ),
    "BGIK22->BGIM22.*2 session"
  )

  expect_equal(
    bundle$contract_map$active_contract,
    c("BGIK22", "BGIK22", "BGIK22", "BGIM22")
  )
  expect_equal(
    bundle$contract_map$nominal_active_contract,
    c("BGIK22", "BGIM22", "BGIM22", "BGIM22")
  )
  expect_equal(bundle$contract_map$roll_pending, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(bundle$contract_map$roll_grace_session, c(0L, 1L, 2L, 0L))
  expect_equal(bundle$roll_schedule$decision_date, as.Date("2022-02-25"))
  expect_equal(bundle$roll_schedule$nominal_effective_date, as.Date("2022-03-02"))
  expect_equal(bundle$roll_schedule$execution_date, as.Date("2022-03-03"))
  expect_equal(bundle$roll_schedule$effective_date, as.Date("2022-03-04"))
  expect_equal(bundle$roll_schedule$grace_sessions_used, 2L)
  expect_true(bundle$roll_schedule$roll_delayed)
  expect_equal(bundle$roll_schedule$execution_days_to_last_trade, 89L)
  expect_match(
    bundle$roll_schedule$rejected_candidate_reasons,
    "missing_incoming_row:BGIM22"
  )
  expect_match(
    bundle$roll_schedule$rejected_candidate_reasons,
    "incoming_not_tradable:missing_or_invalid_ohlc"
  )
  expect_equal(
    bundle$contract_map$signal_contract,
    c("BGIK22", "BGIK22", "BGIM22", "BGIM22")
  )
  expect_equal(
    bundle$contract_map$execution_tradable,
    c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_equal(
    bundle$contract_map$signal_adjustment_factor,
    c(108 / 102, 108 / 102, 1, 1)
  )
  expect_equal(
    bundle$contract_map$execution_adjustment_factor,
    c(108 / 102, 108 / 102, 110 / 105, 1)
  )
  expect_equal(
    bundle$signal_series$active_contract,
    c("BGIK22", "BGIM22", "BGIM22")
  )
  expect_equal(
    bundle$signal_series$date,
    as.Date(c("2022-02-25", "2022-03-03", "2022-03-04"))
  )
  expect_equal(
    bundle$contract_map$signal_available,
    c(TRUE, FALSE, TRUE, TRUE)
  )
  expect_true(all(bundle$signal_series$open > 0))
  expect_equal(bundle$roll_schedule$from_settlement_price, 105)
  expect_equal(bundle$roll_schedule$to_settlement_price, 110)
  expect_equal(bundle$roll_schedule$adjustment_ratio, 110 / 105)
  expect_equal(bundle$manifest$delayed_roll_count, 1L)
  expect_equal(bundle$manifest$max_grace_sessions_used, 2L)
})

test_that("divergent order transforms use only the prior official settlement bridge", {
  expect_warning(
    baseline <- build_continuous_bundle(
      .continuous_bundle_causal_bridge_fixture(
        first_new_ohlc_valid = FALSE,
        current_new_settlement = 110
      ),
      root = "BGI",
      days_before_roll = 90,
      roll_grace_sessions = 3L,
      synthetic_ticker = "BGIFUT_B90"
    ),
    "BGIK22->BGIM22.*2 session"
  )
  expect_warning(
    changed_current_pa <- build_continuous_bundle(
      .continuous_bundle_causal_bridge_fixture(
        first_new_ohlc_valid = FALSE,
        current_new_settlement = 120
      ),
      root = "BGI",
      days_before_roll = 90,
      roll_grace_sessions = 3L,
      synthetic_ticker = "BGIFUT_B90"
    ),
    "BGIK22->BGIM22.*2 session"
  )

  date <- as.Date("2022-03-03")
  baseline_row <- match(date, baseline$contract_map$date)
  changed_row <- match(date, changed_current_pa$contract_map$date)

  expect_equal(
    baseline$contract_map$execution_adjustment_factor[[baseline_row]],
    110 / 105
  )
  expect_equal(
    changed_current_pa$contract_map$execution_adjustment_factor[[changed_row]],
    120 / 105
  )
  expect_equal(
    baseline$contract_map$order_transform_factor[[baseline_row]],
    108 / 102
  )
  expect_equal(
    changed_current_pa$contract_map$order_transform_factor[[changed_row]],
    baseline$contract_map$order_transform_factor[[baseline_row]]
  )
  expect_equal(
    baseline$contract_map$order_transform_asof_date[[baseline_row]],
    as.Date("2022-03-02")
  )
  expect_lt(
    baseline$contract_map$order_transform_asof_date[[baseline_row]],
    baseline$contract_map$date[[baseline_row]]
  )
  expect_true(baseline$contract_map$order_transform_available[[baseline_row]])
  expect_equal(
    baseline$contract_map$order_transform_source[[baseline_row]],
    "prior_official_settlement_bridge"
  )
})

test_that("a same-day signal anchor cannot transform orders until the next session", {
  expect_warning(
    bundle <- build_continuous_bundle(
      .continuous_bundle_causal_bridge_fixture(first_new_ohlc_valid = TRUE),
      root = "BGI",
      days_before_roll = 90,
      roll_grace_sessions = 3L,
      synthetic_ticker = "BGIFUT_B90"
    ),
    "BGIK22->BGIM22.*1 session"
  )

  anchor_date <- as.Date("2022-03-02")
  anchor_row <- match(anchor_date, bundle$contract_map$date)
  next_row <- anchor_row + 1L

  expect_equal(
    bundle$contract_map$signal_adjustment_anchor_date[[anchor_row]],
    anchor_date
  )
  expect_equal(
    bundle$contract_map$signal_first_observed_date[[anchor_row]],
    anchor_date
  )
  expect_equal(
    bundle$contract_map$signal_coordinate_asof_date[[anchor_row]],
    anchor_date
  )
  expect_false(bundle$contract_map$order_transform_available[[anchor_row]])
  expect_true(is.na(bundle$contract_map$order_transform_factor[[anchor_row]]))
  expect_equal(
    bundle$contract_map$order_transform_source[[anchor_row]],
    "unavailable"
  )

  expect_true(bundle$contract_map$order_transform_available[[next_row]])
  expect_equal(bundle$contract_map$order_transform_factor[[next_row]], 1)
  expect_lt(
    bundle$contract_map$order_transform_asof_date[[next_row]],
    bundle$contract_map$date[[next_row]]
  )
})

test_that("settlement-only carried sessions are marked but never declared tradable", {
  dates <- as.Date(c("2011-11-30", "2011-12-01", "2011-12-06", "2011-12-07"))
  old <- data.frame(
    date = dates,
    root = "BGI",
    ticker = "BGIG12",
    maturity = as.Date("2012-02-29"),
    last_trade_date = as.Date("2012-02-29"),
    open = c(97.5, 0, 97.0, 97.2),
    high = c(98.0, 0, 97.6, 97.8),
    low = c(97.0, 0, 96.8, 97.0),
    close = c(97.5, 0, 97.3, 97.4),
    settlement_price = c(98.20, 97.81, 97.30, 97.40),
    volume = c(10, 0, 8, 6),
    stringsAsFactors = FALSE
  )
  new <- data.frame(
    date = dates,
    root = "BGI",
    ticker = "BGIH12",
    maturity = as.Date("2012-03-30"),
    last_trade_date = as.Date("2012-03-30"),
    open = c(0, 97.2, 96.2, 96.5),
    high = c(0, 97.8, 96.8, 97.0),
    low = c(0, 97.0, 96.0, 96.3),
    close = c(0, 97.5, 96.5, 96.8),
    settlement_price = c(97.75, 97.45, 96.50, 96.80),
    volume = c(0, 12, 15, 20),
    stringsAsFactors = FALSE
  )

  expect_warning(
    bundle <- build_continuous_bundle(
      .continuous_bundle_with_observed_clocks(rbind(old, new)),
      root = "BGI",
      days_before_roll = 90,
      roll_grace_sessions = 3L,
      synthetic_ticker = "BGIFUT_B90"
    ),
    "BGIG12->BGIH12.*2 session"
  )

  expect_equal(
    bundle$contract_map$active_contract,
    c("BGIG12", "BGIG12", "BGIG12", "BGIH12")
  )
  expect_equal(
    bundle$contract_map$signal_contract,
    c("BGIG12", "BGIH12", "BGIH12", "BGIH12")
  )
  expect_equal(
    bundle$contract_map$execution_tradable,
    c(TRUE, FALSE, TRUE, TRUE)
  )
  expect_true(all(bundle$contract_map$marking_supported))
  expect_equal(bundle$contract_map$data_quality[[2L]], "settlement_only_no_trade")
  expect_equal(bundle$manifest$settlement_only_session_count, 1L)
  expect_true(bundle$manifest$execution_supported)
  expect_equal(bundle$roll_schedule$execution_date, as.Date("2011-12-06"))
  expect_equal(bundle$roll_schedule$effective_date, as.Date("2011-12-07"))
  expect_match(
    bundle$roll_schedule$rejected_candidate_reasons,
    "outgoing_not_tradable:missing_or_invalid_ohlc"
  )
})

test_that("roll grace fails closed before carrying the old contract past expiry", {
  expect_error(
    build_continuous_bundle(
      .continuous_bundle_no_bridge_before_expiry_fixture(),
      "TST",
      1,
      roll_grace_sessions = 0L
    ),
    "no executable bridge before last_trade_date"
  )
})

test_that("roll prebuffer records an early signed offset and execution-ahead state", {
  expect_warning(
    bundle <- build_continuous_bundle(
      .continuous_bundle_fixture(),
      "TST",
      1,
      roll_grace_sessions = 1L
    ),
    "1 session\\(s\\) early"
  )

  expect_equal(bundle$roll_schedule$execution_date, as.Date("2024-01-10"))
  expect_equal(bundle$roll_schedule$effective_date, as.Date("2024-01-11"))
  expect_equal(bundle$roll_schedule$execution_offset_sessions, -1L)
  expect_equal(bundle$roll_schedule$early_sessions_used, 1L)
  expect_true(bundle$roll_schedule$roll_early)
  expect_false(bundle$roll_schedule$roll_delayed)
  expect_equal(
    bundle$contract_map$active_contract,
    c("TSTF24", "TSTG24", "TSTG24")
  )
  expect_equal(
    bundle$contract_map$nominal_active_contract,
    c("TSTF24", "TSTF24", "TSTG24")
  )
  expect_equal(
    bundle$contract_map$roll_mapping_state,
    c("aligned", "execution_ahead", "aligned")
  )
})

test_that("pending roll intent coalesces to the latest nominal target", {
  expect_warning(
    bundle <- build_continuous_bundle(
      .continuous_bundle_coalesced_target_fixture(),
      "TST",
      1,
      roll_grace_sessions = 1L
    ),
    "skipped nominal TSTJ24"
  )

  expect_equal(bundle$roll_schedule$from_contract, "TSTH24")
  expect_equal(bundle$roll_schedule$requested_nominal_contract, "TSTK24")
  expect_equal(bundle$roll_schedule$to_contract, "TSTK24")
  expect_equal(bundle$roll_schedule$target_selection_reason, "nominal_target")
  expect_equal(bundle$roll_schedule$skipped_nominal_contracts, "TSTJ24")
  expect_equal(bundle$roll_schedule$skipped_nominal_count, 1L)
  expect_equal(bundle$manifest$coalesced_roll_count, 1L)
  expect_equal(bundle$manifest$skipped_nominal_contract_count, 1L)
})

test_that("late primary target can use the nearest executable later successor", {
  expect_warning(
    bundle <- build_continuous_bundle(
      .continuous_bundle_successor_fallback_fixture(),
      "TST",
      1,
      roll_grace_sessions = 3L
    ),
    "skipped nominal TSTJ24"
  )

  expect_equal(bundle$roll_schedule$from_contract, "TSTH24")
  expect_equal(bundle$roll_schedule$requested_nominal_contract, "TSTJ24")
  expect_equal(bundle$roll_schedule$to_contract, "TSTK24")
  expect_equal(
    bundle$roll_schedule$target_selection_reason,
    "nearest_executable_later_successor"
  )
  expect_equal(bundle$roll_schedule$execution_date, as.Date("2024-01-03"))
  expect_equal(bundle$roll_schedule$execution_offset_sessions, 2L)
  expect_equal(bundle$roll_schedule$skipped_nominal_contracts, "TSTJ24")
  expect_equal(bundle$manifest$successor_fallback_count, 1L)
})

test_that("roll selection never consults the following session mark", {
  raw <- .continuous_bundle_fixture()
  raw$settlement_price[
    raw$ticker == "TSTG24" &
      raw$date == as.Date("2024-01-12")
  ] <- NA_real_

  bundle <- build_continuous_bundle(
    raw,
    "TST",
    1,
    roll_grace_sessions = 0L,
    strict = FALSE
  )

  expect_equal(bundle$roll_schedule$execution_date, as.Date("2024-01-11"))
  expect_equal(bundle$roll_schedule$effective_date, as.Date("2024-01-12"))
  expect_equal(bundle$roll_schedule$to_contract, "TSTG24")
  next_row <- match(as.Date("2024-01-12"), bundle$contract_map$date)
  expect_equal(bundle$contract_map$active_contract[[next_row]], "TSTG24")
  expect_false(bundle$contract_map$marking_supported[[next_row]])
  expect_false(bundle$manifest$execution_supported)
})

test_that("exact mapping never infers a missing last trade date", {
  raw <- .continuous_bundle_fixture()
  raw$last_trade_date <- NULL

  expect_error(
    build_continuous_bundle(raw, "TST", 1),
    "Missing required column `last_trade_date`"
  )
})

test_that("exact mapping preserves settlement-only active rows and rejects absent successors", {
  invalid_ohlc <- .continuous_bundle_fixture()
  invalid_ohlc$low[
    invalid_ohlc$ticker == "TSTF24" &
      invalid_ohlc$date == as.Date("2024-01-10")
  ] <- 102
  invalid_bundle <- build_continuous_bundle(invalid_ohlc, "TST", 1)
  expect_false(invalid_bundle$contract_map$execution_tradable[[1L]])
  expect_false(invalid_bundle$contract_map$signal_available[[1L]])
  expect_true(invalid_bundle$contract_map$marking_supported[[1L]])

  no_successor <- subset(.continuous_bundle_fixture(), ticker == "TSTF24")
  expect_error(
    build_continuous_bundle(no_successor, "TST", 1),
    "exact mapping cannot invent a successor"
  )
})

test_that("exact mapping does not roll early when the expected row is absent", {
  raw <- .continuous_bundle_fixture()
  raw <- raw[!(
    raw$ticker == "TSTF24" & raw$date == as.Date("2024-01-11")
  ), , drop = FALSE]

  expect_error(
    build_continuous_bundle(raw, "TST", 1),
    "exact mapping cannot switch early"
  )

  non_strict <- build_continuous_bundle(raw, "TST", 1, strict = FALSE)
  expect_false(non_strict$manifest$mapping_complete)
  expect_false(non_strict$manifest$execution_supported)
  expect_equal(non_strict$manifest$unmapped_sessions, as.Date("2024-01-11"))
  expect_equal(
    non_strict$manifest$unmapped_reasons,
    "missing_active_contract_row:TSTF24"
  )
})

test_that("continuous bundle v4 preserves economic execution without invented clocks", {
  absent <- .continuous_bundle_fixture()
  absent$available_at <- NULL
  absent$settlement_available_at <- NULL
  without_clock <- build_continuous_bundle(
    absent,
    "TST",
    1,
    roll_grace_sessions = 0L
  )
  expect_true(without_clock$manifest$execution_supported)
  expect_true(
    without_clock$manifest$daily_session_phase_execution_supported
  )
  expect_false(without_clock$manifest$observed_clock_complete)
  expect_false(
    without_clock$manifest$heterogeneous_same_close_supported
  )
  expect_s3_class(without_clock$contract_map$available_at, "POSIXct")
  expect_s3_class(
    without_clock$contract_map$settlement_available_at,
    "POSIXct"
  )
  expect_true(all(is.na(without_clock$contract_map$available_at)))
  expect_true(all(is.na(
    without_clock$contract_map$settlement_available_at
  )))
  expect_true(all(is.na(without_clock$execution_data$available_at)))
  expect_true(all(is.na(
    without_clock$execution_data$settlement_available_at
  )))

  date_only <- .continuous_bundle_fixture()
  date_only$available_at <- date_only$date
  date_only$settlement_available_at <- date_only$date
  expect_error(
    build_continuous_bundle(date_only, "TST", 1),
    "observed POSIXct timestamp, not a Date/storage key"
  )
})

test_that("continuous bundle v4 validates UTC, completeness, and causality", {
  non_utc <- .continuous_bundle_fixture()
  non_utc$available_at <- as.POSIXct(
    format(non_utc$available_at, tz = "America/Sao_Paulo"),
    tz = "America/Sao_Paulo"
  )
  expect_error(
    build_continuous_bundle(non_utc, "TST", 1),
    "explicitly tagged UTC"
  )

  partial <- .continuous_bundle_fixture()
  partial$settlement_available_at[[1L]] <- as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = "UTC"
  )
  partial_bundle <- build_continuous_bundle(
    partial,
    "TST",
    1,
    roll_grace_sessions = 0L
  )
  expect_true(partial_bundle$manifest$execution_supported)
  expect_false(partial_bundle$manifest$observed_clock_complete)
  expect_false(
    partial_bundle$manifest$heterogeneous_same_close_supported
  )

  orphan <- .continuous_bundle_fixture()
  orphan$available_at[[1L]] <- as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = "UTC"
  )
  expect_error(
    build_continuous_bundle(orphan, "TST", 1),
    "cannot carry settlement_available_at without"
  )

  before_session <- .continuous_bundle_fixture()
  before_session$available_at[[1L]] <- as.POSIXct(
    "2024-01-09 23:59:59",
    tz = "UTC"
  )
  before_session$settlement_available_at[[1L]] <-
    before_session$available_at[[1L]]
  expect_error(
    build_continuous_bundle(before_session, "TST", 1),
    "cannot precede session"
  )

  reversed <- .continuous_bundle_fixture()
  reversed$settlement_available_at[[1L]] <-
    reversed$available_at[[1L]] + 1
  expect_error(
    build_continuous_bundle(reversed, "TST", 1),
    "available_at cannot precede settlement_available_at"
  )
})

test_that("continuous bundle validates supported configuration explicitly", {
  raw <- .continuous_bundle_fixture()
  expect_error(
    build_continuous_bundle(raw, "TST", 1, adjustment_anchor = "close"),
    "supports only adjustment_anchor"
  )
  expect_error(
    build_continuous_bundle(raw, "TST", 1, roll_date_col = "maturity"),
    "requires roll_date_col"
  )
  expect_error(
    build_continuous_bundle(raw, "TST", 1, strict = NA),
    "must be TRUE or FALSE"
  )
  expect_error(
    build_continuous_bundle(raw, "TST", 1, roll_grace_sessions = 1.5),
    "single non-negative integer"
  )
})
