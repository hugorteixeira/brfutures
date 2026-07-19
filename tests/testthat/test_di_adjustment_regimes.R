di_regime_rows <- function(date, ticker, settlement, previous, corrected, change) {
  data.frame(
    date = as.Date(rep(date, length(ticker))),
    root = "DI1",
    contract_code = ticker,
    ticker = ticker,
    source = "html",
    settlement_price = settlement,
    previous_settlement = previous,
    corrected_settlement = corrected,
    change_points = change,
    stringsAsFactors = FALSE
  )
}

test_that("DI HTML adjustment semantics switch only inside the observed regime", {
  rows <- rbind(
    di_regime_rows(
      "2018-09-21",
      c("DI1F23", "DI1F24", "DI1F25"),
      c(63753.61, 56402.17, 49757.01),
      c(63328.30, 55876.39, 49111.12),
      c(63328.30, 55876.39, 49111.12),
      c(425.31, 525.78, 645.89)
    ),
    di_regime_rows(
      "2018-09-24",
      c("DI1F23", "DI1F24", "DI1F25"),
      c(63658.30, 56267.60, 49584.56),
      c(63753.61, 56402.17, 49757.01),
      c(63769.28, 56416.03, 49769.24),
      c(-95.31, -134.57, -172.45)
    ),
    di_regime_rows(
      "2019-06-28",
      c("DI1F23", "DI1F24", "DI1F25"),
      c(79810.11, 74020.16, 68484.67),
      c(79553.72, 73658.88, 68045.07),
      c(79573.31, 73677.01, 68061.82),
      c(256.39, 361.28, 439.60)
    ),
    di_regime_rows(
      "2019-07-01",
      c("DI1F23", "DI1F24", "DI1F25"),
      c(79961.77, 74226.75, 68750.10),
      c(79829.76, 74038.38, 68501.53),
      c(79829.76, 74038.38, 68501.53),
      c(132.01, 188.37, 248.57)
    )
  )

  out <- brfutures:::`.brf_add_di_adjustment_columns`(rows)

  before <- out$date == as.Date("2018-09-21")
  expect_equal(out$di_adjustment_base[before], rows$previous_settlement[before])
  expect_equal(out$di_adjustment_points[before], rows$change_points[before])
  expect_equal(
    out$di_adjustment_quality[before],
    rep("official_reported_change_points", sum(before))
  )

  first <- out$date == as.Date("2018-09-24")
  expect_equal(out$di_adjustment_base[first], rows$corrected_settlement[first])
  expect_equal(out$di_adjustment_points[first], c(-110.98, -148.43, -184.68))
  expect_equal(
    out$di_adjustment_quality[first],
    rep("official_corrected_adjusted_quote", sum(first))
  )

  last <- out$date == as.Date("2019-06-28")
  expect_equal(out$di_adjustment_base[last], rows$corrected_settlement[last])
  expect_equal(out$di_adjustment_points[last], c(236.80, 343.15, 422.85))
  expect_equal(
    out$di_adjustment_quality[last],
    rep("official_corrected_adjusted_quote", sum(last))
  )

  after <- out$date == as.Date("2019-07-01")
  expect_equal(out$di_adjustment_base[after], rows$previous_settlement[after])
  expect_equal(out$di_adjustment_points[after], rows$change_points[after])
  expect_equal(
    out$di_adjustment_quality[after],
    rep("official_reported_change_points", sum(after))
  )

  expect_equal(out$corrected_settlement, rows$corrected_settlement)
  expect_equal(out$change_points, rows$change_points)
  expect_true(all(out$di_adjustment_is_official))
  expect_equal(
    out$settlement_price - out$di_adjustment_base,
    out$di_adjustment_points,
    tolerance = 1e-8
  )

  without_source <- rows
  without_source$source <- NULL
  inferred <- normalize_brfut_di_adjustments(without_source)
  expect_equal(inferred$di_adjustment_base, out$di_adjustment_base)
  expect_equal(inferred$di_adjustment_points, out$di_adjustment_points)
  expect_equal(inferred$di_adjustment_quality, out$di_adjustment_quality)
  expect_equal(inferred$di_adjustment_is_official, out$di_adjustment_is_official)

  bounded <- rows$date == as.Date("2018-09-24")
  explicit_non_html <- rows[bounded, , drop = FALSE]
  explicit_non_html$source <- "xml"
  not_reclassified <- normalize_brfut_di_adjustments(explicit_non_html)
  expect_equal(
    not_reclassified$di_adjustment_base,
    explicit_non_html$settlement_price - explicit_non_html$change_points
  )
  expect_equal(not_reclassified$di_adjustment_points, explicit_non_html$change_points)
  expect_equal(
    not_reclassified$di_adjustment_quality,
    rep("official_reported_change_points", nrow(explicit_non_html))
  )
})

test_that("DI corrected-base regime resolves launch and expiry layout outliers", {
  rows <- di_regime_rows(
    "2019-04-01",
    c("DI1F23", "DI1F24", "DI1F25", "DI1J24"),
    c(74593.63, 68089.32, 62082.90, 66596.99),
    c(74286.41, 67800.18, 61735.04, 66280.00),
    c(74304.70, 67816.87, 61750.24, 66613.39),
    c(307.22, 289.14, 347.86, 316.99)
  )

  out <- brfutures:::`.brf_add_di_adjustment_columns`(rows)
  launch <- out$ticker == "DI1J24"

  expect_equal(out$di_adjustment_base[launch], 66296.32)
  expect_equal(out$di_adjustment_points[launch], 300.67)
  expect_equal(
    out$di_adjustment_quality[launch],
    "official_corrected_adjusted_quote"
  )
  expect_equal(out$corrected_settlement[launch], 66613.39)
  expect_equal(out$change_points[launch], 316.99)
})

test_that("DI corrected-base regime fails closed without a cross-sectional factor", {
  rows <- di_regime_rows(
    "2019-01-02",
    c("DI1F23", "DI1F24"),
    c(72497.13, 65943.68),
    c(72077.68, 65478.28),
    c(72113.18, 65510.53),
    c(419.45, 465.40)
  )

  out <- brfutures:::`.brf_add_di_adjustment_columns`(rows)

  expect_true(all(is.na(out$di_adjustment_base)))
  expect_true(all(is.na(out$di_adjustment_points)))
  expect_true(all(out$di_adjustment_quality == "missing_base"))
  expect_false(any(out$di_adjustment_is_official))
})

test_that("DI corrected-base regime rejects three divergent ratios as consensus", {
  rows <- di_regime_rows(
    "2019-01-02",
    c("DI1F23", "DI1F24", "DI1F25"),
    c(80100, 80100, 80100),
    c(80000, 80000, 80000),
    c(80010, 80020, 80030),
    c(100, 100, 100)
  )

  out <- normalize_brfut_di_adjustments(rows)

  expect_true(all(is.na(out$di_adjustment_base)))
  expect_true(all(is.na(out$di_adjustment_points)))
  expect_true(all(out$di_adjustment_quality == "missing_base"))
  expect_false(any(out$di_adjustment_is_official))
})

test_that("DI XML adjustment uses the official previous adjusted quote", {
  rows <- data.frame(
    date = as.Date("2025-12-16"),
    root = "DI1",
    contract_code = "DI1F26",
    ticker = "DI1F26",
    source = "xml",
    settlement_price = 99395.45,
    previous_settlement = 99395.44,
    corrected_settlement = NA_real_,
    change_points = NA_real_,
    stringsAsFactors = FALSE
  )

  out <- brfutures:::`.brf_add_di_adjustment_columns`(rows)

  expect_equal(out$di_adjustment_base, 99395.44)
  expect_equal(out$di_adjustment_points, 0.01, tolerance = 1e-8)
  expect_equal(out$di_adjustment_quality, "official_previous_adjusted_quote")
  expect_true(out$di_adjustment_is_official)
})

di_zero_ohlc_repair_rows <- function() {
  data.frame(
    date = rep(as.Date(c("2021-06-09", "2021-06-10")), each = 4L),
    root = "DI1",
    contract_code = rep(c("DI1N23", "DI1F24", "DI1N24", "DI1V24"), 2L),
    ticker = rep(c("DI1N23", "DI1F24", "DI1N24", "DI1V24"), 2L),
    source = "html",
    maturity = rep(
      as.Date(c("2023-07-03", "2024-01-02", "2024-07-01", "2024-10-01")),
      2L
    ),
    open = c(7.110, 7.400, 7.630, 0, 7.205, 0, 7.630, 0),
    high = c(7.230, 7.490, 7.690, 0, 7.360, 0, 7.820, 0),
    low = c(7.110, 7.390, 7.600, 0, 7.180, 0, 7.630, 0),
    close = c(7.210, 7.470, 7.690, 0, 7.350, 0, 7.800, 0),
    average_price = c(7.199, 7.457, 7.675, 0, 7.270, 7.509, 7.689, 0),
    trade_count = c(1311, 11807, 760, 0, 2563, 14655, 1578, 0),
    contracts_traded = c(
      36575, 205490, 25660, 0,
      37920, 257530, 53015, 0
    ),
    volume = c(1, 1, 1, 0, 1, 1, 1, 0),
    settlement_price = c(
      86658.89, 83194.64, 79833.94, 78015.48,
      86508.54, 82991.61, 79609.55, 77787.95
    ),
    previous_settlement = c(
      86700, 83300, 79900, 78100,
      86658.89, 83194.64, 79833.94, 78015.48
    ),
    change_points = c(-41.11, -105.36, -66.06, -84.52, -150.35, -203.03, -224.39, -227.53),
    last_bid = c(NA, NA, NA, NA, 7.35, 7.60, 7.80, NA),
    last_ask = c(NA, NA, NA, NA, 7.36, 7.61, 7.81, NA),
    stringsAsFactors = FALSE
  )
}

test_that("DI repairs only proven traded all-zero legacy bulletin OHLC", {
  rows <- di_zero_ohlc_repair_rows()
  out <- brfutures:::`.brf_agg_di_adjustments_treatment`(rows)
  target <- out$date == as.Date("2021-06-10") & out$ticker == "DI1F24"
  no_trade <- out$date == as.Date("2021-06-10") & out$ticker == "DI1V24"

  expect_true(out$ohlc_repaired[target])
  expect_identical(
    out$ohlc_repair_method[target],
    "prior_session_cross_sectional_vwap_settlement_v1"
  )
  expect_identical(
    out$ohlc_repair_status[target],
    "modelled_traded_bulletin_zero_ohlc"
  )
  expect_identical(out$ohlc_repair_source_contracts[target], "DI1N23,DI1N24")
  expect_identical(out$ohlc_repair_neighbor_mode[target], "bracket_interpolation")
  expect_equal(out$ohlc_repair_prior_session_date[target], as.Date("2021-06-09"))
  expect_equal(
    unname(unlist(out[target, paste0("ohlc_original_", c("open", "high", "low", "close"))])),
    rep(0, 4L)
  )
  expect_true(all(is.finite(unlist(out[target, c("open", "high", "low", "close")]))))
  expect_lte(out$low[target], min(out$open[target], out$close[target], out$average_price[target]))
  expect_gte(out$high[target], max(out$open[target], out$close[target], out$average_price[target]))

  expect_false(out$ohlc_repaired[no_trade])
  expect_true(all(unlist(out[no_trade, c("open", "high", "low", "close")]) == 0))
  expect_true(is.na(out$ohlc_repair_method[no_trade]))
})

test_that("DI continuous strict coverage retains a repaired traded bulletin day", {
  rows <- di_zero_ohlc_repair_rows()
  series <- build_continuous_di(
    rows,
    target_tenor = 3,
    tenor_unit = "years",
    allowed_maturities = "F",
    selection_mode = "calendar_horizon",
    coverage_mode = "restart_strict_suffix",
    add_attrs = FALSE,
    add_globalenv = FALSE
  )

  expect_identical(
    as.character(as.Date(zoo::index(series))),
    c("2021-06-09", "2021-06-10")
  )
  expect_identical(
    attr(series, "active_contracts")$contract_symbol,
    c("DI1F24", "DI1F24")
  )
  expect_true(all(as.numeric(series[, "RateOpenRaw"]) > 0))
})
