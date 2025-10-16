build_di_contract_df <- function(symbol, start_date, open_rates, maturity) {
  stopifnot(length(open_rates) >= 3)
  ref_dates <- as.Date(start_date) + seq_along(open_rates) - 1L
  high_rates <- open_rates + 0.05
  low_rates <- open_rates - 0.05
  close_rates <- open_rates + 0.02
  valid_days <- as.numeric(as.Date(maturity) - ref_dates)
  pu_from <- function(r_vec) brfutures:::.di_pu_from_rate(r_vec, valid_days)
  data.frame(
    refdate = ref_dates,
    symbol = symbol,
    ticker = symbol,
    open = open_rates,
    high = high_rates,
    low = low_rates,
    close = close_rates,
    volume = seq_along(open_rates) * 10,
    estimated_maturity = as.Date(maturity),
    PU_o = pu_from(open_rates),
    PU_h = pu_from(high_rates),
    PU_l = pu_from(low_rates),
    PU_c = pu_from(close_rates),
    stringsAsFactors = FALSE
  )
}

test_that("DI continuous builder restores rate OHLC when using notional stitching", {
  front_df <- build_di_contract_df(
    "DI1F25",
    as.Date("2024-12-18"),
    open_rates = c(11.10, 11.15, 11.05, 11.00, 10.95, 10.90),
    maturity = as.Date("2025-01-02")
  )
  next_df <- build_di_contract_df(
    "DI1G25",
    as.Date("2024-12-23"),
    open_rates = c(11.25, 11.28, 11.30, 11.32, 11.35, 11.38),
    maturity = as.Date("2025-02-03")
  )
  aggregate_df <- rbind(front_df, next_df)
  roll_spec <- brfutures:::.brf_resolve_roll_spec(
    brfutures::days_before_roll(
      days_before_expiry = 5,
      include_roll_details = TRUE
    )
  )
  ctx <- list(
    ticker_root = "DI1",
    aggregate_raw = aggregate_df,
    roll_spec = roll_spec,
    build_type = "fwd_rt",
    adjusted = FALSE,
    single_xts = TRUE,
    debug = FALSE,
    everything = TRUE,
    use_notional = TRUE,
    include_older = FALSE,
    maturities = "all"
  )
  res <- brfutures:::.brf_build_continuous_di(ctx)
  expect_type(res, "list")
  expect_true(all(c("aggregate", "no_adj", "adj", "daily") %in% names(res)))

  no_adj <- res$no_adj
  adj <- res$adj
  expect_s3_class(no_adj, "xts")
  expect_s3_class(adj, "xts")
  expect_true(all(c("Open", "High", "Low", "Close") %in% colnames(no_adj)))
  expect_true(all(c("PU_o", "PU_h", "PU_l", "PU_c") %in% colnames(no_adj)))
  expect_true(all(c("Open", "High", "Low", "Close") %in% colnames(adj)))
  expect_true(all(c("PU_o", "PU_h", "PU_l", "PU_c") %in% colnames(adj)))

  expect_lt(max(as.numeric(no_adj$Close), na.rm = TRUE), 100)
  expect_lt(max(as.numeric(adj$Close), na.rm = TRUE), 100)
  expect_gt(min(as.numeric(no_adj$Close), na.rm = TRUE), 5)

  idx_valid_no_adj <- is.finite(no_adj$PU_c) &
    is.finite(no_adj$DaysToMaturity) &
    no_adj$DaysToMaturity > 0
  idx_valid_adj <- is.finite(adj$PU_c) &
    is.finite(adj$DaysToMaturity) &
    adj$DaysToMaturity > 0
  expect_true(any(idx_valid_no_adj))
  expect_true(any(idx_valid_adj))

  rec_rates_no_adj <- brfutures:::.di_rate_from_pu(
    as.numeric(no_adj$PU_c[idx_valid_no_adj]),
    as.numeric(no_adj$DaysToMaturity[idx_valid_no_adj])
  )
  expect_equal(
    rec_rates_no_adj,
    as.numeric(no_adj$Close[idx_valid_no_adj]),
    tolerance = 1e-8
  )
  rec_rates_adj <- brfutures:::.di_rate_from_pu(
    as.numeric(adj$PU_c[idx_valid_adj]),
    as.numeric(adj$DaysToMaturity[idx_valid_adj])
  )
  expect_equal(
    rec_rates_adj,
    as.numeric(adj$Close[idx_valid_adj]),
    tolerance = 1e-8
  )
})
