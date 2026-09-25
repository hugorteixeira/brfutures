test_that("public cached readers return canonical date rules", {
  cache <- tempfile("brf-maturity-rules-")
  dir.create(cache)
  old <- options(
    brfutures.cache_dir = cache,
    brfutures.b3_reference_download_hook = function(...) stop("unexpected download")
  )
  on.exit({ options(old); unlink(cache, recursive = TRUE) }, add = TRUE)
  raw <- data.frame(
    root = c("WIN", "WIN", "BGI"),
    ticker = c("WINV26", "WINV26", "BGIZ12"),
    contract_code = c("WINV26", "WINV26", "BGIZ12"),
    date = as.Date(c("2026-08-25", "2026-08-26", "2012-12-20")),
    open = 100, high = 102, low = 99, close = 101,
    settlement_price = 101, contracts_traded = 20,
    open_interest = 40, volume = 2020,
    stringsAsFactors = FALSE
  )
  for (root in unique(raw$root)) {
    brfutures:::.brf_save_root_data(root, raw[raw$root == root, ])
  }
  update_brfut_agg(all = TRUE, rebuild_roots = FALSE, quiet = TRUE)
  readers <- list(
    aggregate = get_brfut_agg(treatment = "regular"),
    dated = get_brfut("WINV26", treatment = "regular", add_attrs = FALSE)
  )
  for (data in readers) {
    expected <- brf_contract_resolve(data$ticker, reference_date = data$date)
    expect_true(all(c("maturity_rule", "last_trade_rule") %in% names(data)))
    expect_identical(data$maturity, expected$maturity_date)
    expect_identical(data$last_trade_date, expected$last_trade_date)
    expect_identical(data$maturity_rule, expected$maturity_rule)
    expect_identical(data$last_trade_rule, expected$last_trade_rule)
  }
})

test_that("date rules only backfill dates matching the canonical source", {
  input <- data.frame(
    ticker = rep("WINV26", 5),
    date = rep(as.Date("2026-08-26"), 5),
    maturity = as.Date(c(NA, "2026-10-14", "2026-10-13", "2026-10-13", "2026-10-14")),
    last_trade_date = as.Date(c(NA, "2026-10-14", "2026-10-13", "2026-10-13", "2026-10-13")),
    maturity_rule = c(NA, "", NA, "published_expiry", NA),
    last_trade_rule = c(NA, " ", NA, "published_last_trade", NA),
    stringsAsFactors = FALSE
  )
  source <- brf_contract_resolve("WINV26", reference_date = as.Date("2026-08-26"))
  out <- brfutures:::.brf_estimate_maturity(input)
  expect_identical(out$maturity[2:5], input$maturity[2:5])
  expect_identical(out$last_trade_date[2:5], input$last_trade_date[2:5])
  expect_identical(out$maturity_rule[c(1, 2, 5)], rep(source$maturity_rule, 3))
  expect_identical(out$last_trade_rule[1:2], rep(source$last_trade_rule, 2))
  expect_true(is.na(out$maturity_rule[3]))
  expect_true(all(is.na(out$last_trade_rule[c(3, 5)])))
  expect_identical(out$maturity_rule[4], "published_expiry")
  expect_identical(out$last_trade_rule[4], "published_last_trade")
})

test_that("unresolved contract dates never receive invented provenance", {
  input <- data.frame(
    ticker = c("ZZZF17", "CCMU34"),
    date = as.Date(c("2016-12-01", "2026-08-26")),
    stringsAsFactors = FALSE
  )
  out <- brfutures:::.brf_estimate_maturity(input)
  expect_true(all(c("maturity_rule", "last_trade_rule") %in% names(out)))
  expect_true(all(is.na(out$maturity)))
  expect_true(all(is.na(out$last_trade_date)))
  expect_length(out$maturity_rule, 2)
  expect_length(out$last_trade_rule, 2)
  expect_true(all(is.na(out$maturity_rule)))
  expect_true(all(is.na(out$last_trade_rule)))
})
