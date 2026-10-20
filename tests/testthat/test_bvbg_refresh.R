refresh_fixture <- function(date, price = 100) {
  data.frame(date = as.Date(date), contract_code = "WINZ26", close = price,
    settlement_price = price, source_sha256 = paste0("sha-", price), source = "xml")
}

test_that("failed ZIP entries retry across roots and repair an internal date", {
  cache <- tempfile("retry-cache-")
  withr::local_options(list(brfutures.cache_dir = cache))
  withr::defer(unlink(cache, recursive = TRUE))
  ns <- asNamespace("brfutures")
  date <- as.Date("2026-10-09")
  ns$.brf_register_no_data_zip("ALL", date, quiet = TRUE)
  ns$.brf_save_root_data("WIN", ns$.brf_prepare_root_data("WIN",
    rbind(refresh_fixture("2026-10-08"), refresh_fixture("2026-10-19"))))
  calls <- 0L
  now <- as.POSIXct("2026-10-20 21:00:00", tz = "UTC")
  available <- FALSE
  testthat::local_mocked_bindings(
    .brf_bvbg_now = function() now,
    brf_b3_prices_fetch = function(date, ...) {
      calls <<- calls + 1L
      if (!available) stop("HTTP temporary failure")
      refresh_fixture(date)
    }, .env = ns)
  # Explicit bounds retry old absence; the second root shares the cooldown.
  update_brfut(c("WIN", "BGI"), start = date, end = date, quiet = TRUE, rebuild_agg = FALSE)
  expect_equal(calls, 1L)
  expect_equal(nrow(ns$.brf_load_root_data("WIN")), 2L)
  available <- TRUE
  now <- now + 7 * 3600
  update_brfut("WIN", end = "2026-10-09", quiet = TRUE, rebuild_agg = FALSE)
  expect_equal(calls, 2L)
  expect_true(date %in% ns$.brf_load_root_data("WIN")$date)
  expect_false(date %in% ns$.brf_no_data_entries("WIN", "zip")$date)
  expect_true(date %in% ns$.brf_bvbg_year_data("2026")$date)
})

test_that("recent snapshots revalidate once per day and preserve good data on failure", {
  cache <- tempfile("refresh-cache-")
  withr::local_options(list(brfutures.cache_dir = cache))
  withr::defer(unlink(cache, recursive = TRUE))
  ns <- asNamespace("brfutures")
  date <- as.Date("2026-10-16")
  now <- as.POSIXct("2026-10-16 22:00:00", tz = "UTC")
  price <- 100
  calls <- refreshes <- 0L
  fail <- FALSE
  testthat::local_mocked_bindings(
    .brf_bvbg_now = function() now,
    brf_b3_prices_fetch = function(date, refresh, ...) {
      calls <<- calls + 1L
      refreshes <<- refreshes + as.integer(refresh)
      if (fail) stop("HTTP temporary failure")
      refresh_fixture(date, price)
    }, .env = ns)
  update_brfut("WIN", start = date, end = date, quiet = TRUE, rebuild_agg = FALSE)
  expect_equal(calls, 1L)
  now <- now + 23 * 3600
  price <- 105
  update_brfut("WIN", end = date, quiet = TRUE, rebuild_agg = FALSE)
  expect_equal(calls, 2L)
  expect_equal(refreshes, 1L)
  expect_equal(ns$.brf_load_root_data("WIN")$close, 105)
  before <- digest::digest(file = ns$.brf_bvbg_parsed_path(date))
  update_brfut("WIN", end = date, quiet = TRUE, rebuild_agg = FALSE)
  expect_equal(calls, 2L)
  fail <- TRUE
  now <- now + 24 * 3600
  update_brfut("WIN", end = date, quiet = TRUE, rebuild_agg = FALSE)
  expect_equal(calls, 3L)
  expect_identical(digest::digest(file = ns$.brf_bvbg_parsed_path(date)), before)
  expect_equal(ns$.brf_load_root_data("WIN")$close, 105)
  expect_equal(ns$.brf_bvbg_attempt_read(date)$status, "unavailable")
  # The same failed refresh does not get retried once per commodity root.
  update_brfut("WIN", end = date, quiet = TRUE, rebuild_agg = FALSE)
  expect_equal(calls, 3L)
})
