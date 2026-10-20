test_that("official archive requests carry finite connection and transfer deadlines", {
  withr::local_options(list(brfutures.b3_reference_download_hook = NULL,
                           brfutures.b3_reference_timeout = 12))
  captured <- NULL
  testthat::local_mocked_bindings(RETRY = function(...) {
    captured <<- list(...)
    stop("fixture transfer timeout")
  }, .package = "httr")
  expect_error(brfutures:::.brf_b3_download_daily_archive(as.Date("2026-10-09"),
    "price_full", tempdir(), quiet = TRUE), "fixture transfer timeout")
  configs <- Filter(function(x) inherits(x, "request"), captured)
  opts <- unlist(lapply(configs, function(x) x$options))
  expect_equal(as.numeric(opts["timeout_ms"]), 12000)
  expect_equal(as.numeric(opts["connecttimeout"]), 10)
})
