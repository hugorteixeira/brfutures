test_that("bounded annual assembly preserves chronological replacement and column types", {
  ns <- asNamespace("brfutures")
  legacy_merge <- function(existing, date, day) {
    existing$date <- as.Date(existing$date)
    existing <- existing[!existing$date %in% date, , drop = FALSE]
    combined <- ns$.brf_bind_rows(list(existing, day))
    combined$date <- as.Date(combined$date)
    combined <- ns$.brf_align_bulletin_schema(combined)
    combined[order(combined$date, combined$contract_code), , drop = FALSE]
  }
  base <- data.frame(date = as.Date(c("2026-03-02", "2026-03-03", "2026-03-04")),
                     contract_code = c("WINJ26", "BGIK26", "WINJ26"),
                     close = c(10L, 20L, 30L), legacy = c("a", "b", "c"))
  day1 <- data.frame(date = as.Date(c("2026-03-03", "2026-03-04")),
                     contract_code = c("BGIK26", "WINJ26"), close = c(21.5, 999),
                     available_at = as.POSIXct(c("2026-03-03 22:00:00", NA), tz = "UTC"))
  day2 <- data.frame(date = as.Date(c("2026-03-04", "2026-03-03")),
                     contract_code = c("WINJ26", "WINJ26"), close = c(32, 33),
                     available_at = as.POSIXct(c("2026-03-04 22:00:00", "2026-03-04 22:01:00"), tz = "UTC"),
                     extra_date = as.Date(c("2026-04-01", NA)))
  attr(base, "source_generation") <- "retained-source-generation"
  dates <- as.Date(c("2026-03-03", "2026-03-04"))
  expected <- legacy_merge(legacy_merge(base, dates[[1L]], day1), dates[[2L]], day2)
  actual <- ns$.brf_bvbg_merge_year_days(base, dates, list(day1, day2))
  rownames(expected) <- rownames(actual) <- NULL
  expect_identical(actual, expected)
  expect_equal(actual$close, c(10, 21.5, 33, 32))
  expect_s3_class(actual$available_at, "POSIXct")
  rebuilt <- ns$.brf_bvbg_bind_ordered(list(day1, day2), list(1:2, 1:2))
  all_rows <- ns$.brf_align_bulletin_schema(ns$.brf_bind_rows(list(day1, day2)))
  all_rows <- all_rows[order(all_rows$date, all_rows$contract_code), ]
  rownames(all_rows) <- NULL
  expect_identical(rebuilt, all_rows)
})

test_that("XML resume verifies values, repairs internal gaps and revisits corrected days", {
  ns <- asNamespace("brfutures")
  cache <- tempfile("brf-memory-resume-")
  withr::local_options(list(brfutures.cache_dir = cache,
                           brfutures.xml_cutover_date = as.Date("2026-01-02")))
  withr::defer(unlink(cache, recursive = TRUE))
  dates <- as.Date(c("2026-01-02", "2026-01-05", "2026-01-06"))
  days <- lapply(seq_along(dates), function(i) data.frame(
    date = dates[[i]], contract_code = "WINJ26", close = 100 + i,
    source = "xml", source_sha256 = paste0("day-", i),
    available_at = as.POSIXct(paste(dates[[i]], "23:00:00"), tz = "UTC")
  ))
  for (i in seq_along(dates)) ns$.brf_bvbg_save_parsed_day(dates[[i]], days[[i]])
  # Legacy HTML establishes the old resume bound, while XML has newer days.
  writeLines("cached HTML", file.path(ns$.brf_raw_dir("WIN"), "WIN_2026-01-01.html"))
  ns$.brf_save_parsed_day("WIN", as.Date("2026-01-01"), days[[1L]][0, ])
  current <- ns$.brf_prepare_root_data("WIN", ns$.brf_bind_rows(days[c(1L, 3L)]))
  current$close[[2L]] <- 1
  ns$.brf_save_root_data("WIN", current)
  raw_hashes <- vapply(dates, function(date) digest::digest(file = ns$.brf_bvbg_parsed_path(date)), character(1))
  save_root <- ns$.brf_save_root_data
  testthat::local_mocked_bindings(
    brf_b3_prices_fetch = function(...) stop("Unexpected source acquisition"),
    .env = ns
  )
  update_brfut("WIN", end = max(dates), quiet = TRUE, rebuild_agg = FALSE)
  actual <- ns$.brf_load_root_data("WIN")
  expected <- ns$.brf_prepare_root_data("WIN", ns$.brf_bind_rows(days))
  rownames(actual) <- rownames(expected) <- NULL
  expect_equal(actual, expected)
  expect_length(ns$.brf_bvbg_pending_year_paths("2026"), 0L)
  update_brfut("WIN", end = max(dates), quiet = TRUE, rebuild_agg = FALSE)
  expect_equal(ns$.brf_load_root_data("WIN"), actual)
  expect_identical(vapply(dates, function(date) digest::digest(file = ns$.brf_bvbg_parsed_path(date)), character(1)), raw_hashes)

  # A new earlier checkpoint must be applied even when the root's maximum date
  # already equals end; a missing middle day must also be restored.
  days[[1L]]$close <- 105
  days[[1L]]$source_sha256 <- "corrected-day-1"
  ns$.brf_bvbg_save_parsed_day(dates[[1L]], days[[1L]])
  save_root("WIN", actual[actual$date != dates[[2L]], ])
  update_brfut("WIN", end = max(dates), quiet = TRUE, rebuild_agg = FALSE)
  repaired <- ns$.brf_load_root_data("WIN")
  expect_equal(repaired$date, dates)
  expect_equal(repaired$close, c(105, 102, 103))
  expect_equal(repaired$source_sha256, c("corrected-day-1", "day-2", "day-3"))

  # Even when all XML days match, legacy HTML rows still pass through the
  # established root normalization before publication.
  legacy <- repaired[1L, ]
  legacy$date <- as.Date("2025-12-12")
  legacy$contract_code <- legacy$ticker <- "WINZ25"
  legacy$source <- ""
  save_root("WIN", ns$.brf_bind_rows(list(legacy, repaired)))
  update_brfut("WIN", end = max(dates), quiet = TRUE, rebuild_agg = FALSE)
  normalized <- ns$.brf_load_root_data("WIN")
  expect_identical(normalized$source[normalized$date == legacy$date], "html")
  expect_equal(normalized$close[normalized$date == legacy$date], legacy$close)
})
