with_bvbg_batch_cache <- function(code) {
  cache <- tempfile("brf-year-batch-")
  old <- options(brfutures.cache_dir = cache,
                 brfutures.xml_cutover_date = as.Date("2025-12-15"))
  on.exit({ options(old); unlink(cache, recursive = TRUE) }, add = TRUE)
  force(code)
}

bvbg_batch_day <- function(date) {
  data.frame(
    date = as.Date(date), contract_code = c("WINZ26", "BGIF27"),
    close = c(100, 200) + as.integer(as.Date(date)) / 1000,
    settlement_price = c(101, 201), source_instrument_id = c("1", "2"),
    available_at = as.POSIXct(paste(date, "23:00:00"), tz = "UTC")
  )
}

test_that("XML batches publish each year once and reuse days across roots", {
  with_bvbg_batch_cache({
    ns <- asNamespace("brfutures")
    prior <- as.Date("2025-12-29")
    ns$.brf_bvbg_save_parsed_day(prior, bvbg_batch_day(prior))
    ns$.brf_bvbg_year_data("2025", quiet = TRUE)
    dates <- as.Date(c("2025-12-30", "2025-12-31", "2026-01-01", "2026-01-02"))
    downloads <- saves <- loads <- character()
    save_year <- ns$.brf_bvbg_save_year
    load_year <- ns$.brf_bvbg_load_year
    testthat::local_mocked_bindings(
      brf_b3_prices_fetch = function(date, ...) {
        downloads <<- c(downloads, as.character(date))
        bvbg_batch_day(date)
      },
      .brf_bvbg_save_year = function(year, data) {
        saves <<- c(saves, year)
        save_year(year, data)
      },
      .brf_bvbg_load_year = function(year) {
        loads <<- c(loads, year)
        load_year(year)
      }, .env = ns
    )
    update_brfut(c("WIN", "BGI"), start = min(dates), end = max(dates),
                 quiet = TRUE, rebuild_agg = FALSE)
    expect_equal(downloads, as.character(dates))
    expect_equal(saves, c("2025", "2026"))
    expect_equal(loads, c("2025", "2026"))
    year_2025 <- readRDS(ns$.brf_bvbg_year_path("2025"))
    expect_equal(unique(year_2025$date), c(prior, dates[1:2]))
    expect_equal(year_2025$close, unlist(lapply(c(prior, dates[1:2]), function(x) {
      rev(bvbg_batch_day(as.Date(x, origin = "1970-01-01"))$close)
    })))
    expect_s3_class(year_2025$available_at, "POSIXct")
    for (year in c("2025", "2026")) {
      expect_length(ns$.brf_bvbg_pending_year_paths(year), 0L)
    }
    for (root in c("WIN", "BGI")) {
      expect_equal(unique(readRDS(ns$.brf_root_data_path(root))$date), dates)
    }
    update_brfut(c("WIN", "BGI"), start = min(dates), end = max(dates),
                 quiet = TRUE, rebuild_agg = FALSE)
    expect_equal(downloads, as.character(dates))
    expect_equal(saves, c("2025", "2026"))
    expect_equal(loads, c("2025", "2026"))
  })
})

test_that("interrupted acquisition retains daily progress and annual reads reconcile it", {
  with_bvbg_batch_cache({
    ns <- asNamespace("brfutures")
    dates <- as.Date(c("2026-03-02", "2026-03-03", "2026-03-04"))
    ns$.brf_bvbg_save_parsed_day(as.Date("2026-02-27"), bvbg_batch_day("2026-02-27"))
    ns$.brf_bvbg_year_data("2026", quiet = TRUE)
    year_path <- ns$.brf_bvbg_year_path("2026")
    before <- digest::digest(file = year_path)
    downloads <- character()
    interrupted <- TRUE
    testthat::local_mocked_bindings(
      brf_b3_prices_fetch = function(date, ...) {
        downloads <<- c(downloads, as.character(date))
        if (interrupted && date == dates[[3L]]) {
          stop(structure(list(message = "fixture interrupt", call = NULL),
                         class = c("interrupt", "condition")))
        }
        bvbg_batch_day(date)
      }, .env = ns
    )
    caught <- tryCatch({
      update_brfut("WIN", start = min(dates), end = max(dates), quiet = TRUE,
                   rebuild_agg = FALSE)
      FALSE
    }, interrupt = function(e) TRUE)
    expect_true(caught)
    expect_identical(digest::digest(file = year_path), before)
    expect_length(ns$.brf_bvbg_pending_year_paths("2026"), 2L)
    completed <- ns$.brf_bvbg_year_data("2026", quiet = TRUE)
    expect_equal(unique(completed$date), c(as.Date("2026-02-27"), dates[1:2]))
    expect_equal(downloads, as.character(dates))
    expect_length(ns$.brf_bvbg_pending_year_paths("2026"), 0L)
    interrupted <- FALSE
    update_brfut("WIN", start = min(dates), end = max(dates), quiet = TRUE,
                 rebuild_agg = FALSE)
    expect_equal(downloads, as.character(c(dates, dates[[3L]])))
    expect_equal(unique(ns$.brf_bvbg_year_data("2026")$date),
                 c(as.Date("2026-02-27"), dates))
  })
})

test_that("failed annual publication preserves the old file and resumes without downloads", {
  with_bvbg_batch_cache({
    ns <- asNamespace("brfutures")
    dates <- as.Date(c("2026-03-02", "2026-03-03", "2026-03-04"))
    ns$.brf_bvbg_save_parsed_day(as.Date("2026-02-27"), bvbg_batch_day("2026-02-27"))
    ns$.brf_bvbg_year_data("2026", quiet = TRUE)
    year_path <- ns$.brf_bvbg_year_path("2026")
    before <- digest::digest(file = year_path)
    fail <- TRUE
    downloads <- character()
    atomic <- ns$.brf_b3_atomic_save_rds
    atomic_env <- new.env(parent = environment(atomic))
    atomic_env$file.rename <- function(from, to) {
      if (fail && identical(to, year_path)) return(FALSE)
      base::file.rename(from, to)
    }
    environment(atomic) <- atomic_env
    testthat::local_mocked_bindings(
      brf_b3_prices_fetch = function(date, ...) {
        downloads <<- c(downloads, as.character(date))
        bvbg_batch_day(date)
      },
      .brf_b3_atomic_save_rds = atomic, .env = ns
    )
    expect_error(update_brfut("WIN", start = min(dates), end = max(dates),
                             quiet = TRUE, rebuild_agg = FALSE), "atomically publish")
    expect_identical(digest::digest(file = year_path), before)
    expect_length(ns$.brf_bvbg_pending_year_paths("2026"), 3L)
    expect_length(list.files(dirname(year_path), pattern = "^\\.partial-", all.files = TRUE), 0L)
    fail <- FALSE
    update_brfut("WIN", start = min(dates), end = max(dates), quiet = TRUE,
                 rebuild_agg = FALSE)
    expect_equal(downloads, as.character(dates))
    expect_equal(unique(ns$.brf_bvbg_year_data("2026")$date),
                 c(as.Date("2026-02-27"), dates))
    expect_length(ns$.brf_bvbg_pending_year_paths("2026"), 0L)
  })
})

test_that("incomplete pending days cannot silently publish a partial annual cache", {
  with_bvbg_batch_cache({
    ns <- asNamespace("brfutures")
    prior <- as.Date("2026-03-02")
    date <- as.Date("2026-03-03")
    ns$.brf_bvbg_save_parsed_day(prior, bvbg_batch_day(prior))
    ns$.brf_bvbg_year_data("2026", quiet = TRUE)
    before <- digest::digest(file = ns$.brf_bvbg_year_path("2026"))
    fail <- TRUE
    atomic <- ns$.brf_b3_atomic_save_rds
    testthat::local_mocked_bindings(
      .brf_b3_atomic_save_rds = function(object, path, ...) {
        if (fail && grepl("03-03-parsed.rds$", path)) stop("fixture daily failure")
        atomic(object, path, ...)
      },
      brf_b3_prices_fetch = function(date, ...) bvbg_batch_day(date), .env = ns
    )
    expect_error(update_brfut("WIN", start = date, end = date, quiet = TRUE),
                 "fixture daily failure")
    expect_error(ns$.brf_bvbg_year_data("2026"), "daily cache is incomplete")
    expect_identical(digest::digest(file = ns$.brf_bvbg_year_path("2026")), before)
    expect_length(ns$.brf_bvbg_pending_year_paths("2026"), 1L)
    fail <- FALSE
    update_brfut("WIN", start = date, end = date, quiet = TRUE, rebuild_agg = FALSE)
    expect_equal(unique(ns$.brf_bvbg_year_data("2026")$date), c(prior, date))
    expect_length(ns$.brf_bvbg_pending_year_paths("2026"), 0L)
  })
})
