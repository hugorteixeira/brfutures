library(testthat)
library(brfutures)

sample_html <- function(path) {
  html <- c(
    "<html><body>",
    "<div id='MercadoFut0'><table>",
    "<tr><td>VENCTO</td></tr>",
    "<tr><td>WINM24</td></tr>",
    "</table></div>",
    "<div id='MercadoFut1'><table>",
    "<tr><td>VENCTO</td><td>CONTR. ABERT.(1)</td><td>NUM. NEGOC.</td><td>CONTR. NEGOC.</td><td>VOL.</td></tr>",
    "<tr><td>WINM24</td><td>1.234</td><td>10</td><td>500</td><td>1.000</td></tr>",
    "</table></div>",
    "<div id='MercadoFut2'><table>",
    "<tr><td>VENCTO</td><td>PRECO ABERT.</td><td>PRECO MIN.</td><td>PRECO MAX.</td><td>ULT. PRECO</td><td>AJUSTE</td></tr>",
    "<tr><td>WINM24</td><td>120.000</td><td>115.000</td><td>125.000</td><td>123.000</td><td>122.500</td></tr>",
    "</table></div>",
    "<td>ATUALIZADO EM 02/04/2024</td>",
    "</body></html>"
  )
  writeLines(html, con = path)
}

test_that("update_brfut requires configured cache", {
  old_opt <- getOption("brfutures.cache_dir")
  on.exit(options(brfutures.cache_dir = old_opt), add = TRUE)
  options(brfutures.cache_dir = NULL)
  expect_error(
    update_brfut("WIN", start = "2024-01-01", end = "2024-01-05"),
    "Cache directory not configured"
  )
})

test_that("HTML parser keeps raw column names", {
  tmp <- tempfile(fileext = ".html")
  sample_html(tmp)
  parsed <- brfutures:::`.brf_parse_html_report`(tmp, "WIN")
  expect_s3_class(parsed, "data.frame")
  expect_equal(parsed$contract_code, "WINM24")
  expect_equal(parsed$root, "WIN")
  expect_equal(parsed$ticker, "WINM24")
  expect_equal(parsed$date, as.Date("2024-04-02"))
  expect_equal(parsed$`PRECO ABERT.`, "120.000")
  regular <- brfutures:::`.brf_regular_treatment`(parsed)
  expect_equal(regular$`PRECO ABERT.`, 120000)
  standard <- brfutures:::`.brf_standard_treatment`(parsed)
  expect_equal(standard$open, 120000)
})

test_that("get_brfut applies treatments", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  root_dir <- file.path(cache, "WIN")
  dir.create(file.path(root_dir, "raw"), recursive = TRUE, showWarnings = FALSE)
  data <- data.frame(
    date = as.Date("2024-04-02"),
    root = "WIN",
    contract_code = "WINM24",
    ticker = "WINM24",
    VENCTO = "WINM24",
    `CONTR. ABERT.(1)` = "1.234",
    `NUM. NEGOC.` = "10",
    `CONTR. NEGOC.` = "500",
    `VOL.` = "1.000",
    `PRECO ABERT.` = "120.000",
    `PRECO MIN.` = "115.000",
    `PRECO MAX.` = "125.000",
    `ULT. PRECO` = "123.000",
    AJUSTE = "122.500",
    stringsAsFactors = FALSE
  )
  saveRDS(data, file.path(root_dir, "WIN.rds"))
  saveRDS(data, file.path(cache, "aggregate.rds"))
  xts_result <- get_brfut("WINM24")
  expect_true(xts::is.xts(xts_result))
  expect_equal(NROW(xts_result), 1)
  expect_equal(as.numeric(xts_result$Open), 120000)
  raw_df <- get_brfut("WINM24", treatment = "raw")
  expect_equal(raw_df[["PRECO.ABERT."]], "120.000")
  expect_false("open" %in% names(raw_df))
  expect_false("volume" %in% names(raw_df))
  standard_df <- get_brfut("WINM24", treatment = "standard")
  expect_equal(standard_df$open, 120000)
  standard_tbl <- get_brfut("WINM24", treatment = "standard_tibble")
  expect_s3_class(standard_tbl, "tbl_df")
  custom <- get_brfut("WINM24", treatment = function(df) nrow(df))
  expect_equal(custom, 1L)
  agg <- get_brfut_agg(start = "2024-04-01", end = "2024-04-05")
  expect_equal(nrow(agg), 1)
  expect_equal(agg$root, "WIN")
})

test_that("update_brfut skips non-business days", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  download_log <- character()
  testthat::local_mocked_bindings(
    .brf_download_html = function(date, root, quiet) {
      download_log <<- c(download_log, format(date, "%Y-%m-%d"))
      dest <- file.path(brfutures:::`.brf_raw_dir`(root), paste0(root, "_", format(date, "%Y-%m-%d"), ".html"))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      writeLines("<html></html>", dest)
      dest
    },
    .brf_parse_html_report = function(path, root) {
      day <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.html$", "\\1", basename(path)))
      data.frame(
        date = day,
        root = root,
        contract_code = paste0(root, "X"),
        ticker = paste0(root, "X"),
        close = 1,
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("brfutures")
  )
  brfutures:::`.brf_update_root`("TST", as.Date("2025-01-03"), as.Date("2025-01-06"), quiet = TRUE)
  expect_equal(download_log, c("2025-01-03", "2025-01-06"))
  saved <- readRDS(file.path(cache, "TST", "TST.rds"))
  expect_equal(unique(saved$date), as.Date(c("2025-01-03", "2025-01-06")))
})

test_that("update_brfut skips dates listed in no-data.csv", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  raw_dir <- file.path(cache, "SKP", "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  skip_path <- file.path(raw_dir, "SKP_2025-01-06.html")
  writeLines("<html></html>", skip_path)
  utils::write.csv(
    data.frame(filename = "SKP_2025-01-06.html", stringsAsFactors = FALSE),
    file = file.path(cache, "no-data.csv"),
    row.names = FALSE,
    quote = TRUE
  )
  existing <- data.frame(
    date = as.Date(c("2025-01-03", "2025-01-06")),
    root = "SKP",
    contract_code = c("SKPX", "SKPY"),
    ticker = c("SKPX", "SKPY"),
    close = c(1, 2),
    stringsAsFactors = FALSE
  )
  saveRDS(existing, file.path(cache, "SKP", "SKP.rds"))
  download_log <- character()
  testthat::local_mocked_bindings(
    .brf_download_html = function(date, root, quiet) {
      download_log <<- c(download_log, format(date, "%Y-%m-%d"))
      dest <- file.path(brfutures:::`.brf_raw_dir`(root), paste0(root, "_", format(date, "%Y-%m-%d"), ".html"))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      writeLines("<html></html>", dest)
      dest
    },
    .brf_parse_html_report = function(path, root) {
      day <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.html$", "\\1", basename(path)))
      data.frame(
        date = day,
        root = root,
        contract_code = paste0(root, "X"),
        ticker = paste0(root, "X"),
        close = 42,
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("brfutures")
  )
  brfutures:::`.brf_update_root`("SKP", as.Date("2025-01-03"), as.Date("2025-01-07"), quiet = FALSE)
  expect_true("2025-01-03" %in% download_log)
  expect_false("2025-01-06" %in% download_log)
  expect_false(file.exists(skip_path))
  saved <- readRDS(file.path(cache, "SKP", "SKP.rds"))
  expect_false(as.Date("2025-01-06") %in% saved$date)
  expect_true(as.Date("2025-01-03") %in% saved$date)
})

test_that("update_brfut records downloaded reports with no data message", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  download_log <- character()
  raw_dir <- file.path(cache, "SKP", "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  testthat::local_mocked_bindings(
    .brf_download_html = function(date, root, quiet) {
      download_log <<- c(download_log, format(date, "%Y-%m-%d"))
      dest <- file.path(brfutures:::`.brf_raw_dir`(root), paste0(root, "_", format(date, "%Y-%m-%d"), ".html"))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (format(date, "%Y-%m-%d") == "2025-01-06") {
        writeLines(
          c(
            "<html><body>",
            "<p>Não há &nbsp;dados para a",
            "data &nbsp; consultada</p>",
            "</body></html>"
          ),
          dest
        )
      } else {
        sample_html(dest)
      }
      dest
    },
    .env = asNamespace("brfutures")
  )
  brfutures:::`.brf_update_root`("SKP", as.Date("2025-01-03"), as.Date("2025-01-07"), quiet = FALSE)
  expect_true("2025-01-06" %in% download_log)
  expect_false(file.exists(file.path(raw_dir, "SKP_2025-01-06.html")))
  no_data_file <- file.path(cache, "no-data.csv")
  expect_true(file.exists(no_data_file))
  entries <- utils::read.csv(no_data_file, stringsAsFactors = FALSE)
  expect_true("SKP_2025-01-06.html" %in% entries$filename)
  skip_entries <- brfutures:::`.brf_no_data_entries`("SKP")
  expect_true(as.Date("2025-01-06") %in% skip_entries$date)
  saved <- readRDS(file.path(cache, "SKP", "SKP.rds"))
  expect_false(as.Date("2025-01-06") %in% saved$date)
  before_second <- sum(download_log == "2025-01-06")
  brfutures:::`.brf_update_root`("SKP", as.Date("2025-01-03"), as.Date("2025-01-07"), quiet = TRUE)
  after_second <- sum(download_log == "2025-01-06")
  expect_equal(before_second, 1L)
  expect_equal(after_second, 1L)
})

test_that("update_brfut treats missing combo option as no data", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  download_log <- character()
  raw_dir <- file.path(cache, "CCM", "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  combo_html <- function(path) {
    writeLines(
      c(
        "<html><body>",
        "<select id='comboMerc1' name='cboMercadoria'>",
        "<option value='0'>Selecione a Mercadoria</option>",
        "<option value='IND  '>IND  : Ibovespa</option>",
        "<option value='DOL  '>DOL  : Dólar comercial</option>",
        "</select>",
        "</body></html>"
      ),
      con = path
    )
  }
  testthat::local_mocked_bindings(
    .brf_download_html = function(date, root, quiet) {
      download_log <<- c(download_log, format(date, "%Y-%m-%d"))
      dest <- file.path(brfutures:::`.brf_raw_dir`(root), paste0(root, "_", format(date, "%Y-%m-%d"), ".html"))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      combo_html(dest)
      dest
    },
    .env = asNamespace("brfutures")
  )
  update_brfut("CCM", start = as.Date("2000-01-28"), end = as.Date("2000-01-28"), quiet = TRUE)
  expect_equal(download_log, "2000-01-28")
  expect_false(file.exists(file.path(raw_dir, "CCM_2000-01-28.html")))
  no_data_file <- file.path(cache, "no-data.csv")
  expect_true(file.exists(no_data_file))
  entries <- utils::read.csv(no_data_file, stringsAsFactors = FALSE)
  expect_true("CCM_2000-01-28.html" %in% entries$filename)
  ccm_data_path <- file.path(cache, "CCM", "CCM.rds")
  expect_true(file.exists(ccm_data_path))
  ccm_data <- readRDS(ccm_data_path)
  expect_equal(nrow(ccm_data), 0)
})

test_that("update_brfut can defer aggregate rebuild", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  download_log <- character()
  testthat::local_mocked_bindings(
    .brf_download_html = function(date, root, quiet) {
      download_log <<- c(download_log, format(date, "%Y-%m-%d"))
      dest <- file.path(brfutures:::`.brf_raw_dir`(root), paste0(root, "_", format(date, "%Y-%m-%d"), ".html"))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      writeLines("<html></html>", dest)
      dest
    },
    .brf_parse_html_report = function(path, root) {
      day <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.html$", "\\1", basename(path)))
      data.frame(
        date = day,
        root = root,
        contract_code = paste0(root, "X"),
        ticker = paste0(root, "X"),
        close = 999,
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("brfutures")
  )
  agg_path <- file.path(cache, "aggregate.rds")
  expect_false(file.exists(agg_path))
  update_brfut("AGG", start = as.Date("2024-01-01"), end = as.Date("2024-01-02"), quiet = TRUE, rebuild_agg = FALSE)
  expect_false(file.exists(agg_path))
  expect_equal(length(download_log), 2L)
  agg <- get_brfut_agg(rebuild_agg = TRUE)
  expect_true(file.exists(agg_path))
  expect_true(all(agg$root == "AGG"))
  expect_true(all(as.Date(c("2024-01-01", "2024-01-02")) %in% agg$date))
})

test_that("get_brfut rebuilds aggregates when needed", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  download_log <- character()
  testthat::local_mocked_bindings(
    .brf_download_html = function(date, root, quiet) {
      download_log <<- c(download_log, format(date, "%Y-%m-%d"))
      dest <- file.path(brfutures:::`.brf_raw_dir`(root), paste0(root, "_", format(date, "%Y-%m-%d"), ".html"))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      writeLines("<html></html>", dest)
      dest
    },
    .brf_parse_html_report = function(path, root) {
      day <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.html$", "\\1", basename(path)))
      data.frame(
        date = day,
        root = root,
        contract_code = paste0(root, "X24"),
        ticker = paste0(root, "X24"),
        close = 123,
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("brfutures")
  )
  agg_path <- file.path(cache, "aggregate.rds")
  expect_false(file.exists(agg_path))
  update_brfut_agg(all = TRUE, quiet = TRUE)
  expect_true(file.exists(agg_path))
  agg_data <- readRDS(agg_path)
  expect_s3_class(agg_data, "data.frame")
  expect_equal(download_log, character())
})
