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
    "<tr><td rowspan='2'>VENCTO</td><td colspan='5'>Cotacoes</td></tr>",
    "<tr><td>VENCTO</td><td>PRECO ABERT.</td><td>PRECO MIN.</td><td>PRECO MAX.</td><td>ULT. PRECO</td><td>AJUSTE</td></tr>",
    "<tr><td>WINM24</td><td>120.000</td><td>115.000</td><td>125.000</td><td>123.000</td><td>122.500</td></tr>",
    "</table></div>",
    "<td>ATUALIZADO EM 02/04/2024</td>",
    "</body></html>"
  )
  writeLines(html, con = path)
}

test_that("table parser fixes malformed sup tags in headers", {
  tmp <- tempfile(fileext = ".html")
  html <- c(
    "<html><body>",
    "<div id='MercadoFut2'></div>",
    "<script>",
    "var MercFut1 = '';",
    "MercFut1 = MercFut1 + '<table><tr><td>AJUSTE ANTER. <sup>(3)</sup</td><td>AJUSTE CORRIG. <sup>(4)</sup</td><td>PRECO ABERT.</td><td>PRECO MIN.</td></tr><tr><td>99.999,99</td><td>100.041,63</td><td>0,000</td><td>0,000</td></tr></table>';",
    "MercadoFut2.innerHTML = MercFut1;",
    "</script>",
    "</body></html>"
  )
  writeLines(html, tmp)
  doc <- xml2::read_html(tmp)
  tbl <- brfutures:::`.brf_table_from_doc`(doc, "MercadoFut2")
  expect_equal(
    names(tbl),
    c("AJUSTE ANTER. (3)", "AJUSTE CORRIG. (4)", "PRECO ABERT.", "PRECO MIN.")
  )
  expect_equal(nrow(tbl), 1)
  unlink(tmp)
})

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

test_that("parsed cache stores canonical daily data", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  raw_path <- file.path(cache, "WIN", "raw", "WIN_2024-04-02.html")
  dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
  sample_html(raw_path)
  register_calls <- character()
  registrar <- function(paths) {
    register_calls <<- c(register_calls, paths)
  }
  first <- brfutures:::`.brf_sync_parsed_report`(raw_path, "WIN", registrar, quiet = TRUE)
  expect_true(first$updated)
  expect_false(first$no_data)
  parsed_path <- brfutures:::`.brf_parsed_path`("WIN", as.Date("2024-04-02"))
  expect_true(file.exists(parsed_path))
  stored <- readRDS(parsed_path)
  expect_true(all(c("contract_code", "ticker", "open", "low", "high", "close", "settlement_price") %in% names(stored)))
  expect_equal(stored$open, 120000)
  mtime_before <- file.info(parsed_path)$mtime
  second <- brfutures:::`.brf_sync_parsed_report`(raw_path, "WIN", registrar, quiet = TRUE)
  expect_null(second)
  expect_equal(file.info(parsed_path)$mtime, mtime_before)
  forced <- brfutures:::`.brf_sync_parsed_report`(raw_path, "WIN", registrar, force = TRUE, quiet = TRUE)
  expect_false(forced$updated)
  expect_s3_class(forced$data, "data.frame")
  expect_equal(forced$data$open, 120000)
  expect_length(register_calls, 0)
})

test_that("composite price header is normalized", {
  composite <- data.frame(
    contract_code = "TESTF24",
    preco_med_ult_precoajuste_var_ptos_ult_of_comprault_of_venda = "12,345",
    stringsAsFactors = FALSE
  )
  cleaned <- brfutures:::`.brf_clean_parsed_dataframe`(
    composite,
    root = "TEST",
    source_path = "TEST_2024-01-02.html"
  )
  expect_true("average_price" %in% names(cleaned))
  expect_true(is.numeric(cleaned$average_price))
  expect_equal(cleaned$average_price, 12.345)
  extra_cols <- intersect(c("close", "settlement_price", "change_points", "last_bid", "last_ask"), names(cleaned))
  if (length(extra_cols)) {
    expect_true(all(vapply(cleaned[extra_cols], function(col) all(is.na(col)), logical(1))))
  }
})

test_that("DI futures headers retain settlement and bids", {
  raw <- data.frame(
    contract_code = "DI1F15",
    `ULT. PRECOAJUSTE			VAR. PTOS. ULT. OF. COMPRAULT. OF. VENDA` = "11,796",
    V8 = "99.206,69",
    V9 = "0,73+",
    V10 = "0,000",
    V11 = "0,123",
    stringsAsFactors = FALSE
  )
  cleaned <- brfutures:::`.brf_clean_parsed_dataframe`(raw, root = "DI1", source_path = "DI1_2024-01-07.html")
  expect_equal(cleaned$close, 11.796)
  expect_equal(cleaned$settlement_price, 99206.69)
  expect_equal(cleaned$change_points, 0.73)
  expect_equal(cleaned$last_bid, 0)
  expect_equal(cleaned$last_ask, 0.123)
})

test_that("standard treatment renames truncated bulletin columns", {
  raw_di <- data.frame(
    date = as.Date("2015-01-06"),
    root = "DI1",
    contract_code = "DI1F25",
    ticker = "DI1F25",
    contr = "48.807",
    abert_1 = "91.608.705",
    fech_2 = "510.540.971",
    num = "324.799.848",
    stringsAsFactors = FALSE
  )
  standard_di <- brfutures:::`.brf_standard_treatment`(raw_di)
  expect_true(all(c("contracts_traded", "open_interest", "close_interest", "volume") %in% names(standard_di)))
  expect_equal(standard_di$contracts_traded, 48807)
  expect_equal(standard_di$open_interest, 91608705)
  expect_equal(standard_di$close_interest, 510540971)
  expect_equal(standard_di$volume, 324799848)

  raw_ccm <- data.frame(
    date = as.Date("2015-01-02"),
    root = "CCM",
    contract_code = "CCMF15",
    ticker = "CCMF15",
    contr = "8.117",
    abert_1 = "0",
    fech_2 = "248",
    num = "3.268.354",
    stringsAsFactors = FALSE
  )
  standard_ccm <- brfutures:::`.brf_standard_treatment`(raw_ccm)
  expect_true(all(c("contracts_traded", "open_interest", "close_interest", "volume") %in% names(standard_ccm)))
  expect_equal(standard_ccm$contracts_traded, 8117)
  expect_equal(standard_ccm$open_interest, 0)
  expect_equal(standard_ccm$close_interest, 248)
  expect_equal(standard_ccm$volume, 3268354)
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
    open_interest = 1234,
    close_interest = 4321,
    trade_count = 10,
    contracts_traded = 500,
    volume = 1000,
    open = 120000,
    low = 115000,
    high = 125000,
    close = 123000,
    settlement_price = 122500,
    stringsAsFactors = FALSE
  )
  saveRDS(data, file.path(root_dir, "WIN.rds"))
  data_di <- data
  data_di$root <- "DI1"
  data_di$contract_code <- "DI1M24"
  data_di$ticker <- "DI1M24"
  data_di$open_interest <- 10
  data_di$close_interest <- 10
  data_di$trade_count <- 0
  data_di$contracts_traded <- 0
  data_di$volume <- 0
  data_di$open <- 0
  data_di$low <- 0
  data_di$high <- 0
  data_di$close <- 0
  data_di$settlement_price <- 0
  data_bgi <- data
  data_bgi$date <- as.Date("1998-12-30")
  data_bgi$root <- "BGI"
  data_bgi$contract_code <- "BGIF99"
  data_bgi$ticker <- "BGIF99"
  data_bgi$open_interest <- 987
  data_bgi$close_interest <- 678
  data_bgi$trade_count <- 4
  data_bgi$contracts_traded <- 150
  data_bgi$volume <- 1500
  agg_data <- rbind(data, data_di, data_bgi)
  saveRDS(agg_data, file.path(cache, "aggregate.rds"))
  xts_result <- get_brfut("WINM24")
  expect_true(xts::is.xts(xts_result))
  expect_equal(NROW(xts_result), 1)
  expect_equal(as.numeric(xts_result$Open), 120000)
  raw_df <- get_brfut("WINM24", treatment = "raw")
  expect_true(all(c(
    "open_interest",
    "close_interest",
    "trade_count",
    "contracts_traded",
    "volume",
    "open",
    "low",
    "high",
    "close",
    "settlement_price"
  ) %in% names(raw_df)))
  expect_equal(raw_df$open, 120000)
  expect_equal(raw_df$volume, 1000)
  standard_df <- get_brfut("WINM24", treatment = "standard")
  expect_equal(standard_df$open, 120000)
  standard_tbl <- get_brfut("WINM24", treatment = "standard_tibble")
  expect_s3_class(standard_tbl, "tbl_df")
  custom <- get_brfut("WINM24", treatment = function(df) nrow(df))
  expect_equal(custom, 1L)
  agg_all <- get_brfut_agg()
  expect_equal(nrow(agg_all), 3)
  expect_equal(sort(unique(agg_all$root)), c("BGI", "DI1", "WIN"))
  removed_cols <- c("contract_code", "contracts_traded")
  expect_false(any(removed_cols %in% names(agg_all)))
  key_cols <- c("volume_qty", "volume", "open", "low", "high", "close")
  expect_true(all(key_cols %in% names(agg_all)))
  expect_true(all(vapply(agg_all[key_cols], is.numeric, logical(1))))
  expect_true("maturity" %in% names(agg_all))
  expect_equal(
    agg_all$maturity[agg_all$ticker == "WINM24"],
    as.Date("2024-06-12")
  )
  expect_equal(
    agg_all$maturity[agg_all$ticker == "DI1M24"],
    as.Date("2024-06-03")
  )
  expect_equal(
    agg_all$maturity[agg_all$ticker == "BGIF99"],
    as.Date("1999-01-29")
  )
  expect_true("BGIF99" %in% agg_all$ticker)
  agg_win <- get_brfut_agg(start = "2024-04-01", end = "2024-04-05", root = "WIN")
  expect_equal(nrow(agg_win), 1)
  expect_equal(agg_win$root, "WIN")
  expect_false("contracts_traded" %in% names(agg_win))
  expect_equal(agg_win$open, 120000)
  agg_di <- get_brfut_agg(root = "DI1")
  expect_equal(unique(agg_di$root), "DI1")
  expect_true(all(c("volume_qty", "volume") %in% names(agg_di)))
  expect_equal(agg_di$volume, 0)
  agg_drop0 <- get_brfut_agg(treatment = "clean_data_drop0")
  expect_equal(nrow(agg_drop0), 2)
  expect_equal(sort(unique(agg_drop0$root)), c("BGI", "WIN"))
  agg_regular <- get_brfut_agg(root = "WIN", treatment = "regular")
  expect_true("open_interest" %in% names(agg_regular))
  expect_equal(agg_regular$open_interest, 1234)
})

test_that("corrupted root cache is rebuilt automatically", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  raw_dir <- file.path(cache, "WIN", "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  html_path <- file.path(raw_dir, "WIN_2024-04-02.html")
  sample_html(html_path)
  root_rds <- file.path(cache, "WIN", "WIN.rds")
  writeLines("not an rds file", root_rds)
  data <- brfutures:::`.brf_load_root_data`("WIN")
  expect_s3_class(data, "data.frame")
  expect_true(nrow(data) > 0)
  expect_equal(unique(data$root), "WIN")
  expect_error(readRDS(root_rds), NA)
})

test_that("corrupted aggregate cache is rebuilt automatically", {
  cache <- tempfile("brf-cache-")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  old_opt <- getOption("brfutures.cache_dir")
  on.exit({
    options(brfutures.cache_dir = old_opt)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  options(brfutures.cache_dir = cache)
  root_dir <- file.path(cache, "WIN")
  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)
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
  agg_path <- file.path(cache, "aggregate.rds")
  writeLines("not an rds file", agg_path)
  result <- get_brfut("WINM24", treatment = "raw")
  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$ticker), "WINM24")
  expect_error(readRDS(agg_path), NA)
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
