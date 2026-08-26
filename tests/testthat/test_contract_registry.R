contract_registry_lifecycle_xml <- function(root,
                                            ticker,
                                            expiry_date,
                                            app_created_at,
                                            group_id,
                                            instrument_id,
                                            contract_multiplier) {
  c(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
    "<Document xmlns=\"urn:bvmf.052.01.xsd\">",
    "<BizFileHdr><Xchg><BizGrpDesc><BizGrpDtls>",
    paste0("<BizGrpIdr>", group_id, "</BizGrpIdr>"),
    "<BizGrpTp>BVBG.028.02</BizGrpTp>",
    "<CreDtAndTm>2025-12-23T18:48:25</CreDtAndTm>",
    "</BizGrpDtls></BizGrpDesc></Xchg></BizFileHdr>",
    "<BizGrp>",
    "<AppHdr xmlns=\"urn:iso:std:iso:20022:tech:xsd:head.001.001.01\">",
    paste0("<BizMsgIdr>MSG-", group_id, "</BizMsgIdr>"),
    paste0("<CreDt>", app_created_at, "</CreDt>"),
    "</AppHdr><Document xmlns=\"urn:bvmf.100.02.xsd\"><Instrm>",
    "<RptParams><ActvtyInd>true</ActvtyInd>",
    "<RptDtAndTm><Dt>2025-12-23</Dt></RptDtAndTm><UpdTp>COMP</UpdTp>",
    "</RptParams><FinInstrmId><OthrId>",
    paste0("<Id>", instrument_id, "</Id>"),
    "</OthrId></FinInstrmId><FinInstrmAttrCmon>",
    paste0("<Asst>", root, "</Asst>"),
    "</FinInstrmAttrCmon><InstrmInf><FutrCtrctsInf>",
    paste0("<XprtnDt>", expiry_date, "</XprtnDt>"),
    paste0("<TckrSymb>", ticker, "</TckrSymb>"),
    "<TradgStartDt>2025-10-30</TradgStartDt>",
    paste0("<TradgEndDt>", expiry_date, "</TradgEndDt>"),
    paste0("<CtrctMltplr>", contract_multiplier, "</CtrctMltplr>"),
    "<TradgCcy>BRL</TradgCcy><ISIN>TESTISIN</ISIN>",
    "</FutrCtrctsInf></InstrmInf></Instrm></Document></BizGrp>",
    "</Document>"
  )
}

test_that("bundled registry contains unique causal official definitions", {
  old_cache <- getOption("brfutures.cache_dir")
  old_hook <- getOption("brfutures.b3_reference_download_hook")
  on.exit(options(
    brfutures.cache_dir = old_cache,
    brfutures.b3_reference_download_hook = old_hook
  ), add = TRUE)
  options(
    brfutures.cache_dir = NULL,
    brfutures.b3_reference_download_hook = function(...) {
      stop("ordinary registry reads must not access the network")
    }
  )

  registry <- brf_contract_registry()

  expect_gte(nrow(registry), 7483L)
  expect_equal(anyDuplicated(registry$ticker), 0L)
  expect_true(all(registry$official))
  expect_true(all(registry$date_quality == "official"))
  expect_true(all(registry$maturity_date >= as.Date("2018-01-01")))
  expect_true(all(is.finite(registry$multiplier)))
  expect_true(all(registry$multiplier > 0))
  expect_true(all(grepl(
    "^[0-9a-f]{64}$", registry$source_archive_sha256
  )))
  expect_true(all(grepl(
    "^[0-9a-f]{64}$", registry$source_snapshot_sha256
  )))
})

test_that("root specifications are centralized and DI tick facts stay dynamic", {
  specs <- brf_contract_specs()
  di <- specs[specs$root == "DI1", , drop = FALSE]
  win <- specs[specs$root == "WIN", , drop = FALSE]

  expect_equal(di$multiplier, 1)
  expect_equal(di$contract_size, 100000)
  expect_true(is.na(di$tick_size))
  expect_true(is.na(di$tick_value))
  expect_equal(win$multiplier, 0.2)
  expect_equal(win$tick_size, 5)
  expect_equal(win$tick_value, 1)
})

test_that("resolver uses official BVBG.028 facts from 2018 onward", {
  resolved <- brf_contract_resolve(
    c("WINV26", "DI1F29", "WDOU26", "BITZ25", "SOLZ25", "ETRZ25"),
    reference_date = as.Date("2026-08-26")
  )

  expect_equal(
    resolved$maturity_date,
    as.Date(c(
      "2026-10-14", "2029-01-02", "2026-09-01", "2025-12-23",
      "2025-12-23", "2025-12-23"
    ))
  )
  expect_equal(
    resolved$last_trade_date,
    as.Date(c(
      "2026-10-14", "2028-12-28", "2026-08-31", "2025-12-23",
      "2025-12-23", "2025-12-23"
    ))
  )
  expect_equal(resolved$multiplier, c(0.2, 1, 10, 0.01, 5, 0.25))
  expect_equal(resolved$contract_size, c(NA, 100000, 10000, 0.01, NA, NA))
  expect_true(all(resolved$official))
  expect_true(all(resolved$status == "resolved"))
})

test_that("resolver estimates only supported contracts before 2018", {
  resolved <- brf_contract_resolve(
    c("BGIZ12", "DI1K12", "CCMK10"),
    reference_date = as.Date("2016-01-04")
  )

  expect_equal(
    resolved$maturity_date,
    as.Date(c("2012-12-28", "2012-05-02", "2010-05-17"))
  )
  expect_equal(
    resolved$last_trade_date,
    as.Date(c("2012-12-28", "2012-04-30", "2010-05-17"))
  )
  expect_false(any(resolved$official))
  expect_true(all(resolved$date_quality == "estimated_historical"))
})

test_that("resolver never estimates a missing post-cutover contract", {
  diagnostic <- brf_contract_resolve(
    "CCMU34",
    reference_date = as.Date("2026-08-26"),
    strict = FALSE
  )

  expect_equal(diagnostic$contract_year, 2034L)
  expect_equal(diagnostic$status, "official_contract_not_found")
  expect_true(is.na(diagnostic$maturity_date))
  expect_error(
    brf_contract_resolve(
      "CCMU34", reference_date = as.Date("2026-08-26")
    ),
    "official_contract_not_found",
    fixed = TRUE
  )
})

test_that("generic BVBG.028 parser can retain all futures roots", {
  bit_path <- tempfile(fileext = ".xml")
  win_path <- tempfile(fileext = ".xml")
  on.exit(unlink(c(bit_path, win_path)), add = TRUE)
  writeLines(
    contract_registry_lifecycle_xml(
      root = "BIT", ticker = "BITZ25", expiry_date = "2025-12-23",
      app_created_at = "2025-12-23T21:47:59Z", group_id = "BIT-GROUP",
      instrument_id = "BIT-INSTRUMENT", contract_multiplier = 0.01
    ),
    bit_path,
    useBytes = TRUE
  )
  win_xml <- contract_registry_lifecycle_xml(
    root = "WIN", ticker = "WINV26", expiry_date = "2026-10-14",
    app_created_at = "2025-12-23T21:48:59Z", group_id = "WIN-GROUP",
    instrument_id = "WIN-INSTRUMENT", contract_multiplier = 0.2
  )
  writeLines(win_xml, win_path, useBytes = TRUE)

  lifecycle <- brf_b3_contract_lifecycle_read(
    c(bit_path, win_path), root = NULL
  )

  expect_equal(lifecycle$contract, c("BITZ25", "WINV26"))
  expect_equal(lifecycle$root, c("BIT", "WIN"))
  expect_equal(lifecycle$contract_multiplier, c(0.01, 0.2))
  expect_equal(lifecycle$contract_size_btc, c(0.01, NA))
})

test_that("explicit registry update publishes atomically without cycle I/O", {
  cache_dir <- tempfile("contract-registry-")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  lifecycle <- brfutures:::.brf_b3_lifecycle_empty()
  lifecycle <- lifecycle[rep(NA_integer_, 1L), , drop = FALSE]
  lifecycle$contract <- "TSTZ27"
  lifecycle$root <- "TST"
  lifecycle$report_date <- as.Date("2026-08-26")
  lifecycle$available_at <- as.POSIXct(
    "2026-08-26 22:00:00", tz = "UTC"
  )
  lifecycle$expiry_date <- as.Date("2027-12-31")
  lifecycle$last_trade_date <- as.Date("2027-12-30")
  lifecycle$contract_multiplier <- 2
  lifecycle$quote_currency <- "BRL"
  lifecycle$instrument_id <- "TST-INSTRUMENT"
  lifecycle$source_report_type <- "BVBG.028.02"
  lifecycle$source_message_id <- "TST-MESSAGE"
  lifecycle$source_archive_file <- "IN260826.zip"
  lifecycle$source_archive_sha256 <- paste(rep("a", 64L), collapse = "")
  lifecycle$source_sha256 <- paste(rep("b", 64L), collapse = "")
  local_mocked_bindings(
    brf_b3_contract_lifecycle_fetch = function(...) lifecycle,
    .package = "brfutures"
  )

  brf_b3_contract_registry_update(
    as.Date("2026-08-26"), cache_dir = cache_dir, quiet = TRUE
  )
  registry <- brf_contract_registry(cache_dir = cache_dir)
  resolved <- brf_contract_resolve(
    "TSTZ27", registry = registry,
    reference_date = as.Date("2026-08-26")
  )

  expect_true(file.exists(file.path(cache_dir, "b3-futures-contracts.rds")))
  expect_equal(resolved$maturity_date, as.Date("2027-12-31"))
  expect_equal(resolved$last_trade_date, as.Date("2027-12-30"))
  expect_equal(resolved$multiplier, 2)
  expect_true(resolved$official)
})

test_that("cached vector resolution is fast and preserves input order", {
  registry <- brf_contract_registry()
  tickers <- rep(registry$ticker[seq_len(1000L)], 10L)
  elapsed <- system.time({
    resolved <- brf_contract_resolve(tickers, registry = registry)
  })[["elapsed"]]

  expect_identical(resolved$ticker, tickers)
  expect_true(all(resolved$status == "resolved"))
  expect_lt(unname(elapsed), 2)
})
