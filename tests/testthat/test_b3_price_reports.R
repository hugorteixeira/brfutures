price_report_xml <- function(report_type,
                             created_at,
                             rows,
                             group_id = "PRICE-GROUP") {
  header <- c(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
    "<Document xmlns=\"urn:bvmf.052.01.xsd\">",
    "  <BizFileHdr><Xchg><BizGrpDesc><BizGrpDtls>",
    paste0("    <BizGrpIdr>", group_id, "</BizGrpIdr>"),
    paste0("    <BizGrpTp>", report_type, "</BizGrpTp>"),
    paste0("    <CreDtAndTm>", created_at, "</CreDtAndTm>"),
    "  </BizGrpDtls></BizGrpDesc></Xchg></BizFileHdr>"
  )
  groups <- unlist(lapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    liquidity <- if (grepl("^BVBG\\.086", report_type)) {
      c(
        paste0("          <FinInstrmQty>", row$contracts, "</FinInstrmQty>"),
        paste0(
          "          <RglrTraddCtrcts>",
          row$contracts,
          "</RglrTraddCtrcts>"
        ),
        paste0("          <NtlFinVol Ccy=\"BRL\">", row$volume, "</NtlFinVol>"),
        paste0("          <NtlRglrVol Ccy=\"BRL\">", row$volume, "</NtlRglrVol>")
      )
    } else {
      character()
    }
    c(
      "  <BizGrp>",
      "    <AppHdr xmlns=\"urn:iso:std:iso:20022:tech:xsd:head.001.001.01\">",
      paste0("      <BizMsgIdr>MSG-", index, "</BizMsgIdr>"),
      paste0("      <CreDt>", created_at, "</CreDt>"),
      "    </AppHdr>",
      "    <Document xmlns=\"urn:bvmf.217.01.xsd\">",
      "      <PricRpt>",
      paste0("        <TradDt><Dt>", row$date, "</Dt></TradDt>"),
      paste0(
        "        <SctyId><TckrSymb>",
        row$contract,
        "</TckrSymb></SctyId>"
      ),
      "        <FinInstrmId><OthrId>",
      paste0("          <Id>", row$instrument_id, "</Id>"),
      "        </OthrId></FinInstrmId>",
      "        <TradDtls>",
      paste0("          <TradQty>", row$trades, "</TradQty>"),
      "        </TradDtls>",
      "        <FinInstrmAttrbts>",
      "          <MktDataStrmId>E</MktDataStrmId>",
      paste0("          <OpnIntrst>", row$open_interest, "</OpnIntrst>"),
      liquidity,
      paste0("          <FrstPric Ccy=\"BRL\">", row$open, "</FrstPric>"),
      paste0("          <MinPric Ccy=\"BRL\">", row$low, "</MinPric>"),
      paste0("          <MaxPric Ccy=\"BRL\">", row$high, "</MaxPric>"),
      paste0("          <TradAvrgPric Ccy=\"BRL\">", row$average, "</TradAvrgPric>"),
      paste0("          <LastPric Ccy=\"BRL\">", row$close, "</LastPric>"),
      paste0("          <RglrTxsQty>", row$trades, "</RglrTxsQty>"),
      paste0("          <AdjstdQt Ccy=\"BRL\">", row$settlement, "</AdjstdQt>"),
      "          <AdjstdQtStin>F</AdjstdQtStin>",
      "        </FinInstrmAttrbts>",
      "      </PricRpt>",
      "    </Document>",
      "  </BizGrp>"
    )
  }))
  c(header, groups, "</Document>")
}

price_report_archive <- function(directory, prefix, xml_paths) {
  inner <- file.path(directory, paste0(prefix, ".zip"))
  outer <- file.path(directory, paste0("outer-", prefix, ".zip"))
  suppressWarnings(utils::zip(inner, files = xml_paths, flags = "-j"))
  suppressWarnings(utils::zip(outer, files = inner, flags = "-j"))
  outer
}

test_that("full PR cache selects only the final snapshot and is reparsable", {
  fixture_dir <- tempfile("b3-price-fixture-")
  cache_dir <- tempfile("b3-price-cache-")
  dir.create(fixture_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(c(fixture_dir, cache_dir), recursive = TRUE), add = TRUE)
  base_rows <- data.frame(
    date = c("2025-12-16", "2025-12-17"),
    contract = c("BGIF26", "BGIF26"),
    instrument_id = c("100000237982", "100000237982"),
    trades = c(7, 1),
    contracts = c(30, 1),
    volume = c(300000, 10000),
    open_interest = c(1000, 1001),
    open = c(320, 321),
    low = c(319, 320),
    high = c(322, 322),
    average = c(320.5, 321.5),
    close = c(321, 321.5),
    settlement = c(321.2, 321.6),
    stringsAsFactors = FALSE
  )
  full_xml <- file.path(
    fixture_dir,
    c("BVBG.086_EARLY.xml", "BVBG.086_MIDDLE.xml", "BVBG.086_FINAL.xml")
  )
  times <- c(
    "2025-12-16T21:00:00Z",
    "2025-12-16T22:00:00Z",
    "2025-12-16T23:00:00Z"
  )
  for (index in seq_along(full_xml)) {
    rows <- base_rows
    rows$contracts[[1L]] <- index * 10
    writeLines(
      price_report_xml("BVBG.086.01", times[[index]], rows),
      full_xml[[index]],
      useBytes = TRUE
    )
  }
  full_archive <- price_report_archive(fixture_dir, "PR251216", full_xml)
  simplified_xml <- file.path(fixture_dir, "BVBG.187_FINAL.xml")
  writeLines(
    price_report_xml(
      "BVBG.187.01",
      "2025-12-16T23:05:00Z",
      base_rows
    ),
    simplified_xml,
    useBytes = TRUE
  )
  simplified_archive <- price_report_archive(
    fixture_dir,
    "SPRD251216",
    simplified_xml
  )
  old_hook <- getOption("brfutures.b3_reference_download_hook")
  on.exit(
    options(brfutures.b3_reference_download_hook = old_hook),
    add = TRUE
  )
  options(brfutures.b3_reference_download_hook = function(url, destination) {
    source <- if (grepl("filelist=PR", url, fixed = TRUE)) {
      full_archive
    } else {
      simplified_archive
    }
    file.copy(source, destination, overwrite = TRUE)
  })

  full <- brf_b3_prices_fetch(
    as.Date("2025-12-16"),
    report = "full",
    root = "BGI",
    cache_dir = cache_dir,
    refresh = TRUE,
    quiet = TRUE
  )
  expect_equal(nrow(full), 1L)
  expect_equal(full$contracts_traded, 30)
  expect_equal(full$trade_count, 7)
  expect_equal(full$volume, 300000)
  manifest <- attr(full, "brf_b3_price_manifest")
  expect_equal(manifest$snapshot_count, 3L)
  expect_equal(manifest$selected_snapshot_file, basename(full_xml[[3L]]))
  expect_equal(sum(manifest$snapshots$selected), 1L)
  expect_equal(manifest$snapshots$created_at[manifest$snapshots$selected],
    as.POSIXct("2025-12-16 23:00:00", tz = "UTC"))
  expect_length(list.files(
    cache_dir,
    pattern = "\\.xml\\.gz$",
    recursive = TRUE
  ), 1L)
  expect_length(list.files(
    cache_dir,
    pattern = "\\.zip$",
    recursive = TRUE
  ), 0L)

  all_dates <- brf_b3_prices_fetch(
    as.Date("2025-12-16"),
    report = "full",
    root = "BGI",
    cache_dir = cache_dir,
    quiet = TRUE,
    all_trade_dates = TRUE
  )
  expect_equal(all_dates$date, as.Date(c("2025-12-16", "2025-12-17")))

  unlink(file.path(cache_dir, "prices", "full", "2025-12-16", manifest$parsed_path))
  options(brfutures.b3_reference_download_hook = function(url, destination) {
    stop("valid compressed snapshot attempted a download")
  })
  rebuilt <- brf_b3_prices_fetch(
    as.Date("2025-12-16"),
    report = "full",
    root = "BGI",
    cache_dir = cache_dir,
    quiet = TRUE
  )
  expect_equal(rebuilt$contracts_traded, 30)

  options(brfutures.b3_reference_download_hook = function(url, destination) {
    if (grepl("filelist=SPRD", url, fixed = TRUE)) {
      return(file.copy(simplified_archive, destination, overwrite = TRUE))
    }
    stop("completed full PR cache attempted a download")
  })
  comparison <- brf_b3_prices_compare(
    as.Date("2025-12-16"),
    root = "BGI",
    cache_dir = cache_dir,
    quiet = TRUE
  )
  expect_s3_class(comparison, "brf_b3_price_comparison")
  expect_equal(comparison$daily$matched_rows, 1L)
  expect_equal(nrow(comparison$unmatched), 0L)
  trade_count <- comparison$fields[
    comparison$fields$field == "trade_count",
    ,
    drop = FALSE
  ]
  expect_equal(trade_count$equal_when_both, 1L)
  contracts <- comparison$fields[
    comparison$fields$field == "contracts_traded",
    ,
    drop = FALSE
  ]
  expect_equal(contracts$full_only, 1L)
  expect_equal(contracts$simplified_present, 0L)
})

test_that("wrong-date legacy SPRD cache is rejected instead of relabelled", {
  fixture_dir <- tempfile("b3-wrong-date-fixture-")
  base_dir <- tempfile("b3-wrong-date-cache-")
  cache_dir <- file.path(base_dir, "BDI", "reference")
  legacy_dir <- file.path(base_dir, "BDI", "BVBG", "2026")
  dir.create(fixture_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  dir.create(legacy_dir, recursive = TRUE)
  on.exit(unlink(c(fixture_dir, base_dir), recursive = TRUE), add = TRUE)
  row <- data.frame(
    date = "2026-06-01",
    contract = "BGIF27",
    instrument_id = "100000999999",
    trades = 9,
    contracts = 12,
    volume = 120000,
    open_interest = 300,
    open = 330,
    low = 329,
    high = 332,
    average = 330.5,
    close = 331,
    settlement = 331.2,
    stringsAsFactors = FALSE
  )
  wrong <- row
  wrong$date <- "2026-01-06"
  writeLines(
    price_report_xml(
      "BVBG.187.01",
      "2026-01-06T22:00:00Z",
      wrong
    ),
    file.path(legacy_dir, "2026-06-01-raw.xml"),
    useBytes = TRUE
  )
  correct_xml <- file.path(fixture_dir, "BVBG.187_CORRECT.xml")
  writeLines(
    price_report_xml(
      "BVBG.187.01",
      "2026-06-01T22:00:00Z",
      row
    ),
    correct_xml,
    useBytes = TRUE
  )
  correct_archive <- price_report_archive(
    fixture_dir,
    "SPRD260601",
    correct_xml
  )
  downloads <- 0L
  old_hook <- getOption("brfutures.b3_reference_download_hook")
  on.exit(
    options(brfutures.b3_reference_download_hook = old_hook),
    add = TRUE
  )
  options(brfutures.b3_reference_download_hook = function(url, destination) {
    downloads <<- downloads + 1L
    file.copy(correct_archive, destination, overwrite = TRUE)
  })

  fetched <- brf_b3_prices_fetch(
    as.Date("2026-06-01"),
    report = "simplified",
    root = "BGI",
    cache_dir = cache_dir,
    quiet = TRUE
  )
  expect_equal(downloads, 1L)
  expect_equal(fetched$date, as.Date("2026-06-01"))
  expect_equal(fetched$contract_code, "BGIF27")
  manifest <- attr(fetched, "brf_b3_price_manifest")
  expect_false(identical(manifest$source_origin, "legacy_bvbg_raw_xml"))
  expect_equal(
    brfutures:::`.brf_bvbg_zip_names`(as.Date("2026-06-01")),
    "SPRD260601.zip"
  )
})

test_that("normal XML pipeline uses complete PR from the corrected cutover", {
  fixture_dir <- tempfile("b3-pr-pipeline-fixture-")
  cache_dir <- tempfile("b3-pr-pipeline-cache-")
  dir.create(fixture_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(c(fixture_dir, cache_dir), recursive = TRUE), add = TRUE)
  row <- data.frame(
    date = "2025-12-15",
    contract = "BGIF26",
    instrument_id = "100000237982",
    trades = 7,
    contracts = 30,
    volume = 300000,
    open_interest = 1000,
    open = 320,
    low = 319,
    high = 322,
    average = 320.5,
    close = 321,
    settlement = 321.2,
    stringsAsFactors = FALSE
  )
  xml <- file.path(fixture_dir, "BVBG.086_PIPELINE.xml")
  writeLines(
    price_report_xml(
      "BVBG.086.01",
      "2025-12-15T23:00:00Z",
      row
    ),
    xml,
    useBytes = TRUE
  )
  archive <- price_report_archive(fixture_dir, "PR251215", xml)
  downloads <- 0L
  old_cache <- getOption("brfutures.cache_dir")
  old_hook <- getOption("brfutures.b3_reference_download_hook")
  old_cutover <- getOption("brfutures.xml_cutover_date")
  on.exit(options(
    brfutures.cache_dir = old_cache,
    brfutures.b3_reference_download_hook = old_hook,
    brfutures.xml_cutover_date = old_cutover
  ), add = TRUE)
  options(
    brfutures.cache_dir = cache_dir,
    brfutures.xml_cutover_date = NULL,
    brfutures.b3_reference_download_hook = function(url, destination) {
      downloads <<- downloads + 1L
      expect_match(url, "filelist=PR251215\\.zip")
      file.copy(archive, destination, overwrite = TRUE)
    }
  )

  expect_equal(
    brfutures:::`.brf_xml_cutover_date`(),
    as.Date("2025-12-15")
  )
  parsed <- brfutures:::`.brf_bvbg_ensure_parsed_day`(
    as.Date("2025-12-15"),
    quiet = TRUE
  )
  expect_equal(downloads, 1L)
  expect_equal(parsed$source_report_type, "BVBG.086.01")
  expect_equal(parsed$contracts_traded, 30)
  expect_equal(parsed$volume, 300000)
  expect_equal(parsed$source, "xml")
  expect_equal(parsed$settlement_available_at, parsed$available_at)
  expect_false(file.exists(brfutures:::`.brf_bvbg_raw_path`(
    as.Date("2025-12-15"),
    create = FALSE
  )))

  options(brfutures.b3_reference_download_hook = function(...) {
    stop("completed PR pipeline cache attempted a download")
  })
  cached <- brfutures:::`.brf_bvbg_ensure_parsed_day`(
    as.Date("2025-12-15"),
    quiet = TRUE
  )
  expect_equal(cached$contracts_traded, 30)

  unlink(brfutures:::`.brf_bvbg_parsed_path`(
    as.Date("2025-12-15"),
    create = FALSE
  ))
  unlink(brfutures:::`.brf_bvbg_year_path`("2025", create = FALSE))
  rebuilt_year <- brfutures:::`.brf_bvbg_year_data`("2025", quiet = TRUE)
  expect_equal(downloads, 1L)
  expect_equal(rebuilt_year$date, as.Date("2025-12-15"))
  expect_equal(rebuilt_year$contracts_traded, 30)
})
