bit_lifecycle_xml <- function(expiry_date,
                              app_created_at,
                              group_id,
                              instrument_id = "400000110035",
                              contract_multiplier = 0.01,
                              report_date = "2025-12-23") {
  c(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
    "<Document xmlns=\"urn:bvmf.052.01.xsd\">",
    "  <BizFileHdr><Xchg><BizGrpDesc><BizGrpDtls>",
    paste0("    <BizGrpIdr>", group_id, "</BizGrpIdr>"),
    "    <BizGrpTp>BVBG.028.02</BizGrpTp>",
    "    <CreDtAndTm>2025-12-23T18:48:25</CreDtAndTm>",
    "  </BizGrpDtls></BizGrpDesc></Xchg></BizFileHdr>",
    "  <BizGrp>",
    "    <AppHdr xmlns=\"urn:iso:std:iso:20022:tech:xsd:head.001.001.01\">",
    paste0("      <BizMsgIdr>MSG-", group_id, "</BizMsgIdr>"),
    paste0("      <CreDt>", app_created_at, "</CreDt>"),
    "    </AppHdr>",
    "    <Document xmlns=\"urn:bvmf.100.02.xsd\">",
    "      <Instrm>",
    "        <RptParams>",
    "          <ActvtyInd>true</ActvtyInd>",
    paste0(
      "          <RptDtAndTm><Dt>",
      report_date,
      "</Dt></RptDtAndTm>"
    ),
    "          <UpdTp>COMP</UpdTp>",
    "        </RptParams>",
    "        <FinInstrmId><OthrId>",
    paste0("          <Id>", instrument_id, "</Id>"),
    "        </OthrId></FinInstrmId>",
    "        <FinInstrmAttrCmon><Asst>BIT</Asst></FinInstrmAttrCmon>",
    "        <InstrmInf><FutrCtrctsInf>",
    paste0("          <XprtnDt>", expiry_date, "</XprtnDt>"),
    "          <TckrSymb>BITZ25</TckrSymb>",
    "          <TradgStartDt>2025-10-30</TradgStartDt>",
    paste0("          <TradgEndDt>", expiry_date, "</TradgEndDt>"),
    paste0(
      "          <CtrctMltplr>",
      sprintf("%.9f", contract_multiplier),
      "</CtrctMltplr>"
    ),
    "          <TradgCcy>BRL</TradgCcy>",
    "          <ISIN>BRBMEFBIT0P5</ISIN>",
    "        </FutrCtrctsInf></InstrmInf>",
    "      </Instrm>",
    "    </Document>",
    "  </BizGrp>",
    "</Document>"
  )
}

bit_indicator_line <- function(sequence,
                               reference_date,
                               indicator,
                               mantissa,
                               decimal_places) {
  paste0(
    sprintf("%06d", sequence),
    "00101",
    format(as.Date(reference_date), "%Y%m%d"),
    sprintf("%-27s", indicator),
    "+",
    sprintf("%024.0f", mantissa),
    sprintf("%02d", decimal_places),
    paste(rep(" ", 36L), collapse = "")
  )
}

bit_settlement_xml <- function(contract = "BITJ24",
                               date = "2024-04-26",
                               app_created_at = "2024-04-26T22:40:23Z",
                               message_id = "MSG-187-J24",
                               settlement = 327350.71,
                               previous_settlement = 335123.61) {
  c(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
    "<Document xmlns=\"urn:bvmf.052.01.xsd\">",
    "  <BizFileHdr><Xchg><BizGrpDesc><BizGrpDtls>",
    "    <BizGrpIdr>GROUP-187-J24</BizGrpIdr>",
    "    <BizGrpTp>BVBG.187.01</BizGrpTp>",
    "    <CreDtAndTm>2024-04-26T19:40:23</CreDtAndTm>",
    "  </BizGrpDtls></BizGrpDesc></Xchg></BizFileHdr>",
    "  <BizGrp>",
    "    <AppHdr xmlns=\"urn:iso:std:iso:20022:tech:xsd:head.001.001.01\">",
    paste0("      <BizMsgIdr>", message_id, "</BizMsgIdr>"),
    paste0("      <CreDt>", app_created_at, "</CreDt>"),
    "    </AppHdr>",
    "    <Document xmlns=\"urn:bvmf.217.01.xsd\">",
    "      <PricRpt>",
    paste0("        <TradDt><Dt>", date, "</Dt></TradDt>"),
    paste0("        <SctyId><TckrSymb>", contract, "</TckrSymb></SctyId>"),
    "        <FinInstrmId><OthrId><Id>100000216950</Id></OthrId></FinInstrmId>",
    "        <FinInstrmAttrbts>",
    paste0("          <AdjstdQt Ccy=\"BRL\">", settlement, "</AdjstdQt>"),
    "          <AdjstdQtStin>F</AdjstdQtStin>",
    paste0(
      "          <PrvsAdjstdQt Ccy=\"BRL\">",
      previous_settlement,
      "</PrvsAdjstdQt>"
    ),
    "          <PrvsAdjstdQtStin>F</PrvsAdjstdQtStin>",
    "        </FinInstrmAttrbts>",
    "      </PricRpt>",
    "    </Document>",
    "  </BizGrp>",
    "</Document>"
  )
}

bit_calendar_reference <- function() {
  paste0(
    "https://www.b3.com.br/data/files/AC/20/51/03/",
    "F384591029BEEC39AC094EA8/",
    "OC%20026-2025%20PRE%20Retificacao%20-%20Calendario%20de%20",
    "Feriados%202025%20e%20Funcionamento%20da%20B3.pdf"
  )
}

bit_calendar_evidence <- function(dates,
                                  business_days,
                                  available_at,
                                  calendar_kind = "complete_daily_status",
                                  coverage_start = NULL,
                                  coverage_end = NULL) {
  path <- tempfile(fileext = ".csv")
  source_document_path <- tempfile(fileext = ".pdf")
  writeLines(
    c(
      "OFFICIAL B3 CALENDAR SOURCE FIXTURE",
      bit_calendar_reference()
    ),
    source_document_path,
    useBytes = TRUE
  )
  if (identical(calendar_kind, "complete_daily_status")) {
    utils::write.csv(
      data.frame(
        date = as.Date(dates),
        is_business_day = business_days
      ),
      path,
      row.names = FALSE
    )
  } else {
    utils::write.csv(
      data.frame(date = as.Date(dates)),
      path,
      row.names = FALSE
    )
  }
  evidence <- brf_b3_calendar_evidence_read(
    path = path,
    source_document_path = source_document_path,
    available_at = available_at,
    source_reference = bit_calendar_reference(),
    calendar_id = "B3_LISTED_AND_CLEARING_2025",
    normalization_method = "manual_transcription_reviewed",
    normalization_version = "bit_calendar_fixture_v1",
    reviewer = "brfutures-test-reviewer",
    reviewed_at = available_at,
    review_attestation = "reviewed_against_hashed_b3_source",
    calendar_kind = calendar_kind,
    coverage_start = coverage_start,
    coverage_end = coverage_end,
    normalized_file = "OC-026-2025-PRE-calendar.normalized.csv",
    source_document_file = "OC-026-2025-PRE-calendar.pdf"
  )
  unlink(c(path, source_document_path))
  evidence
}

test_that("B3 calendar evidence resolves weekend and holiday without guessing", {
  weekend <- bit_calendar_evidence(
    dates = seq(as.Date("2025-12-19"), as.Date("2025-12-22"), by = "day"),
    business_days = c(TRUE, FALSE, FALSE, TRUE),
    available_at = "2025-02-27T15:00:00Z"
  )
  weekend_result <- brfutures:::`.brf_b3_calendar_resolve_posting`(
    weekend,
    as.Date("2025-12-19")
  )
  expect_equal(weekend_result$posting_date, as.Date("2025-12-22"))
  expect_equal(
    weekend_result$calendar_schema_id,
    "brfutures_b3_calendar_evidence_v1"
  )
  expect_equal(
    weekend_result$calendar_scope,
    "b3_listed_derivatives_and_clearing"
  )
  expect_match(
    weekend_result$source_document_sha256,
    "^[0-9a-f]{64}$"
  )
  expect_match(weekend_result$normalized_sha256, "^[0-9a-f]{64}$")
  expect_false(identical(
    weekend_result$source_document_sha256,
    weekend_result$normalized_sha256
  ))
  expect_match(weekend_result$calendar_fingerprint, "^[0-9a-f]{64}$")

  holiday <- bit_calendar_evidence(
    dates = seq(as.Date("2025-11-19"), as.Date("2025-11-21"), by = "day"),
    business_days = c(TRUE, FALSE, TRUE),
    available_at = "2025-02-27T15:00:00Z"
  )
  holiday_result <- brfutures:::`.brf_b3_calendar_resolve_posting`(
    holiday,
    as.Date("2025-11-19")
  )
  expect_equal(holiday_result$posting_date, as.Date("2025-11-21"))
})

test_that("session-date evidence uses only listed source sessions", {
  sessions <- bit_calendar_evidence(
    dates = as.Date(c("2025-12-19", "2025-12-23")),
    business_days = NULL,
    available_at = "2025-02-27T15:00:00Z",
    calendar_kind = "session_dates",
    coverage_start = as.Date("2025-12-19"),
    coverage_end = as.Date("2025-12-23")
  )
  expect_equal(
    sessions$date[sessions$is_business_day],
    as.Date(c("2025-12-19", "2025-12-23"))
  )
  result <- brfutures:::`.brf_b3_calendar_resolve_posting`(
    sessions,
    as.Date("2025-12-19")
  )
  expect_equal(result$posting_date, as.Date("2025-12-23"))
})

test_that("B3 calendar evidence is causal and tamper-evident", {
  late <- bit_calendar_evidence(
    dates = seq(as.Date("2025-12-19"), as.Date("2025-12-22"), by = "day"),
    business_days = c(TRUE, FALSE, FALSE, TRUE),
    available_at = "2025-12-20T04:00:00Z"
  )
  expect_error(
    brfutures:::`.brf_b3_calendar_resolve_posting`(
      late,
      as.Date("2025-12-19")
    ),
    "not causally available"
  )

  tampered <- late
  tampered$is_business_day[tampered$date == as.Date("2025-12-21")] <- TRUE
  expect_error(
    brfutures:::`.brf_b3_calendar_validate`(tampered),
    "fingerprint mismatch"
  )

  missing_source_hash <- late
  missing_source_hash$source_document_sha256 <- NULL
  expect_error(
    brfutures:::`.brf_b3_calendar_validate`(missing_source_hash),
    "missing required fields.*source_document_sha256"
  )

  tampered_source_hash <- late
  tampered_source_hash$source_document_sha256 <- paste0(
    if (substr(
      tampered_source_hash$source_document_sha256[[1L]],
      1L,
      1L
    ) == "a") "b" else "a",
    substr(
      tampered_source_hash$source_document_sha256[[1L]],
      2L,
      64L
    )
  )
  expect_error(
    brfutures:::`.brf_b3_calendar_validate`(tampered_source_hash),
    "fingerprint mismatch"
  )

  missing_attestation <- late
  missing_attestation$review_attestation <- NULL
  expect_error(
    brfutures:::`.brf_b3_calendar_validate`(missing_attestation),
    "missing required fields.*review_attestation"
  )

  tampered_attestation <- late
  tampered_attestation$review_attestation <- "not_reviewed"
  expect_error(
    brfutures:::`.brf_b3_calendar_validate`(tampered_attestation),
    "review attestation"
  )
})

test_that("BVBG.028 lifecycle selects the latest official snapshot", {
  early <- tempfile(fileext = ".xml")
  late <- tempfile(fileext = ".xml")
  on.exit(unlink(c(early, late)), add = TRUE)
  writeLines(
    bit_lifecycle_xml(
      "2025-12-26",
      "2025-12-23T04:47:54Z",
      "EARLY"
    ),
    early,
    useBytes = TRUE
  )
  writeLines(
    bit_lifecycle_xml(
      "2025-12-23",
      "2025-12-23T21:47:59Z",
      "LATE"
    ),
    late,
    useBytes = TRUE
  )

  all_versions <- brf_b3_contract_lifecycle_read(
    c(late, early),
    latest = FALSE
  )
  expect_equal(nrow(all_versions), 2L)
  expect_equal(
    all_versions$available_at,
    as.POSIXct(
      c("2025-12-23 04:47:54", "2025-12-23 21:47:59"),
      tz = "UTC"
    )
  )

  latest <- brf_b3_contract_lifecycle_read(c(early, late))
  expect_equal(nrow(latest), 1L)
  expect_equal(latest$contract, "BITZ25")
  expect_equal(latest$report_date, as.Date("2025-12-23"))
  expect_equal(latest$expiry_date, as.Date("2025-12-23"))
  expect_equal(latest$last_trade_date, as.Date("2025-12-23"))
  expect_equal(latest$contract_multiplier, 0.01)
  expect_equal(latest$contract_size_btc, 0.01)
  expect_equal(latest$contract_size_regime, "current_0.01_btc")
  expect_equal(
    latest$contract_size_effective_from,
    as.Date("2025-06-16")
  )
  expect_true(is.na(latest$contract_size_effective_to))
  expect_equal(
    latest$position_conversion_asof_date,
    as.Date("2025-06-13")
  )
  expect_equal(
    latest$position_conversion_effective_date,
    as.Date("2025-06-16")
  )
  expect_equal(latest$position_conversion_ratio, 10)
  expect_equal(
    latest$administrative_position_transform,
    "open_quantity_multiply_10"
  )
  expect_equal(
    latest$specification_source,
    "B3 Circular Letter 013/2025-VPC"
  )
  expect_equal(latest$instrument_id, "400000110035")
  expect_equal(latest$source_group_id, "LATE")
  expect_equal(latest$source_schema_id, "brfutures_b3_bit_sources_v1")
  expect_equal(latest$source_schema_version, 1L)
  expect_equal(latest$source_parser, "bounded_bizgrp_stream_v1")
  expect_match(latest$source_sha256, "^[0-9a-f]{64}$")
})

test_that("lifecycle exposes the administrative BIT size conversion", {
  legacy <- tempfile(fileext = ".xml")
  current <- tempfile(fileext = ".xml")
  on.exit(unlink(c(legacy, current)), add = TRUE)
  writeLines(
    bit_lifecycle_xml(
      "2025-12-26",
      "2025-06-13T21:47:59Z",
      "LEGACY-SIZE",
      contract_multiplier = 0.1,
      report_date = "2025-06-13"
    ),
    legacy,
    useBytes = TRUE
  )
  writeLines(
    bit_lifecycle_xml(
      "2025-12-26",
      "2025-06-16T21:47:59Z",
      "CURRENT-SIZE",
      contract_multiplier = 0.01,
      report_date = "2025-06-16"
    ),
    current,
    useBytes = TRUE
  )

  lifecycle <- brf_b3_contract_lifecycle_read(
    c(current, legacy),
    latest = FALSE
  )
  expect_equal(
    lifecycle$contract_size_regime,
    c("legacy_0.1_btc", "current_0.01_btc")
  )
  expect_equal(lifecycle$contract_size_btc, c(0.1, 0.01))
  expect_equal(
    lifecycle$contract_size_effective_to,
    as.Date(c("2025-06-13", NA))
  )
  expect_equal(
    lifecycle$contract_size_effective_from,
    as.Date(c(NA, "2025-06-16"))
  )
  expect_equal(lifecycle$position_conversion_ratio, c(10, 10))
  expect_true(all(
    lifecycle$administrative_position_transform ==
      "open_quantity_multiply_10"
  ))
})

test_that("lifecycle reader streams past many irrelevant instruments", {
  path <- tempfile(fileext = ".xml")
  connection <- file(path, open = "wt", encoding = "UTF-8")
  on.exit({
    try(close(connection), silent = TRUE)
    unlink(path)
  }, add = TRUE)
  writeLines(c(
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
    "<Document xmlns=\"urn:bvmf.052.01.xsd\">",
    "  <BizFileHdr><Xchg><BizGrpDesc><BizGrpDtls>",
    "    <BizGrpIdr>STREAM-GROUP</BizGrpIdr>",
    "    <BizGrpTp>BVBG.028.02</BizGrpTp>",
    "    <CreDtAndTm>2025-12-23T18:48:25</CreDtAndTm>",
    "  </BizGrpDtls></BizGrpDesc></Xchg></BizFileHdr>"
  ), connection)
  noise <- c(
    "  <BizGrp>",
    "    <AppHdr xmlns=\"urn:iso:std:iso:20022:tech:xsd:head.001.001.01\">",
    "      <BizMsgIdr>NOISE</BizMsgIdr><CreDt>2025-12-23T21:00:00Z</CreDt>",
    "    </AppHdr>",
    "    <Document xmlns=\"urn:bvmf.100.02.xsd\"><Instrm>",
    "      <RptParams><ActvtyInd>true</ActvtyInd>",
    "        <RptDtAndTm><Dt>2025-12-23</Dt></RptDtAndTm><UpdTp>COMP</UpdTp>",
    "      </RptParams>",
    "      <FinInstrmId><OthrId><Id>NOISE</Id></OthrId></FinInstrmId>",
    "      <FinInstrmAttrCmon><Asst>WIN</Asst></FinInstrmAttrCmon>",
    "      <InstrmInf><FutrCtrctsInf>",
    "        <XprtnDt>2026-01-30</XprtnDt><TckrSymb>WING26</TckrSymb>",
    "        <TradgStartDt>2025-01-01</TradgStartDt>",
    "        <TradgEndDt>2026-01-30</TradgEndDt>",
    "        <CtrctMltplr>0.2</CtrctMltplr><TradgCcy>BRL</TradgCcy>",
    "      </FutrCtrctsInf></InstrmInf>",
    "    </Instrm></Document>",
    "  </BizGrp>"
  )
  for (i in seq_len(5000L)) {
    writeLines(noise, connection)
  }
  bit_document <- bit_lifecycle_xml(
    "2025-12-23",
    "2025-12-23T21:47:59Z",
    "BIT-AFTER-NOISE"
  )
  group_start <- match("  <BizGrp>", bit_document)
  group_end <- max(which(bit_document == "  </BizGrp>"))
  writeLines(bit_document[group_start:group_end], connection)
  writeLines("</Document>", connection)
  close(connection)

  parsed <- brf_b3_contract_lifecycle_read(path)
  expect_equal(nrow(parsed), 1L)
  expect_equal(parsed$contract, "BITZ25")
  expect_equal(parsed$source_group_id, "STREAM-GROUP")
  expect_equal(parsed$source_message_id, "MSG-BIT-AFTER-NOISE")
  expect_equal(parsed$source_parser, "bounded_bizgrp_stream_v1")
})

test_that("Indic.txt parser preserves scale, aliases and causal provenance", {
  path <- tempfile("Indic", fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    bit_indicator_line(1, "2025-12-23", "BTCLIQUSD", 8781713, 2),
    bit_indicator_line(2, "2025-12-23", "RTDOL-D1", 55379, 4),
    bit_indicator_line(3, "2025-12-23", "RTBITLIQ", 48632248, 2),
    bit_indicator_line(4, "2025-12-23", "UNRELATED", 123, 2)
  ), path, useBytes = TRUE)
  available_at <- as.POSIXct("2025-12-23 22:45:00", tz = "UTC")

  parsed <- brf_b3_indicators_read(
    path,
    report_date = as.Date("2025-12-23"),
    available_at = available_at,
    source_file = "ID251223.ex_"
  )
  expect_equal(
    parsed$canonical_indicator,
    c("NQBTCS", "RTBITLIQ", "RTDOL-D1")
  )
  expect_equal(
    parsed$value,
    c(87817.13, 486322.48, 5.5379)
  )
  expect_equal(parsed$decimal_places, c(2L, 2L, 4L))
  expect_true(all(parsed$available_at == available_at))
  expect_true(all(parsed$source_file == "ID251223.ex_"))
  expect_true(all(
    parsed$source_schema_id == "brfutures_b3_bit_sources_v1"
  ))
  expect_true(all(parsed$source_schema_version == 1L))
  expect_true(all(grepl("^[0-9a-f]{64}$", parsed$source_sha256)))

  expect_error(
    brf_b3_indicators_read(
      path,
      report_date = as.Date("2025-12-23"),
      available_at = NULL
    ),
    "causal timestamp"
  )
})

test_that("BIT terminal source assembly reconciles all three official prices", {
  lifecycle_path <- tempfile(fileext = ".xml")
  indicator_path <- tempfile(fileext = ".txt")
  on.exit(unlink(c(lifecycle_path, indicator_path)), add = TRUE)
  writeLines(
    bit_lifecycle_xml(
      "2025-12-23",
      "2025-12-23T21:47:59Z",
      "LATE"
    ),
    lifecycle_path,
    useBytes = TRUE
  )
  lifecycle <- brf_b3_contract_lifecycle_read(lifecycle_path)
  writeLines(c(
    bit_indicator_line(1, "2025-12-23", "BTCLIQUSD", 8781713, 2),
    bit_indicator_line(2, "2025-12-23", "RTDOL-D1", 55379, 4),
    bit_indicator_line(3, "2025-12-23", "RTBITLIQ", 48632248, 2)
  ), indicator_path, useBytes = TRUE)
  indicators <- brf_b3_indicators_read(
    indicator_path,
    report_date = as.Date("2025-12-23"),
    available_at = "2025-12-23T22:45:00Z",
    source_file = "ID251223.ex_"
  )
  settlements <- data.frame(
    contract_code = "BITZ25",
    date = as.Date("2025-12-23"),
    available_at = as.POSIXct(
      "2025-12-23 22:39:49",
      tz = "UTC"
    ),
    settlement_price = 486322.48,
    settlement_status = "F",
    previous_settlement = 494212.64,
    previous_settlement_status = "F",
    source_file = "BVBG.187.01.xml",
    source_sha256 = paste(rep("a", 64L), collapse = ""),
    stringsAsFactors = FALSE
  )

  terminal <- brf_b3_bit_terminal_assemble(
    settlements,
    indicators,
    lifecycle
  )
  expect_equal(terminal$contract, "BITZ25")
  expect_equal(
    terminal$source_schema_id,
    "brfutures_b3_bit_sources_v1"
  )
  expect_equal(terminal$source_schema_version, 1L)
  expect_equal(terminal$session_date, as.Date("2025-12-23"))
  expect_equal(terminal$nqbtcs_usd, 87817.13)
  expect_equal(terminal$rtdol_d1, 5.5379)
  expect_equal(terminal$rtbitliq_brl, 486322.48)
  expect_equal(terminal$calculated_settlement_brl, 486322.48)
  expect_true(terminal$formula_reconciled)
  expect_true(terminal$direct_brl_reconciled)
  expect_equal(
    terminal$pnl_formula_id,
    "b3_bit_final_settlement_nqbtcs_fx_v2"
  )
  expect_equal(terminal$cash_available_business_day_lag, 1L)
  expect_true(is.na(terminal$cash_posting_date))
  expect_equal(
    terminal$cash_posting_date_status,
    "requires_official_b3_calendar"
  )
  expect_false(terminal$execution_supported)
  expect_equal(terminal$usage, "source_validation_only")
  expect_match(terminal$terminal_fingerprint, "^[0-9a-f]{64}$")

  calendar <- bit_calendar_evidence(
    dates = seq(as.Date("2025-12-23"), as.Date("2025-12-26"), by = "day"),
    business_days = c(TRUE, FALSE, FALSE, TRUE),
    available_at = "2025-02-27T15:00:00Z"
  )
  with_calendar <- brf_b3_bit_terminal_assemble(
    settlements,
    indicators,
    lifecycle,
    calendar_evidence = calendar
  )
  expect_equal(
    with_calendar$cash_posting_date,
    as.Date("2025-12-26")
  )
  expect_equal(
    with_calendar$cash_posting_date_status,
    "official_b3_business_day_calendar"
  )
  expect_equal(
    with_calendar$cash_posting_calendar_schema_id,
    "brfutures_b3_calendar_evidence_v1"
  )
  expect_equal(
    with_calendar$cash_posting_calendar_scope,
    "b3_listed_derivatives_and_clearing"
  )
  expect_equal(
    with_calendar$cash_posting_calendar_available_at,
    as.POSIXct("2025-02-27 15:00:00", tz = "UTC")
  )
  expect_match(
    with_calendar$cash_posting_calendar_source_document_sha256,
    "^[0-9a-f]{64}$"
  )
  expect_match(
    with_calendar$cash_posting_calendar_normalized_sha256,
    "^[0-9a-f]{64}$"
  )
  expect_equal(
    with_calendar$cash_posting_calendar_normalization_method,
    "manual_transcription_reviewed"
  )
  expect_equal(
    with_calendar$cash_posting_calendar_normalization_version,
    "bit_calendar_fixture_v1"
  )
  expect_equal(
    with_calendar$cash_posting_calendar_review_attestation,
    "reviewed_against_hashed_b3_source"
  )
  expect_match(
    with_calendar$cash_posting_calendar_fingerprint,
    "^[0-9a-f]{64}$"
  )
  expect_false(identical(
    with_calendar$terminal_fingerprint,
    terminal$terminal_fingerprint
  ))

  caller_date <- brf_b3_bit_terminal_assemble(
    settlements,
    indicators,
    lifecycle,
    posting_date = as.Date("2025-12-24")
  )
  expect_equal(
    caller_date$cash_posting_date_status,
    "caller_supplied_not_calendar_validated"
  )
  expect_true(is.na(caller_date$cash_posting_calendar_schema_id))
  expect_error(
    brf_b3_bit_terminal_assemble(
      settlements,
      indicators,
      lifecycle,
      posting_date = as.Date("2025-12-24"),
      calendar_evidence = calendar
    ),
    "either posting_date or calendar_evidence"
  )

  non_final <- settlements
  non_final$settlement_status <- "P"
  expect_error(
    brf_b3_bit_terminal_assemble(non_final, indicators, lifecycle),
    "statuses must both be final"
  )

  bad_direct <- indicators
  bad_direct$value[bad_direct$canonical_indicator == "RTBITLIQ"] <- 486322.47
  expect_error(
    brf_b3_bit_terminal_assemble(settlements, bad_direct, lifecycle),
    "must equal RTBITLIQ"
  )
})

test_that("daily reference URL names are stable and explicit", {
  expect_equal(
    brfutures:::`.brf_b3_daily_file_url`(
      as.Date("2025-12-23"),
      "instrument"
    ),
    paste0(
      "https://www.b3.com.br/pesquisapregao/download?filelist=",
      "IN251223.zip"
    )
  )
  expect_equal(
    brfutures:::`.brf_b3_daily_file_url`(
      as.Date("2025-12-23"),
      "indicator"
    ),
    paste0(
      "https://www.b3.com.br/pesquisapregao/download?filelist=",
      "ID251223.ex_"
    )
  )
  expect_equal(
    brfutures:::`.brf_b3_daily_file_url`(
      as.Date("2024-04-26"),
      "settlement"
    ),
    paste0(
      "https://www.b3.com.br/pesquisapregao/download?filelist=",
      "SPRD240426.zip"
    )
  )
})

test_that("historical settlement fetch bypasses the global XML cutover", {
  fixture_dir <- tempfile("b3-sprd-fixture-")
  cache_dir <- tempfile("b3-sprd-cache-")
  dir.create(fixture_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(c(fixture_dir, cache_dir), recursive = TRUE), add = TRUE)
  payload <- file.path(fixture_dir, "BVBG.187.01_BITJ24.xml")
  inner <- file.path(fixture_dir, "SPRD240426.zip")
  outer <- file.path(fixture_dir, "outer-SPRD240426.zip")
  writeLines(bit_settlement_xml(), payload, useBytes = TRUE)
  suppressWarnings(utils::zip(inner, files = payload, flags = "-j"))
  suppressWarnings(utils::zip(outer, files = inner, flags = "-j"))
  old_hook <- getOption("brfutures.b3_reference_download_hook")
  old_cutover <- getOption("brfutures.xml_cutover_date")
  on.exit({
    options(
      brfutures.b3_reference_download_hook = old_hook,
      brfutures.xml_cutover_date = old_cutover
    )
  }, add = TRUE)
  options(
    brfutures.xml_cutover_date = "2099-01-01",
    brfutures.b3_reference_download_hook = function(url, destination) {
      file.copy(outer, destination, overwrite = TRUE)
    }
  )

  settlement <- brf_b3_settlements_fetch(
    as.Date("2024-04-26"),
    root = "BIT",
    cache_dir = cache_dir,
    refresh = TRUE,
    quiet = TRUE
  )
  expect_equal(nrow(settlement), 1L)
  expect_equal(settlement$contract_code, "BITJ24")
  expect_equal(settlement$settlement_price, 327350.71)
  expect_equal(settlement$settlement_status, "F")
  expect_equal(settlement$previous_settlement, 335123.61)
  expect_equal(settlement$previous_settlement_status, "F")
  expect_equal(
    settlement$available_at,
    as.POSIXct("2024-04-26 22:40:23", tz = "UTC")
  )
  expect_equal(settlement$source_report_type, "BVBG.187.01")
  expect_equal(
    settlement$source_schema_id,
    "brfutures_b3_bit_sources_v1"
  )
  expect_equal(settlement$source_schema_version, 1L)
  expect_match(settlement$source_sha256, "^[0-9a-f]{64}$")
  expect_equal(
    getOption("brfutures.xml_cutover_date"),
    "2099-01-01"
  )
})

test_that("BVBG.187 provenance stays aligned after many preceding groups", {
  path <- tempfile(fileext = ".xml")
  connection <- file(path, open = "wt", encoding = "UTF-8")
  on.exit({
    try(close(connection), silent = TRUE)
    unlink(path)
  }, add = TRUE)
  template <- bit_settlement_xml(
    contract = "WINF26",
    message_id = "NOISE-187"
  )
  group_start <- match("  <BizGrp>", template)
  group_end <- max(which(template == "  </BizGrp>"))
  writeLines(template[seq_len(group_start - 1L)], connection)
  for (i in seq_len(1000L)) {
    writeLines(template[group_start:group_end], connection)
  }
  target <- bit_settlement_xml(
    contract = "BITJ24",
    app_created_at = "2024-04-26T22:40:23Z",
    message_id = "BIT-FINAL-187"
  )
  writeLines(target[group_start:group_end], connection)
  writeLines("</Document>", connection)
  close(connection)

  parsed <- brfutures:::`.brf_parse_bvbg_xml_for_root`(path, "BIT")
  expect_equal(nrow(parsed), 1L)
  expect_equal(parsed$contract_code, "BITJ24")
  expect_equal(parsed$source_message_id, "BIT-FINAL-187")
  expect_equal(
    parsed$available_at,
    as.POSIXct("2024-04-26 22:40:23", tz = "UTC")
  )
  expect_equal(parsed$settlement_status, "F")
  expect_equal(parsed$previous_settlement_status, "F")
})

test_that("lifecycle fetch extracts only the newest nested BVBG.028 XML", {
  fixture_dir <- tempfile("b3-in-fixture-")
  cache_dir <- tempfile("b3-in-cache-")
  dir.create(fixture_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(c(fixture_dir, cache_dir), recursive = TRUE), add = TRUE)
  early <- file.path(fixture_dir, "BVBG.028.02_EARLY.xml")
  late <- file.path(fixture_dir, "BVBG.028.02_LATE.xml")
  inner <- file.path(fixture_dir, "IN251223.zip")
  outer <- file.path(fixture_dir, "outer-IN251223.zip")
  writeLines(
    bit_lifecycle_xml(
      "2025-12-26",
      "2025-12-23T04:47:54Z",
      "EARLY"
    ),
    early,
    useBytes = TRUE
  )
  writeLines(
    bit_lifecycle_xml(
      "2025-12-23",
      "2025-12-23T21:47:59Z",
      "LATE"
    ),
    late,
    useBytes = TRUE
  )
  suppressWarnings(utils::zip(
    inner,
    files = c(early, late),
    flags = "-j"
  ))
  suppressWarnings(utils::zip(
    outer,
    files = inner,
    flags = "-j"
  ))
  old_hook <- getOption("brfutures.b3_reference_download_hook")
  on.exit(
    options(brfutures.b3_reference_download_hook = old_hook),
    add = TRUE
  )
  options(brfutures.b3_reference_download_hook = function(url, destination) {
    file.copy(outer, destination, overwrite = TRUE)
  })

  revisions <- brf_b3_contract_lifecycle_fetch(
    as.Date("2025-12-23"),
    cache_dir = cache_dir,
    refresh = TRUE,
    quiet = TRUE,
    latest = FALSE
  )
  expect_equal(nrow(revisions), 2L)
  expect_equal(
    revisions$expiry_date,
    as.Date(c("2025-12-26", "2025-12-23"))
  )
  expect_equal(revisions$source_group_id, c("EARLY", "LATE"))
  expect_true(all(grepl(
    "^[0-9a-f]{64}$",
    revisions$source_archive_sha256
  )))
  expect_true(all(revisions$source_archive_file == "IN251223.zip"))

  options(brfutures.b3_reference_download_hook = function(url, destination) {
    stop("cache read attempted a download")
  })
  lifecycle <- brf_b3_contract_lifecycle_fetch(
    as.Date("2025-12-23"),
    cache_dir = cache_dir,
    refresh = FALSE,
    quiet = TRUE
  )
  expect_equal(lifecycle$expiry_date, as.Date("2025-12-23"))
  expect_equal(lifecycle$source_group_id, "LATE")
  cached_xml <- list.files(
    cache_dir,
    pattern = "\\.xml$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(cached_xml, 0L)
  cached_archive <- list.files(
    cache_dir,
    pattern = "\\.zip$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(cached_archive, 1L)
  expect_match(cached_archive, "/archives/sha256/[0-9a-f]{64}/")
})

test_that("indicator fetch retains immutable causal observations", {
  fixture_dir <- tempfile("b3-id-fixture-")
  cache_dir <- tempfile("b3-id-cache-")
  dir.create(fixture_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(c(fixture_dir, cache_dir), recursive = TRUE), add = TRUE)
  payload <- file.path(fixture_dir, "Indic.txt")
  inner <- file.path(fixture_dir, "ID251223.ex_")
  outer <- file.path(fixture_dir, "outer-ID251223.ex_")
  writeLines(c(
    bit_indicator_line(1, "2025-12-23", "BTCLIQUSD", 8781713, 2),
    bit_indicator_line(2, "2025-12-23", "RTDOL-D1", 55379, 4),
    bit_indicator_line(3, "2025-12-23", "RTBITLIQ", 48632248, 2)
  ), payload, useBytes = TRUE)
  suppressWarnings(utils::zip(inner, files = payload, flags = "-j"))
  suppressWarnings(utils::zip(outer, files = inner, flags = "-j"))
  old_hook <- getOption("brfutures.b3_reference_download_hook")
  on.exit(
    options(brfutures.b3_reference_download_hook = old_hook),
    add = TRUE
  )
  options(brfutures.b3_reference_download_hook = function(url, destination) {
    file.copy(outer, destination, overwrite = TRUE)
  })
  available_at_1 <- as.POSIXct("2025-12-23 22:45:00", tz = "UTC")
  available_at_2 <- as.POSIXct("2025-12-23 22:50:00", tz = "UTC")

  first <- brf_b3_indicators_fetch(
    as.Date("2025-12-23"),
    available_at = available_at_1,
    cache_dir = cache_dir,
    refresh = TRUE,
    quiet = TRUE
  )
  expect_equal(nrow(first), 3L)
  expect_true(all(first$available_at == available_at_1))
  expect_true(all(first$source_archive_file == "ID251223.ex_"))
  expect_true(all(grepl(
    "^[0-9a-f]{64}$",
    first$source_archive_sha256
  )))
  second <- brf_b3_indicators_fetch(
    as.Date("2025-12-23"),
    available_at = available_at_2,
    cache_dir = cache_dir,
    refresh = TRUE,
    quiet = TRUE
  )
  expect_equal(nrow(second), 3L)
  expect_true(all(second$available_at == available_at_2))
  expect_true(all(grepl(
    "^[0-9a-f]{64}$",
    second$source_observation_fingerprint
  )))
  expect_false(identical(
    unique(first$source_observation_fingerprint),
    unique(second$source_observation_fingerprint)
  ))
  manifest_paths <- list.files(
    cache_dir,
    pattern = "^manifest-[0-9a-f]{64}\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(manifest_paths, 2L)
  manifest_times <- unname(sort(as.POSIXct(
    vapply(
      manifest_paths,
      function(path) as.numeric(readRDS(path)$available_at),
      numeric(1L)
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )))
  expect_equal(manifest_times, c(available_at_1, available_at_2))

  options(brfutures.b3_reference_download_hook = function(url, destination) {
    stop("cache read attempted a download")
  })
  cached <- brf_b3_indicators_fetch(
    as.Date("2025-12-23"),
    cache_dir = cache_dir,
    refresh = FALSE,
    quiet = TRUE
  )
  expect_equal(cached, second)
})

test_that("first BIT expiry fixture reconciles the original 0.1 BTC regime", {
  lifecycle <- data.frame(
    contract = "BITJ24",
    root = "BIT",
    report_date = as.Date("2024-04-26"),
    available_at = as.POSIXct("2024-04-26 21:37:18", tz = "UTC"),
    last_trade_date = as.Date("2024-04-26"),
    expiry_date = as.Date("2024-04-26"),
    contract_multiplier = 0.1,
    source_file = "BVBG.028.02_EOD.xml",
    source_sha256 = paste(rep("1", 64L), collapse = ""),
    stringsAsFactors = FALSE
  )
  settlements <- data.frame(
    contract_code = "BITJ24",
    date = as.Date("2024-04-26"),
    available_at = as.POSIXct("2024-04-26 22:40:23", tz = "UTC"),
    settlement_price = 327350.71,
    settlement_status = "F",
    previous_settlement = 335123.61,
    previous_settlement_status = "F",
    source_file = "BVBG.187.01.xml",
    source_sha256 = paste(rep("2", 64L), collapse = ""),
    stringsAsFactors = FALSE
  )
  indicators <- data.frame(
    indicator = c("BTCLIQUSD", "RTDOL-D1", "RTBITLIQ"),
    canonical_indicator = c("NQBTCS", "RTDOL-D1", "RTBITLIQ"),
    reference_date = rep(as.Date("2024-04-26"), 3L),
    value = c(64004.44, 5.1145, 327350.71),
    available_at = rep(
      as.POSIXct("2024-04-26 22:45:00", tz = "UTC"),
      3L
    ),
    source_file = rep("ID240426.ex_", 3L),
    source_sha256 = rep(paste(rep("3", 64L), collapse = ""), 3L),
    stringsAsFactors = FALSE
  )

  terminal <- brf_b3_bit_terminal_assemble(
    settlements,
    indicators,
    lifecycle
  )
  expect_equal(terminal$contract_size_btc, 0.1)
  expect_equal(terminal$raw_formula_price_brl, 327350.70838)
  expect_equal(terminal$calculated_settlement_brl, 327350.71)
  expect_true(terminal$formula_reconciled)
  expect_true(terminal$direct_brl_reconciled)
})
