.brf_b3_bit_source_schema_id <- function() {
  "brfutures_b3_bit_sources_v2"
}

.brf_b3_bit_source_schema_version <- function() {
  2L
}

.brf_b3_bit_source_schema_validate <- function(x, label) {
  required <- c("source_schema_id", "source_schema_version")
  missing <- setdiff(required, names(x))
  valid <- !length(missing)
  if (valid) {
    schema_id <- trimws(as.character(x$source_schema_id))
    schema_version <- suppressWarnings(as.numeric(x$source_schema_version))
    valid <- length(schema_id) == nrow(x) &&
      length(schema_version) == nrow(x) &&
      !anyNA(schema_id) &&
      !anyNA(schema_version) &&
      all(nzchar(schema_id)) &&
      all(schema_id == .brf_b3_bit_source_schema_id()) &&
      all(is.finite(schema_version)) &&
      all(schema_version == .brf_b3_bit_source_schema_version())
  }
  if (!valid) {
    stop(
      label,
      " must use ",
      .brf_b3_bit_source_schema_id(),
      " schema version ",
      .brf_b3_bit_source_schema_version(),
      "; rebuild version-1 rows from retained official sources.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.brf_b3_source_file_sha256 <- function(path) {
  if (!is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("A readable B3 source file is required.", call. = FALSE)
  }
  tolower(digest::digest(
    path,
    algo = "sha256",
    serialize = FALSE,
    file = TRUE
  ))
}

.brf_b3_parse_timestamp <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  normalized <- sub(
    "([+-][0-9]{2}):([0-9]{2})$",
    "\\1\\2",
    x,
    perl = TRUE
  )
  out <- as.POSIXct(
    rep(NA_real_, length(normalized)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  formats <- c(
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS"
  )
  for (format in formats) {
    missing <- is.na(out) & !is.na(normalized)
    if (!any(missing)) {
      break
    }
    parsed <- suppressWarnings(as.POSIXct(
      normalized[missing],
      format = format,
      tz = "UTC"
    ))
    out[missing] <- parsed
  }
  out
}

.brf_b3_xml_local_text <- function(node, xpath) {
  .brf_bvbg_xml_text(node, xpath)
}

.brf_b3_lifecycle_empty <- function() {
  data.frame(
    contract = character(),
    root = character(),
    source_schema_id = character(),
    source_schema_version = integer(),
    source_parser = character(),
    report_date = as.Date(character()),
    available_at = as.POSIXct(character(), tz = "UTC"),
    update_type = character(),
    activity_indicator = logical(),
    expiry_date = as.Date(character()),
    trading_start_date = as.Date(character()),
    last_trade_date = as.Date(character()),
    contract_multiplier = numeric(),
    contract_size_btc = numeric(),
    contract_size_regime = character(),
    contract_size_effective_from = as.Date(character()),
    contract_size_effective_to = as.Date(character()),
    position_conversion_asof_date = as.Date(character()),
    position_conversion_effective_date = as.Date(character()),
    position_conversion_ratio = numeric(),
    administrative_position_transform = character(),
    specification_source = character(),
    quote_currency = character(),
    instrument_id = character(),
    isin = character(),
    source_report_type = character(),
    source_group_id = character(),
    source_group_created_at = character(),
    source_message_id = character(),
    source_file = character(),
    source_sha256 = character(),
    source_archive_file = character(),
    source_archive_sha256 = character(),
    source_archive_entry = character(),
    stringsAsFactors = FALSE
  )
}

.brf_b3_xml_tag_value <- function(text, tag) {
  namespace <- "(?:[[:alnum:]_.-]+:)?"
  pattern <- paste0(
    "<", namespace, tag, "(?:\\s[^>]*)?>\\s*",
    "([^<]+?)\\s*</", namespace, tag, "\\s*>"
  )
  matched <- regexec(pattern, text, perl = TRUE)
  pieces <- regmatches(text, matched)[[1L]]
  if (length(pieces) < 2L) {
    return(NA_character_)
  }
  trimws(pieces[[2L]])
}

.brf_b3_lifecycle_parse_group <- function(block,
                                          root,
                                          source_report_type,
                                          source_group_id,
                                          source_group_created_at,
                                          source_file,
                                          source_sha256) {
  fragment <- paste0(
    "<Root xmlns=\"urn:bvmf.052.01.xsd\">",
    paste(block, collapse = "\n"),
    "</Root>"
  )
  doc <- xml2::read_xml(fragment)
  groups <- xml2::xml_find_all(
    doc,
    "./*[local-name()='BizGrp']"
  )
  if (length(groups) != 1L) {
    stop("Malformed BVBG.028 BizGrp fragment.", call. = FALSE)
  }
  group <- groups[[1L]]
  instrument <- xml2::xml_find_first(
    group,
    ".//*[local-name()='Instrm']"
  )
  if (inherits(instrument, "xml_missing")) {
    return(list())
  }
  parsed_root <- toupper(.brf_b3_xml_local_text(
    instrument,
    "./*[local-name()='FinInstrmAttrCmon']/*[local-name()='Asst']"
  ))
  if (is.na(parsed_root) ||
      (!is.null(root) && !identical(parsed_root, root))) {
    return(list())
  }
  nodes <- xml2::xml_find_all(
    instrument,
    ".//*[local-name()='FutrCtrctsInf']"
  )
  if (!length(nodes)) {
    return(list())
  }
  report_date <- as.Date(.brf_b3_xml_local_text(
    instrument,
    paste0(
      "./*[local-name()='RptParams']",
      "/*[local-name()='RptDtAndTm']/*[local-name()='Dt']"
    )
  ))
  available_at <- .brf_b3_parse_timestamp(.brf_b3_xml_local_text(
    group,
    "./*[local-name()='AppHdr']/*[local-name()='CreDt']"
  ))
  update_type <- .brf_b3_xml_local_text(
    instrument,
    "./*[local-name()='RptParams']/*[local-name()='UpdTp']"
  )
  activity_raw <- tolower(.brf_b3_xml_local_text(
    instrument,
    "./*[local-name()='RptParams']/*[local-name()='ActvtyInd']"
  ))
  instrument_id <- .brf_b3_xml_local_text(
    instrument,
    paste0(
      "./*[local-name()='FinInstrmId']",
      "/*[local-name()='OthrId']/*[local-name()='Id']"
    )
  )
  source_message_id <- .brf_b3_xml_local_text(
    group,
    "./*[local-name()='AppHdr']/*[local-name()='BizMsgIdr']"
  )

  rows <- lapply(nodes, function(node) {
    contract <- toupper(.brf_b3_xml_local_text(
      node,
      "./*[local-name()='TckrSymb']"
    ))
    if (is.na(contract) || !.brf_bvbg_is_future_code(contract)) {
      return(NULL)
    }
    contract_multiplier <- .brf_bvbg_xml_number(
      .brf_b3_xml_local_text(
        node,
        "./*[local-name()='CtrctMltplr']"
      )
    )
    is_bit <- identical(parsed_root, "BIT")
    size_metadata <- if (is_bit) {
      .brf_b3_bit_contract_size_metadata(contract_multiplier)
    } else {
      list(
        contract_size_regime = NA_character_,
        contract_size_effective_from = as.Date(NA),
        contract_size_effective_to = as.Date(NA),
        position_conversion_asof_date = as.Date(NA),
        position_conversion_effective_date = as.Date(NA),
        position_conversion_ratio = NA_real_,
        administrative_position_transform = NA_character_,
        specification_source = "B3 BVBG.028 CtrctMltplr"
      )
    }
    data.frame(
      contract = contract,
      root = parsed_root,
      source_schema_id = .brf_b3_bit_source_schema_id(),
      source_schema_version = .brf_b3_bit_source_schema_version(),
      source_parser = "bounded_bizgrp_stream_v1",
      report_date = report_date,
      available_at = available_at,
      update_type = update_type,
      activity_indicator = activity_raw %in% c("true", "1", "y"),
      expiry_date = as.Date(.brf_b3_xml_local_text(
        node,
        "./*[local-name()='XprtnDt']"
      )),
      trading_start_date = as.Date(.brf_b3_xml_local_text(
        node,
        "./*[local-name()='TradgStartDt']"
      )),
      last_trade_date = as.Date(.brf_b3_xml_local_text(
        node,
        "./*[local-name()='TradgEndDt']"
      )),
      contract_multiplier = contract_multiplier,
      contract_size_btc = if (is_bit) contract_multiplier else NA_real_,
      contract_size_regime = size_metadata$contract_size_regime,
      contract_size_effective_from =
        size_metadata$contract_size_effective_from,
      contract_size_effective_to =
        size_metadata$contract_size_effective_to,
      position_conversion_asof_date =
        size_metadata$position_conversion_asof_date,
      position_conversion_effective_date =
        size_metadata$position_conversion_effective_date,
      position_conversion_ratio =
        size_metadata$position_conversion_ratio,
      administrative_position_transform =
        size_metadata$administrative_position_transform,
      specification_source = size_metadata$specification_source,
      quote_currency = .brf_b3_xml_local_text(
        node,
        "./*[local-name()='TradgCcy']"
      ),
      instrument_id = instrument_id,
      isin = .brf_b3_xml_local_text(
        node,
        "./*[local-name()='ISIN']"
      ),
      source_report_type = source_report_type,
      source_group_id = source_group_id,
      source_group_created_at = source_group_created_at,
      source_message_id = source_message_id,
      source_file = source_file,
      source_sha256 = source_sha256,
      source_archive_file = NA_character_,
      source_archive_sha256 = NA_character_,
      source_archive_entry = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  Filter(Negate(is.null), rows)
}

.brf_b3_contract_lifecycle_parse_one <- function(path, root) {
  if (!file.exists(path)) {
    stop("File '", path, "' not found.", call. = FALSE)
  }
  source_file <- basename(path)
  source_sha256 <- .brf_b3_source_file_sha256(path)
  namespace <- "(?:[[:alnum:]_.-]+:)?"
  group_open_pattern <- paste0(
    "<", namespace, "BizGrp(?:\\s[^>]*)?>"
  )
  group_close_pattern <- paste0(
    "</", namespace, "BizGrp\\s*>"
  )
  root_pattern <- if (is.null(root)) {
    NULL
  } else {
    paste0(
      "<", namespace, "Asst(?:\\s[^>]*)?>\\s*",
      root,
      "\\s*</", namespace, "Asst\\s*>"
    )
  }
  future_pattern <- paste0(
    "<", namespace, "FutrCtrctsInf(?:\\s[^>]*)?>"
  )
  connection <- file(path, open = "rt", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  pending <- character()
  header <- character()
  header_complete <- FALSE
  tail_lines <- character()
  selected_blocks <- list()

  repeat {
    lines <- readLines(
      connection,
      n = 50000L,
      warn = FALSE,
      encoding = "UTF-8"
    )
    if (!length(lines)) {
      break
    }
    tail_lines <- tail(c(tail_lines, lines), 20L)
    combined <- c(pending, lines)
    starts <- grep(group_open_pattern, combined, perl = TRUE)
    ends <- grep(group_close_pattern, combined, perl = TRUE)

    if (!header_complete) {
      if (length(starts)) {
        first_start <- starts[[1L]]
        if (first_start > 1L) {
          header <- c(header, combined[seq_len(first_start - 1L)])
        }
        header_complete <- TRUE
      } else {
        header <- c(header, combined)
      }
    }

    future_hits <- grep(future_pattern, combined, perl = TRUE)
    root_hits <- if (is.null(root_pattern)) {
      future_hits
    } else {
      grep(root_pattern, combined, perl = TRUE)
    }
    if (length(root_hits) && length(starts) && length(ends) &&
        length(future_hits)) {
      block_keys <- character()
      for (hit in root_hits) {
        start_candidates <- starts[starts <= hit]
        end_candidates <- ends[ends >= hit]
        if (!length(start_candidates) || !length(end_candidates)) {
          next
        }
        start <- tail(start_candidates, 1L)
        end <- end_candidates[[1L]]
        if (!any(future_hits >= start & future_hits <= end)) {
          next
        }
        key <- paste(start, end, sep = ":")
        if (!key %in% block_keys) {
          selected_blocks[[length(selected_blocks) + 1L]] <-
            combined[start:end]
          block_keys <- c(block_keys, key)
        }
      }
    }

    last_start <- if (length(starts)) tail(starts, 1L) else integer()
    last_end <- if (length(ends)) tail(ends, 1L) else integer()
    if (length(last_start) &&
        (!length(last_end) || last_start > last_end)) {
      pending <- combined[last_start:length(combined)]
    } else {
      pending <- character()
    }
  }
  if (length(pending)) {
    stop("Truncated BVBG.028 BizGrp at end of file.", call. = FALSE)
  }
  non_empty_tail <- trimws(tail_lines[nzchar(trimws(tail_lines))])
  final_line <- if (length(non_empty_tail)) {
    tail(non_empty_tail, 1L)
  } else {
    ""
  }
  if (!header_complete ||
      !grepl(
        "</(?:[[:alnum:]_.-]+:)?Document\\s*>\\s*$",
        final_line,
        perl = TRUE
      )) {
    stop("Malformed or truncated BVBG.028 XML document.", call. = FALSE)
  }
  header_text <- paste(header, collapse = "\n")
  source_report_type <- .brf_b3_xml_tag_value(
    header_text,
    "BizGrpTp"
  )
  source_group_id <- .brf_b3_xml_tag_value(
    header_text,
    "BizGrpIdr"
  )
  source_group_created_at <- .brf_b3_xml_tag_value(
    header_text,
    "CreDtAndTm"
  )
  if (is.na(source_report_type) ||
      !grepl("^BVBG\\.028(?:\\.|$)", source_report_type)) {
    stop(
      "Expected a BVBG.028 instrument snapshot in '",
      basename(path),
      "'.",
      call. = FALSE
    )
  }
  if (!length(selected_blocks)) {
    return(.brf_b3_lifecycle_empty())
  }
  rows <- unlist(lapply(
    selected_blocks,
    .brf_b3_lifecycle_parse_group,
    root = root,
    source_report_type = source_report_type,
    source_group_id = source_group_id,
    source_group_created_at = source_group_created_at,
    source_file = source_file,
    source_sha256 = source_sha256
  ), recursive = FALSE)
  if (!length(rows)) {
    return(.brf_b3_lifecycle_empty())
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.brf_b3_contract_lifecycle_finalize <- function(frames, latest = TRUE) {
  frames <- Filter(function(x) nrow(x) > 0L, frames)
  if (!length(frames)) {
    return(.brf_b3_lifecycle_empty())
  }
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  invalid_key <- is.na(out$contract) | !nzchar(out$contract) |
    is.na(out$report_date) | is.na(out$available_at)
  if (any(invalid_key)) {
    stop(
      "BVBG.028 lifecycle rows require contract, report_date and official ",
      "AppHdr available_at.",
      call. = FALSE
    )
  }
  if (any(is.na(out$last_trade_date)) ||
      any(is.na(out$expiry_date)) ||
      any(!is.finite(out$contract_multiplier)) ||
      any(out$contract_multiplier <= 0)) {
    stop(
      "BVBG.028 lifecycle rows require valid expiry, last-trade date and ",
      "positive contract multiplier.",
      call. = FALSE
    )
  }
  out <- out[order(
    out$contract,
    out$report_date,
    out$available_at,
    out$source_sha256
  ), , drop = FALSE]
  observation_key <- paste(
    out$contract,
    out$report_date,
    format(out$available_at, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"),
    out$source_sha256,
    sep = "|"
  )
  out <- out[!duplicated(observation_key), , drop = FALSE]
  if (isTRUE(latest)) {
    key <- paste(out$contract, out$report_date, sep = "|")
    out <- out[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

#' Read causal B3 futures contract lifecycle snapshots
#'
#' Parses one or more extracted `BVBG.028` XML instrument files. Each returned
#' row retains the official application-header timestamp and source
#' fingerprint. When several snapshots describe the same contract and report
#' date, `latest = TRUE` selects the row with the latest official
#' `AppHdr/CreDt`; file modification time and input order are never used.
#'
#' BIT rows also expose the `0.1` to `0.01` BTC size transition and the
#' Circular Letter 013/2025-VPC administrative transform: positions open at
#' the end of 2025-06-13 become ten times as many contracts on 2025-06-16.
#' This is an explicit quantity event, not P&L and not a silent multiplier
#' substitution.
#'
#' This is a source parser, not an execution-support declaration.
#'
#' @param paths Character vector of extracted `BVBG.028` XML paths.
#' @param root B3 futures root to retain. Defaults to `"BIT"`.
#' @param latest Whether to retain only the latest official snapshot per
#'   contract and report date.
#' @return A data frame of versioned contract lifecycle observations and source
#'   provenance.
#' @export
brf_b3_contract_lifecycle_read <- function(paths,
                                           root = "BIT",
                                           latest = TRUE) {
  paths <- path.expand(as.character(paths))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (!length(paths)) {
    stop("At least one BVBG.028 XML path is required.", call. = FALSE)
  }
  if (!is.null(root)) {
    root <- .brf_normalize_root(root)
  }
  frames <- lapply(
    paths,
    .brf_b3_contract_lifecycle_parse_one,
    root = root
  )
  .brf_b3_contract_lifecycle_finalize(frames, latest = latest)
}

.brf_b3_indicator_canonical <- function(indicator) {
  indicator <- toupper(trimws(as.character(indicator)))
  out <- indicator
  out[indicator %in% c("BTCLIQUSD", "NQBTCS")] <- "NQBTCS"
  out[indicator == "RTDOL-D1"] <- "RTDOL-D1"
  out[indicator %in% c("RTBITLIQ", "BTCLIQBRL")] <- "RTBITLIQ"
  out
}

.brf_b3_indicator_report_date <- function(source_file) {
  source_file <- basename(as.character(source_file)[1L])
  matched <- regexec(
    "(?i)(?:^|[^A-Z0-9])ID([0-9]{6})(?:[^0-9]|$)",
    source_file,
    perl = TRUE
  )
  pieces <- regmatches(source_file, matched)[[1L]]
  if (length(pieces) < 2L) {
    return(as.Date(NA))
  }
  suppressWarnings(as.Date(pieces[[2L]], format = "%y%m%d"))
}

#' Read official B3 indicator observations used by BIT settlement
#'
#' Parses the fixed-width `Indic.txt` payload distributed in B3's daily
#' indicator archive. The defaults retain the Nasdaq Bitcoin liquidation
#' reference (`BTCLIQUSD`), the B3 D+1 BRL/USD rate (`RTDOL-D1`), and B3's
#' directly published BRL liquidation result (`RTBITLIQ`). `NQBTCS` and
#' `BTCLIQBRL` are accepted aliases but are never manufactured when absent.
#'
#' `Indic.txt` does not embed an authoritative publication timestamp. Callers
#' must therefore provide `available_at` from the source-delivery evidence;
#' download time is not a historical substitute.
#'
#' @param path Path to the extracted `Indic.txt`.
#' @param report_date Date of the B3 daily indicator archive. It may be omitted
#'   only when `source_file` contains a name such as `ID251223.ex_`.
#' @param available_at Causal publication/availability timestamp with a time
#'   zone.
#' @param source_file Original archive or payload name retained as provenance.
#' @param indicators Indicator labels to retain.
#' @return A data frame of fixed-width indicator observations and provenance.
#' @export
brf_b3_indicators_read <- function(
    path,
    report_date = NULL,
    available_at,
    source_file = basename(path),
    indicators = c("BTCLIQUSD", "RTDOL-D1", "RTBITLIQ")) {
  if (!file.exists(path)) {
    stop("File '", path, "' not found.", call. = FALSE)
  }
  source_file <- as.character(source_file)
  if (length(source_file) != 1L || is.na(source_file) ||
      !nzchar(trimws(source_file))) {
    stop("source_file must be one non-empty file name.", call. = FALSE)
  }
  if (is.null(report_date)) {
    report_date <- .brf_b3_indicator_report_date(source_file)
  } else {
    report_date <- tryCatch(
      as.Date(report_date),
      error = function(e) as.Date(NA)
    )
  }
  if (length(report_date) != 1L || is.na(report_date)) {
    stop(
      "report_date is required when source_file does not encode IDyymmdd.",
      call. = FALSE
    )
  }
  available_at <- .brf_b3_parse_timestamp(available_at)
  if (length(available_at) != 1L || is.na(available_at)) {
    stop(
      "available_at must be one causal timestamp from source-delivery ",
      "evidence.",
      call. = FALSE
    )
  }
  indicators <- unique(toupper(trimws(as.character(indicators))))
  indicators <- indicators[!is.na(indicators) & nzchar(indicators)]
  if (!length(indicators)) {
    stop("At least one indicator label is required.", call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nchar(lines, type = "chars") >= 73L]
  if (!length(lines)) {
    return(data.frame(
      indicator = character(),
      canonical_indicator = character(),
      source_schema_id = character(),
      source_schema_version = integer(),
      reference_date = as.Date(character()),
      value = numeric(),
      decimal_places = integer(),
      report_date = as.Date(character()),
      available_at = as.POSIXct(character(), tz = "UTC"),
      source_file = character(),
      source_sha256 = character(),
      stringsAsFactors = FALSE
    ))
  }
  indicator <- toupper(trimws(substr(lines, 20L, 46L)))
  keep <- indicator %in% indicators
  lines <- lines[keep]
  indicator <- indicator[keep]
  if (!length(lines)) {
    return(data.frame(
      indicator = character(),
      canonical_indicator = character(),
      source_schema_id = character(),
      source_schema_version = integer(),
      reference_date = as.Date(character()),
      value = numeric(),
      decimal_places = integer(),
      report_date = as.Date(character()),
      available_at = as.POSIXct(character(), tz = "UTC"),
      source_file = character(),
      source_sha256 = character(),
      stringsAsFactors = FALSE
    ))
  }
  reference_date <- suppressWarnings(as.Date(
    substr(lines, 12L, 19L),
    format = "%Y%m%d"
  ))
  sign <- substr(lines, 47L, 47L)
  mantissa <- suppressWarnings(as.numeric(substr(lines, 48L, 71L)))
  decimal_places <- suppressWarnings(as.integer(substr(lines, 72L, 73L)))
  value <- mantissa / (10 ^ decimal_places)
  value[sign == "-"] <- -value[sign == "-"]
  invalid <- is.na(reference_date) | !is.finite(value) |
    is.na(decimal_places) | decimal_places < 0L
  if (any(invalid)) {
    stop(
      "Selected Indic.txt rows contain invalid dates, mantissas or scales.",
      call. = FALSE
    )
  }
  out <- data.frame(
    indicator = indicator,
    canonical_indicator = .brf_b3_indicator_canonical(indicator),
    source_schema_id = rep(
      .brf_b3_bit_source_schema_id(),
      length(lines)
    ),
    source_schema_version = rep(
      .brf_b3_bit_source_schema_version(),
      length(lines)
    ),
    reference_date = reference_date,
    value = value,
    decimal_places = decimal_places,
    report_date = rep(as.Date(report_date), length(lines)),
    available_at = rep(available_at, length(lines)),
    source_file = rep(source_file, length(lines)),
    source_sha256 = rep(
      .brf_b3_source_file_sha256(path),
      length(lines)
    ),
    stringsAsFactors = FALSE
  )
  out <- out[order(
    out$reference_date,
    out$canonical_indicator,
    out$indicator
  ), , drop = FALSE]
  key <- paste(out$indicator, out$reference_date, sep = "|")
  duplicate <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(duplicate)) {
    split_values <- split(out$value[duplicate], key[duplicate])
    conflicting <- vapply(
      split_values,
      function(x) length(unique(x)) > 1L,
      logical(1L)
    )
    if (any(conflicting)) {
      stop(
        "Indic.txt contains conflicting duplicate observations for: ",
        paste(names(conflicting)[conflicting], collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    out <- out[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

.brf_b3_required_column <- function(data, candidates, label) {
  found <- candidates[candidates %in% names(data)]
  if (!length(found)) {
    stop(
      "Missing ", label, " column; expected one of: ",
      paste(candidates, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  found[[1L]]
}

.brf_b3_calendar_schema_id <- function() {
  "brfutures_b3_calendar_evidence_v1"
}

.brf_b3_calendar_schema_version <- function() {
  1L
}

.brf_b3_calendar_scope <- function() {
  "b3_listed_derivatives_and_clearing"
}

.brf_b3_calendar_parse_business_day <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    out <- rep(NA, length(x))
    out[x == 0] <- FALSE
    out[x == 1] <- TRUE
    return(out)
  }
  normalized <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(normalized))
  out[normalized %in% c("true", "t", "1", "business_day", "session")] <- TRUE
  out[normalized %in% c("false", "f", "0", "closed", "no_session")] <- FALSE
  out
}

.brf_b3_calendar_fingerprint <- function(calendar) {
  payload <- calendar
  payload$calendar_fingerprint <- NULL
  payload <- payload[order(payload$date), , drop = FALSE]
  rownames(payload) <- NULL
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

#' Read versioned B3 business-calendar evidence
#'
#' Reads a normalized calendar CSV reviewed against a separately hashed B3
#' source document. The CSV must either enumerate every calendar date and its
#' explicit business-day status, or enumerate the B3 session dates inside an
#' explicit coverage interval. The latter is expanded without applying a
#' weekday rule: a date is open only when it is present in the normalized
#' artifact.
#'
#' This function does not turn a free-form caller date into official evidence.
#' It preserves separate hashes for the normalized payload and official source
#' document, the normalization method/version, explicit review attestation,
#' B3 source reference, causal availability timestamp, coverage and a
#' deterministic normalized fingerprint.
#'
#' @param path Readable normalized CSV calendar artifact.
#' @param source_document_path Readable official B3 source document used to
#'   normalize and review the CSV. Its SHA-256 is stored separately.
#' @param available_at Timestamp at which this exact calendar artifact was
#'   available. It must include a time component.
#' @param source_reference Public B3 URL or other B3 HTTPS source reference for
#'   `source_document_path`.
#' @param calendar_id Stable identifier for the calendar represented by the
#'   artifact.
#' @param normalization_method Explicit reviewed normalization method. Either
#'   `"manual_transcription_reviewed"` or `"machine_parse_reviewed"`.
#' @param normalization_version Stable non-empty version of the normalization
#'   procedure.
#' @param reviewer Stable non-empty identifier for the reviewer.
#' @param reviewed_at Timestamp at which the normalized calendar was reviewed
#'   against the hashed official source. It cannot follow `available_at`.
#' @param review_attestation Must be the explicit literal
#'   `"reviewed_against_hashed_b3_source"`.
#' @param calendar_kind Either `"complete_daily_status"` for a contiguous
#'   daily file with an explicit status column, or `"session_dates"` for a file
#'   listing every B3 business/session date in an explicit coverage interval.
#' @param coverage_start,coverage_end Required inclusive coverage interval for
#'   `calendar_kind = "session_dates"`. For complete daily status these are
#'   inferred and, when supplied, must match the file exactly.
#' @param date_column Name of the date column in the CSV.
#' @param business_day_column Name of the explicit business-day status column
#'   for `calendar_kind = "complete_daily_status"`.
#' @param normalized_file Stable normalized CSV filename stored in the
#'   evidence. Defaults to the basename of `path`.
#' @param source_document_file Stable official source-document filename stored
#'   in the evidence. Defaults to the basename of `source_document_path`.
#' @return A contiguous daily calendar evidence data frame with source and
#'   schema provenance.
#' @export
brf_b3_calendar_evidence_read <- function(
    path,
    source_document_path,
    available_at,
    source_reference,
    calendar_id,
    normalization_method,
    normalization_version,
    reviewer,
    reviewed_at,
    review_attestation,
    calendar_kind = c("complete_daily_status", "session_dates"),
    coverage_start = NULL,
    coverage_end = NULL,
    date_column = "date",
    business_day_column = "is_business_day",
    normalized_file = basename(path),
    source_document_file = basename(source_document_path)) {
  calendar_kind <- match.arg(calendar_kind)
  if (!is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("A readable B3 calendar artifact is required.", call. = FALSE)
  }
  if (!is.character(source_document_path) ||
      length(source_document_path) != 1L ||
      is.na(source_document_path) ||
      !nzchar(source_document_path) ||
      !file.exists(source_document_path)) {
    stop(
      "A readable official B3 calendar source document is required.",
      call. = FALSE
    )
  }
  if (identical(
    normalizePath(path, mustWork = TRUE),
    normalizePath(source_document_path, mustWork = TRUE)
  )) {
    stop(
      "The normalized CSV and official B3 source document must be separate ",
      "files.",
      call. = FALSE
    )
  }
  available_at <- .brf_b3_parse_timestamp(available_at)
  if (length(available_at) != 1L || is.na(available_at)) {
    stop(
      "available_at must be one causal timestamp for the calendar artifact.",
      call. = FALSE
    )
  }
  if (!is.character(source_reference) ||
      length(source_reference) != 1L ||
      is.na(source_reference) ||
      !grepl(
        "^https://(?:www\\.)?b3\\.com\\.br/",
        source_reference,
        perl = TRUE,
        ignore.case = TRUE
      )) {
    stop(
      "source_reference must be one public HTTPS b3.com.br reference.",
      call. = FALSE
    )
  }
  if (!is.character(calendar_id) || length(calendar_id) != 1L ||
      is.na(calendar_id) || !nzchar(trimws(calendar_id))) {
    stop("calendar_id must be one non-empty identifier.", call. = FALSE)
  }
  normalization_method <- match.arg(
    normalization_method,
    c("manual_transcription_reviewed", "machine_parse_reviewed")
  )
  if (!is.character(normalization_version) ||
      length(normalization_version) != 1L ||
      is.na(normalization_version) ||
      !nzchar(trimws(normalization_version))) {
    stop(
      "normalization_version must be one non-empty identifier.",
      call. = FALSE
    )
  }
  if (!is.character(reviewer) || length(reviewer) != 1L ||
      is.na(reviewer) || !nzchar(trimws(reviewer))) {
    stop("reviewer must be one non-empty identifier.", call. = FALSE)
  }
  reviewed_at <- .brf_b3_parse_timestamp(reviewed_at)
  if (length(reviewed_at) != 1L || is.na(reviewed_at) ||
      reviewed_at > available_at) {
    stop(
      "reviewed_at must be one timestamp no later than available_at.",
      call. = FALSE
    )
  }
  if (!is.character(review_attestation) ||
      length(review_attestation) != 1L ||
      is.na(review_attestation) ||
      !identical(
        review_attestation,
        "reviewed_against_hashed_b3_source"
      )) {
    stop(
      "review_attestation must explicitly equal ",
      "'reviewed_against_hashed_b3_source'.",
      call. = FALSE
    )
  }
  for (file_field in c("normalized_file", "source_document_file")) {
    value <- get(file_field, inherits = FALSE)
    if (!is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(trimws(value))) {
      stop(file_field, " must be one non-empty filename.", call. = FALSE)
    }
  }
  source <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!date_column %in% names(source)) {
    stop(
      "B3 calendar artifact is missing date column `",
      date_column,
      "`.",
      call. = FALSE
    )
  }
  dates <- suppressWarnings(as.Date(source[[date_column]]))
  if (!length(dates) || anyNA(dates) || anyDuplicated(dates)) {
    stop(
      "B3 calendar artifact dates must be non-empty, valid and unique.",
      call. = FALSE
    )
  }

  if (identical(calendar_kind, "complete_daily_status")) {
    if (!business_day_column %in% names(source)) {
      stop(
        "Complete B3 calendar artifact is missing business-day column `",
        business_day_column,
        "`.",
        call. = FALSE
      )
    }
    business_day <- .brf_b3_calendar_parse_business_day(
      source[[business_day_column]]
    )
    if (anyNA(business_day)) {
      stop(
        "Complete B3 calendar business-day statuses must be explicit.",
        call. = FALSE
      )
    }
    expected_dates <- seq(min(dates), max(dates), by = "day")
    if (!identical(
      as.numeric(sort(dates)),
      as.numeric(expected_dates)
    )) {
      stop(
        "Complete B3 calendar artifact must contain every calendar date ",
        "in its coverage interval.",
        call. = FALSE
      )
    }
    if (is.null(coverage_start)) {
      coverage_start <- min(dates)
    }
    if (is.null(coverage_end)) {
      coverage_end <- max(dates)
    }
    coverage_start <- as.Date(coverage_start)
    coverage_end <- as.Date(coverage_end)
    if (length(coverage_start) != 1L || length(coverage_end) != 1L ||
        anyNA(c(coverage_start, coverage_end)) ||
        coverage_start != min(dates) || coverage_end != max(dates)) {
      stop(
        "Complete B3 calendar coverage must match its first and last dates.",
        call. = FALSE
      )
    }
    order_index <- order(dates)
    dates <- dates[order_index]
    business_day <- business_day[order_index]
  } else {
    coverage_start <- as.Date(coverage_start)
    coverage_end <- as.Date(coverage_end)
    if (length(coverage_start) != 1L || length(coverage_end) != 1L ||
        anyNA(c(coverage_start, coverage_end)) ||
        coverage_end < coverage_start) {
      stop(
        "session_dates evidence requires one valid coverage_start and ",
        "coverage_end.",
        call. = FALSE
      )
    }
    if (any(dates < coverage_start | dates > coverage_end)) {
      stop(
        "B3 session dates fall outside the declared coverage interval.",
        call. = FALSE
      )
    }
    expanded_dates <- seq(coverage_start, coverage_end, by = "day")
    business_day <- expanded_dates %in% dates
    dates <- expanded_dates
  }
  if (!any(business_day)) {
    stop(
      "B3 calendar artifact does not contain any business/session date.",
      call. = FALSE
    )
  }

  normalized_sha256 <- .brf_b3_source_file_sha256(path)
  source_document_sha256 <- .brf_b3_source_file_sha256(
    source_document_path
  )
  out <- data.frame(
    date = dates,
    is_business_day = business_day,
    calendar_schema_id = .brf_b3_calendar_schema_id(),
    calendar_schema_version = .brf_b3_calendar_schema_version(),
    calendar_scope = .brf_b3_calendar_scope(),
    calendar_id = trimws(calendar_id),
    calendar_kind = calendar_kind,
    coverage_start = rep(coverage_start, length(dates)),
    coverage_end = rep(coverage_end, length(dates)),
    available_at = rep(available_at, length(dates)),
    source_authority = "B3",
    source_reference = source_reference,
    source_document_file = trimws(source_document_file),
    source_document_sha256 = source_document_sha256,
    normalized_file = trimws(normalized_file),
    normalized_sha256 = normalized_sha256,
    normalization_method = normalization_method,
    normalization_version = trimws(normalization_version),
    reviewer = trimws(reviewer),
    reviewed_at = rep(reviewed_at, length(dates)),
    review_attestation = review_attestation,
    calendar_fingerprint = NA_character_,
    stringsAsFactors = FALSE
  )
  out$calendar_fingerprint <- .brf_b3_calendar_fingerprint(out)
  out
}

.brf_b3_calendar_validate <- function(calendar) {
  if (!inherits(calendar, "data.frame") || !nrow(calendar)) {
    stop(
      "calendar_evidence must be a non-empty data frame from ",
      "brf_b3_calendar_evidence_read().",
      call. = FALSE
    )
  }
  required <- c(
    "date", "is_business_day", "calendar_schema_id",
    "calendar_schema_version", "calendar_scope", "calendar_id", "calendar_kind",
    "coverage_start", "coverage_end", "available_at", "source_authority",
    "source_reference", "source_document_file", "source_document_sha256",
    "normalized_file", "normalized_sha256", "normalization_method",
    "normalization_version", "reviewer", "reviewed_at",
    "review_attestation",
    "calendar_fingerprint"
  )
  missing <- setdiff(required, names(calendar))
  if (length(missing)) {
    stop(
      "calendar_evidence is missing required fields: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  out <- calendar[, required, drop = FALSE]
  out$date <- as.Date(out$date)
  out$is_business_day <- .brf_b3_calendar_parse_business_day(
    out$is_business_day
  )
  out$coverage_start <- as.Date(out$coverage_start)
  out$coverage_end <- as.Date(out$coverage_end)
  out$available_at <- .brf_b3_parse_timestamp(out$available_at)
  out$reviewed_at <- .brf_b3_parse_timestamp(out$reviewed_at)
  scalar_fields <- c(
    "calendar_schema_id", "calendar_schema_version", "calendar_scope",
    "calendar_id",
    "calendar_kind", "coverage_start", "coverage_end", "available_at",
    "source_authority", "source_reference", "source_document_file",
    "source_document_sha256", "normalized_file", "normalized_sha256",
    "normalization_method", "normalization_version", "reviewer",
    "reviewed_at", "review_attestation",
    "calendar_fingerprint"
  )
  inconsistent <- vapply(
    scalar_fields,
    function(field) length(unique(out[[field]])) != 1L,
    logical(1L)
  )
  if (any(inconsistent)) {
    stop(
      "calendar_evidence provenance and coverage fields must be constant.",
      call. = FALSE
    )
  }
  if (!identical(
    out$calendar_schema_id[[1L]],
    .brf_b3_calendar_schema_id()
  ) || !identical(
    as.integer(out$calendar_schema_version[[1L]]),
    .brf_b3_calendar_schema_version()
  )) {
    stop("Unsupported B3 calendar evidence schema.", call. = FALSE)
  }
  if (!identical(
    out$calendar_scope[[1L]],
    .brf_b3_calendar_scope()
  )) {
    stop(
      "B3 calendar evidence must cover listed derivatives and clearing.",
      call. = FALSE
    )
  }
  if (!identical(out$source_authority[[1L]], "B3") ||
      !nzchar(trimws(out$calendar_id[[1L]])) ||
      !out$calendar_kind[[1L]] %in%
        c("complete_daily_status", "session_dates") ||
      !grepl(
        "^https://(?:www\\.)?b3\\.com\\.br/",
        out$source_reference[[1L]],
        perl = TRUE,
        ignore.case = TRUE
      ) ||
      !nzchar(trimws(out$source_document_file[[1L]])) ||
      !nzchar(trimws(out$normalized_file[[1L]])) ||
      !grepl("^[0-9a-f]{64}$", out$source_document_sha256[[1L]]) ||
      !grepl("^[0-9a-f]{64}$", out$normalized_sha256[[1L]]) ||
      !grepl("^[0-9a-f]{64}$", out$calendar_fingerprint[[1L]])) {
    stop("Invalid B3 calendar source provenance.", call. = FALSE)
  }
  if (!out$normalization_method[[1L]] %in%
      c("manual_transcription_reviewed", "machine_parse_reviewed") ||
      !nzchar(trimws(out$normalization_version[[1L]])) ||
      !nzchar(trimws(out$reviewer[[1L]])) ||
      !identical(
        out$review_attestation[[1L]],
        "reviewed_against_hashed_b3_source"
      ) ||
      is.na(out$reviewed_at[[1L]]) ||
      out$reviewed_at[[1L]] > out$available_at[[1L]]) {
    stop(
      "Invalid B3 calendar normalization review attestation.",
      call. = FALSE
    )
  }
  expected_dates <- seq(
    out$coverage_start[[1L]],
    out$coverage_end[[1L]],
    by = "day"
  )
  if (anyNA(out$date) || anyNA(out$is_business_day) ||
      anyNA(out$available_at) || anyDuplicated(out$date) ||
      !identical(
        as.numeric(sort(out$date)),
        as.numeric(expected_dates)
      )) {
    stop(
      "B3 calendar evidence must be a complete, valid daily interval.",
      call. = FALSE
    )
  }
  expected_fingerprint <- .brf_b3_calendar_fingerprint(out)
  if (!identical(
    out$calendar_fingerprint[[1L]],
    expected_fingerprint
  )) {
    stop("B3 calendar evidence fingerprint mismatch.", call. = FALSE)
  }
  out <- out[order(out$date), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.brf_b3_calendar_resolve_posting <- function(calendar, expiry_date) {
  calendar <- .brf_b3_calendar_validate(calendar)
  expiry_date <- as.Date(expiry_date)
  if (length(expiry_date) != 1L || is.na(expiry_date)) {
    stop("expiry_date must be one valid Date.", call. = FALSE)
  }
  coverage_start <- calendar$coverage_start[[1L]]
  coverage_end <- calendar$coverage_end[[1L]]
  if (expiry_date < coverage_start || expiry_date >= coverage_end) {
    stop(
      "B3 calendar evidence does not cover expiry through a later date.",
      call. = FALSE
    )
  }
  expiry_match <- match(expiry_date, calendar$date)
  if (is.na(expiry_match) ||
      !isTRUE(calendar$is_business_day[[expiry_match]])) {
    stop(
      "BIT expiry is not marked as a B3 business/session date by the ",
      "calendar evidence.",
      call. = FALSE
    )
  }
  candidates <- calendar$date[
    calendar$date > expiry_date & calendar$is_business_day
  ]
  if (!length(candidates)) {
    stop(
      "B3 calendar evidence contains no business day after BIT expiry.",
      call. = FALSE
    )
  }
  posting_date <- min(candidates)
  evidence_available_at <- calendar$available_at[[1L]]
  if (as.Date(
    evidence_available_at,
    tz = "America/Sao_Paulo"
  ) > expiry_date) {
    stop(
      "B3 calendar evidence was not causally available by BIT expiry.",
      call. = FALSE
    )
  }
  list(
    posting_date = posting_date,
    available_at = evidence_available_at,
    calendar_schema_id = calendar$calendar_schema_id[[1L]],
    calendar_schema_version =
      as.integer(calendar$calendar_schema_version[[1L]]),
    calendar_scope = calendar$calendar_scope[[1L]],
    calendar_id = calendar$calendar_id[[1L]],
    calendar_kind = calendar$calendar_kind[[1L]],
    source_reference = calendar$source_reference[[1L]],
    source_document_file = calendar$source_document_file[[1L]],
    source_document_sha256 = calendar$source_document_sha256[[1L]],
    normalized_file = calendar$normalized_file[[1L]],
    normalized_sha256 = calendar$normalized_sha256[[1L]],
    normalization_method = calendar$normalization_method[[1L]],
    normalization_version = calendar$normalization_version[[1L]],
    reviewer = calendar$reviewer[[1L]],
    reviewed_at = calendar$reviewed_at[[1L]],
    review_attestation = calendar$review_attestation[[1L]],
    calendar_fingerprint = calendar$calendar_fingerprint[[1L]]
  )
}

.brf_b3_terminal_fingerprint <- function(row) {
  payload <- unclass(row)
  payload$terminal_fingerprint <- NULL
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

#' Assemble and reconcile official B3 BIT terminal source data
#'
#' Joins terminal lifecycle, BVBG.187 final settlement fields, and the three
#' official B3 indicator observations required to prove a BIT expiry price.
#' The formula and half-up rounding are delegated to
#' [positionsizer::ps_b3_bit_final_settlement()]. This helper validates source
#' data only: every row is explicitly marked `execution_supported = FALSE`.
#'
#' No calendar date is guessed from weekdays. Prefer `calendar_evidence` from
#' [brf_b3_calendar_evidence_read()], which proves the first B3 business day
#' after expiry from a complete, hashed and causally available interval.
#' `posting_date` remains available for compatibility, but a bare caller date
#' is explicitly retained as unvalidated. Without either input, the output
#' keeps the one-business-day lag and an unresolved posting date.
#'
#' @param settlements Parsed BVBG.187 settlement rows, including final status
#'   and source provenance.
#' @param indicators Output from [brf_b3_indicators_read()].
#' @param lifecycle Output from [brf_b3_contract_lifecycle_read()].
#' @param posting_date Optional cash-posting date, scalar or one per
#'   assembled terminal row. Retained for compatibility and never labelled as
#'   calendar-validated.
#' @param strict Whether non-final statuses or reconciliation mismatches are
#'   errors. Missing inputs always fail closed.
#' @param calendar_evidence Optional versioned output from
#'   [brf_b3_calendar_evidence_read()]. It cannot be combined with
#'   `posting_date`.
#' @return A source-validation sidecar with reconciled BIT terminal prices,
#'   availability timestamps, provenance and deterministic row fingerprints.
#' @export
brf_b3_bit_terminal_assemble <- function(settlements,
                                         indicators,
                                         lifecycle,
                                         posting_date = NULL,
                                         strict = TRUE,
                                         calendar_evidence = NULL) {
  for (object_name in c("settlements", "indicators", "lifecycle")) {
    object <- get(object_name, inherits = FALSE)
    if (!inherits(object, "data.frame") || !nrow(object)) {
      stop(object_name, " must be a non-empty data frame.", call. = FALSE)
    }
    .brf_b3_bit_source_schema_validate(object, object_name)
  }
  lifecycle_contract <- .brf_b3_required_column(
    lifecycle,
    c("contract", "contract_code"),
    "lifecycle contract"
  )
  lifecycle_report_date <- .brf_b3_required_column(
    lifecycle,
    c("report_date", "session_date", "date"),
    "lifecycle report date"
  )
  lifecycle_available <- .brf_b3_required_column(
    lifecycle,
    c("available_at"),
    "lifecycle availability"
  )
  for (field in c(
    "last_trade_date", "expiry_date", "contract_multiplier",
    "source_file", "source_sha256"
  )) {
    .brf_b3_required_column(lifecycle, field, paste("lifecycle", field))
  }
  life <- lifecycle
  life$contract <- toupper(trimws(as.character(life[[lifecycle_contract]])))
  life$report_date <- as.Date(life[[lifecycle_report_date]])
  life$available_at <- .brf_b3_parse_timestamp(life[[lifecycle_available]])
  life$last_trade_date <- as.Date(life$last_trade_date)
  life$expiry_date <- as.Date(life$expiry_date)
  life$contract_multiplier <- as.numeric(life$contract_multiplier)
  life$root <- if ("root" %in% names(life)) {
    toupper(trimws(as.character(life$root)))
  } else {
    sub("([A-Z0-9]+)[A-Z][0-9]{2}$", "\\1", life$contract)
  }
  life <- life[
    life$root == "BIT" &
      life$last_trade_date == life$report_date &
      life$expiry_date == life$report_date,
    ,
    drop = FALSE
  ]
  if (!nrow(life)) {
    stop(
      "No BIT lifecycle row has report_date equal to both expiry and ",
      "last_trade_date.",
      call. = FALSE
    )
  }
  life_key <- paste(life$contract, life$report_date, sep = "|")
  life <- life[order(
    life_key,
    life$available_at,
    life$source_sha256
  ), , drop = FALSE]
  life_key <- paste(life$contract, life$report_date, sep = "|")
  life <- life[!duplicated(life_key, fromLast = TRUE), , drop = FALSE]

  settlement_contract <- .brf_b3_required_column(
    settlements,
    c("contract", "contract_code"),
    "settlement contract"
  )
  settlement_date <- .brf_b3_required_column(
    settlements,
    c("session_date", "date", "report_date"),
    "settlement session date"
  )
  settlement_price <- .brf_b3_required_column(
    settlements,
    c("official_settlement_brl", "settlement_price"),
    "official settlement"
  )
  previous_price <- .brf_b3_required_column(
    settlements,
    c("previous_official_settlement_brl", "previous_settlement"),
    "previous official settlement"
  )
  settlement_status <- .brf_b3_required_column(
    settlements,
    c("settlement_status"),
    "settlement status"
  )
  previous_status <- .brf_b3_required_column(
    settlements,
    c("previous_settlement_status"),
    "previous settlement status"
  )
  for (field in c(
    "available_at", "settlement_available_at",
    "source_file", "source_sha256"
  )) {
    .brf_b3_required_column(
      settlements,
      field,
      paste("settlement", field)
    )
  }
  settle <- settlements
  settle$contract <- toupper(trimws(as.character(
    settle[[settlement_contract]]
  )))
  settle$session_date <- as.Date(settle[[settlement_date]])
  settle$available_at <- .brf_b3_parse_timestamp(settle$available_at)
  settle$settlement_available_at <- .brf_b3_parse_timestamp(
    settle$settlement_available_at
  )
  if (anyNA(settle$available_at) ||
      anyNA(settle$settlement_available_at) ||
      any(settle$settlement_available_at > settle$available_at)) {
    stop(
      "BIT settlement evidence requires causal available_at and ",
      "settlement_available_at timestamps from the same official report.",
      call. = FALSE
    )
  }
  settle$official_settlement_brl <- as.numeric(settle[[settlement_price]])
  settle$previous_official_settlement_brl <-
    as.numeric(settle[[previous_price]])
  settle$settlement_status <- toupper(trimws(as.character(
    settle[[settlement_status]]
  )))
  settle$previous_settlement_status <- toupper(trimws(as.character(
    settle[[previous_status]]
  )))
  settle_key <- paste(settle$contract, settle$session_date, sep = "|")
  settle <- settle[order(
    settle_key,
    settle$available_at,
    settle$source_sha256
  ), , drop = FALSE]
  settle_key <- paste(settle$contract, settle$session_date, sep = "|")
  settle <- settle[!duplicated(settle_key, fromLast = TRUE), , drop = FALSE]

  for (field in c(
    "indicator", "reference_date", "value", "available_at",
    "source_file", "source_sha256"
  )) {
    .brf_b3_required_column(indicators, field, paste("indicator", field))
  }
  ind <- indicators
  if (!"canonical_indicator" %in% names(ind)) {
    ind$canonical_indicator <- .brf_b3_indicator_canonical(ind$indicator)
  }
  ind$canonical_indicator <- .brf_b3_indicator_canonical(
    ind$canonical_indicator
  )
  ind$reference_date <- as.Date(ind$reference_date)
  ind$available_at <- .brf_b3_parse_timestamp(ind$available_at)
  ind$value <- as.numeric(ind$value)
  required_indicators <- c("NQBTCS", "RTDOL-D1", "RTBITLIQ")
  ind <- ind[ind$canonical_indicator %in% required_indicators, , drop = FALSE]
  if (!nrow(ind)) {
    stop("No BIT terminal indicators were supplied.", call. = FALSE)
  }
  ind_key <- paste(ind$reference_date, ind$canonical_indicator, sep = "|")
  ind <- ind[order(
    ind_key,
    ind$available_at,
    ind$source_sha256
  ), , drop = FALSE]
  ind_key <- paste(ind$reference_date, ind$canonical_indicator, sep = "|")
  ind <- ind[!duplicated(ind_key, fromLast = TRUE), , drop = FALSE]

  if (!is.null(posting_date) && !is.null(calendar_evidence)) {
    stop(
      "Supply either posting_date or calendar_evidence, not both.",
      call. = FALSE
    )
  }
  validated_calendar <- NULL
  if (!is.null(calendar_evidence)) {
    validated_calendar <- .brf_b3_calendar_validate(calendar_evidence)
  }
  if (is.null(posting_date)) {
    posting_date <- rep(as.Date(NA), nrow(life))
    posting_status <- if (is.null(validated_calendar)) {
      rep("requires_official_b3_calendar", nrow(life))
    } else {
      rep("official_b3_business_day_calendar", nrow(life))
    }
  } else {
    posting_date <- tryCatch(
      as.Date(posting_date),
      error = function(e) as.Date(NA)
    )
    if (!length(posting_date) %in% c(1L, nrow(life)) ||
        anyNA(posting_date)) {
      stop(
        "posting_date must be one valid Date or one per terminal row.",
        call. = FALSE
      )
    }
    posting_date <- rep_len(posting_date, nrow(life))
    posting_status <- rep("caller_supplied_not_calendar_validated", nrow(life))
  }

  rows <- vector("list", nrow(life))
  for (i in seq_len(nrow(life))) {
    contract <- life$contract[[i]]
    session_date <- life$report_date[[i]]
    settlement_match <- which(
      settle$contract == contract &
        settle$session_date == session_date
    )
    if (length(settlement_match) != 1L) {
      stop(
        "Expected exactly one final settlement row for ",
        contract,
        " on ",
        session_date,
        ".",
        call. = FALSE
      )
    }
    settlement_row <- settle[settlement_match, , drop = FALSE]
    indicator_rows <- ind[ind$reference_date == session_date, , drop = FALSE]
    missing_indicators <- setdiff(
      required_indicators,
      indicator_rows$canonical_indicator
    )
    if (length(missing_indicators)) {
      stop(
        "Missing BIT terminal indicators for ",
        session_date,
        ": ",
        paste(missing_indicators, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    indicator_rows <- indicator_rows[
      match(required_indicators, indicator_rows$canonical_indicator),
      ,
      drop = FALSE
    ]
    if (length(unique(indicator_rows$source_sha256)) != 1L ||
        length(unique(indicator_rows$source_file)) != 1L ||
        length(unique(as.numeric(indicator_rows$available_at))) != 1L) {
      stop(
        "BIT terminal indicators must come from one versioned source file.",
        call. = FALSE
      )
    }
    final_status <- identical(
      settlement_row$settlement_status[[1L]],
      "F"
    )
    previous_final_status <- identical(
      settlement_row$previous_settlement_status[[1L]],
      "F"
    )
    if (isTRUE(strict) && (!final_status || !previous_final_status)) {
      stop(
        "BIT terminal settlement and previous settlement statuses must both ",
        "be final (F).",
        call. = FALSE
      )
    }
    nqbtcs <- indicator_rows$value[
      indicator_rows$canonical_indicator == "NQBTCS"
    ]
    rtdol <- indicator_rows$value[
      indicator_rows$canonical_indicator == "RTDOL-D1"
    ]
    rtbitliq <- indicator_rows$value[
      indicator_rows$canonical_indicator == "RTBITLIQ"
    ]
    calculation <- positionsizer::ps_b3_bit_final_settlement(
      q_carry = 0L,
      nasdaq_settlement_usd = nqbtcs,
      fx_brl_per_usd = rtdol,
      previous_settlement_brl =
        settlement_row$previous_official_settlement_brl,
      contract_size_btc = life$contract_multiplier[[i]],
      official_settlement_brl = if (isTRUE(strict)) {
        settlement_row$official_settlement_brl
      } else {
        NULL
      }
    )
    tolerance <- 128 * .Machine$double.eps * max(
      1,
      abs(calculation$rounded_formula_price_brl),
      abs(settlement_row$official_settlement_brl),
      abs(rtbitliq)
    )
    formula_reconciled <- abs(
      settlement_row$official_settlement_brl -
        calculation$rounded_formula_price_brl
    ) <= tolerance
    direct_brl_reconciled <- abs(
      settlement_row$official_settlement_brl - rtbitliq
    ) <= tolerance
    if (isTRUE(strict) &&
        (!formula_reconciled || !direct_brl_reconciled)) {
      stop(
        "BIT terminal AdjstdQt must equal RTBITLIQ and the NQBTCS times ",
        "RTDOL-D1 formula rounded half-up to two decimals.",
        call. = FALSE
      )
    }
    indicator_available_at <- indicator_rows$available_at[[1L]]
    calendar_provenance <- list(
      available_at = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
      calendar_schema_id = NA_character_,
      calendar_schema_version = NA_integer_,
      calendar_scope = NA_character_,
      calendar_id = NA_character_,
      calendar_kind = NA_character_,
      source_reference = NA_character_,
      source_document_file = NA_character_,
      source_document_sha256 = NA_character_,
      normalized_file = NA_character_,
      normalized_sha256 = NA_character_,
      normalization_method = NA_character_,
      normalization_version = NA_character_,
      reviewer = NA_character_,
      reviewed_at = as.POSIXct(
        NA_real_,
        origin = "1970-01-01",
        tz = "UTC"
      ),
      review_attestation = NA_character_,
      calendar_fingerprint = NA_character_
    )
    if (!is.null(validated_calendar)) {
      calendar_provenance <- .brf_b3_calendar_resolve_posting(
        validated_calendar,
        session_date
      )
      posting_date[[i]] <- calendar_provenance$posting_date
    }
    terminal_available_at <- max(c(
      life$available_at[[i]],
      settlement_row$available_at[[1L]],
      indicator_available_at,
      calendar_provenance$available_at
    ), na.rm = TRUE)
    row <- data.frame(
      contract = contract,
      root = "BIT",
      source_schema_id = .brf_b3_bit_source_schema_id(),
      source_schema_version = .brf_b3_bit_source_schema_version(),
      session_date = as.Date(session_date),
      last_trade_date = life$last_trade_date[[i]],
      expiry_date = life$expiry_date[[i]],
      available_at = terminal_available_at,
      official_settlement_brl =
        settlement_row$official_settlement_brl,
      settlement_status = settlement_row$settlement_status,
      previous_official_settlement_brl =
        settlement_row$previous_official_settlement_brl,
      previous_settlement_status =
        settlement_row$previous_settlement_status,
      contract_size_btc = life$contract_multiplier[[i]],
      nqbtcs_usd = nqbtcs,
      rtdol_d1 = rtdol,
      rtbitliq_brl = rtbitliq,
      raw_formula_price_brl = calculation$raw_formula_price_brl,
      calculated_settlement_brl =
        calculation$rounded_formula_price_brl,
      formula_reconciled = formula_reconciled,
      direct_brl_reconciled = direct_brl_reconciled,
      pnl_formula_id = calculation$pnl_formula_id,
      rounding_rule = calculation$rounding_rule,
      cash_available_business_day_lag =
        calculation$cash_available_business_day_lag,
      cash_posting_date = posting_date[[i]],
      cash_posting_date_status = posting_status[[i]],
      cash_posting_calendar_schema_id =
        calendar_provenance$calendar_schema_id,
      cash_posting_calendar_schema_version =
        calendar_provenance$calendar_schema_version,
      cash_posting_calendar_scope =
        calendar_provenance$calendar_scope,
      cash_posting_calendar_id = calendar_provenance$calendar_id,
      cash_posting_calendar_kind = calendar_provenance$calendar_kind,
      cash_posting_calendar_available_at =
        calendar_provenance$available_at,
      cash_posting_calendar_source_reference =
        calendar_provenance$source_reference,
      cash_posting_calendar_source_document_file =
        calendar_provenance$source_document_file,
      cash_posting_calendar_source_document_sha256 =
        calendar_provenance$source_document_sha256,
      cash_posting_calendar_normalized_file =
        calendar_provenance$normalized_file,
      cash_posting_calendar_normalized_sha256 =
        calendar_provenance$normalized_sha256,
      cash_posting_calendar_normalization_method =
        calendar_provenance$normalization_method,
      cash_posting_calendar_normalization_version =
        calendar_provenance$normalization_version,
      cash_posting_calendar_reviewer =
        calendar_provenance$reviewer,
      cash_posting_calendar_reviewed_at =
        calendar_provenance$reviewed_at,
      cash_posting_calendar_review_attestation =
        calendar_provenance$review_attestation,
      cash_posting_calendar_fingerprint =
        calendar_provenance$calendar_fingerprint,
      lifecycle_available_at = life$available_at[[i]],
      lifecycle_source_file = life$source_file[[i]],
      lifecycle_source_sha256 = life$source_sha256[[i]],
      settlement_available_at =
        settlement_row$settlement_available_at[[1L]],
      settlement_source_file = settlement_row$source_file[[1L]],
      settlement_source_sha256 = settlement_row$source_sha256[[1L]],
      indicator_available_at = indicator_available_at,
      indicator_source_file = indicator_rows$source_file[[1L]],
      indicator_source_sha256 = indicator_rows$source_sha256[[1L]],
      terminal_fingerprint = NA_character_,
      execution_supported = FALSE,
      usage = "source_validation_only",
      schema_version = .brf_b3_bit_source_schema_version(),
      stringsAsFactors = FALSE
    )
    row$terminal_fingerprint <- .brf_b3_terminal_fingerprint(row)
    rows[[i]] <- row
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$session_date, out$contract), , drop = FALSE]
  rownames(out) <- NULL
  out
}
