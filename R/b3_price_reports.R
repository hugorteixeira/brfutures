.brf_b3_price_cache_schema_version <- function() 1L

.brf_b3_price_parser_version <- function() 2L

.brf_b3_price_report_kind <- function(report) {
  report <- match.arg(report, c("full", "simplified"))
  if (identical(report, "full")) "price_full" else "price_simplified"
}

.brf_b3_price_expected_report <- function(report) {
  report <- match.arg(report, c("full", "simplified"))
  if (identical(report, "full")) "^BVBG\\.086(?:\\.|$)" else "^BVBG\\.187(?:\\.|$)"
}

.brf_b3_price_empty <- function() {
  data.frame(
    date = as.Date(character()),
    contract_code = character(),
    root = character(),
    available_at = as.POSIXct(character(), tz = "UTC"),
    settlement_available_at = as.POSIXct(character(), tz = "UTC"),
    source_report_type = character(),
    source_group_id = character(),
    source_group_created_at = as.POSIXct(character(), tz = "UTC"),
    source_message_id = character(),
    source_instrument_id = character(),
    source_file = character(),
    source_sha256 = character(),
    source_archive_file = character(),
    source_archive_sha256 = character(),
    source_url = character(),
    source_parser = character(),
    market_data_stream_id = character(),
    trade_quantity = numeric(),
    open_interest = numeric(),
    contracts_traded = numeric(),
    contracts_regular = numeric(),
    contracts_nonregular = numeric(),
    trade_count = numeric(),
    nonregular_trade_count = numeric(),
    volume = numeric(),
    volume_regular = numeric(),
    volume_nonregular = numeric(),
    international_volume = numeric(),
    international_volume_regular = numeric(),
    international_volume_nonregular = numeric(),
    open = numeric(),
    low = numeric(),
    high = numeric(),
    average_price = numeric(),
    close = numeric(),
    upper_trading_limit = numeric(),
    lower_trading_limit = numeric(),
    equivalent_value = numeric(),
    settlement_price = numeric(),
    settlement_rate = numeric(),
    settlement_status = character(),
    previous_settlement = numeric(),
    previous_settlement_rate = numeric(),
    previous_settlement_status = character(),
    change_percent = numeric(),
    change_points = numeric(),
    last_bid = numeric(),
    last_ask = numeric(),
    adjustment_value = numeric(),
    stringsAsFactors = FALSE
  )
}

.brf_b3_price_is_future_code <- function(code) {
  grepl(
    "^[A-Z0-9]{2,}[FGHJKMNQUVXZ][0-9]{2}$",
    toupper(trimws(as.character(code))),
    perl = TRUE
  )
}

.brf_b3_price_contract_root <- function(code) {
  sub(
    "[FGHJKMNQUVXZ][0-9]{2}$",
    "",
    toupper(trimws(as.character(code))),
    perl = TRUE
  )
}

.brf_b3_xml_tag_block <- function(text, tag) {
  namespace <- "(?:[[:alnum:]_.-]+:)?"
  pattern <- paste0(
    "(?s)<", namespace, tag, "(?:\\s[^>]*)?>(.*?)</",
    namespace, tag, "\\s*>"
  )
  matched <- regexec(pattern, text, perl = TRUE)
  pieces <- regmatches(text, matched)[[1L]]
  if (length(pieces) < 2L) NA_character_ else pieces[[2L]]
}

.brf_b3_price_attribute_names <- function(text) {
  if (is.na(text) || !nzchar(text)) {
    return(character())
  }
  hits <- regmatches(
    text,
    gregexpr(
      "<(?:[[:alnum:]_.-]+:)?[[:alnum:]_.-]+(?:\\s[^>]*)?>",
      text,
      perl = TRUE
    )
  )[[1L]]
  if (!length(hits) || identical(hits, character(0))) {
    return(character())
  }
  unique(sub(
    "^<(?:[[:alnum:]_.-]+:)?([[:alnum:]_.-]+).*$",
    "\\1",
    hits,
    perl = TRUE
  ))
}

.brf_b3_price_group_row_lines <- function(lines, contract) {
  trimmed <- trimws(lines)
  leaf_pattern <- paste0(
    "^<(?:[[:alnum:]_.-]+:)?[[:alnum:]_.-]+(?:\\s[^>]*)?>",
    "[^<]*</(?:[[:alnum:]_.-]+:)?[[:alnum:]_.-]+\\s*>$"
  )
  leaf_positions <- grep(leaf_pattern, trimmed, perl = TRUE)
  if (!length(leaf_positions)) {
    return(NULL)
  }
  leaf <- trimmed[leaf_positions]
  tags <- sub(
    "^<(?:[[:alnum:]_.-]+:)?([[:alnum:]_.-]+)(?:\\s[^>]*)?>.*$",
    "\\1",
    leaf,
    perl = TRUE
  )
  values <- sub(
    paste0(
      "^<(?:[[:alnum:]_.-]+:)?[[:alnum:]_.-]+(?:\\s[^>]*)?>\\s*",
      "([^<]*?)\\s*</(?:[[:alnum:]_.-]+:)?[[:alnum:]_.-]+\\s*>$"
    ),
    "\\1",
    leaf,
    perl = TRUE
  )
  scope <- function(tag) {
    opening <- grep(
      paste0("^<(?:[[:alnum:]_.-]+:)?", tag, "(?:\\s[^>]*)?>$"),
      trimmed,
      perl = TRUE
    )
    closing <- grep(
      paste0("^</(?:[[:alnum:]_.-]+:)?", tag, "\\s*>$"),
      trimmed,
      perl = TRUE
    )
    if (!length(opening) || !length(closing)) {
      return(integer())
    }
    start <- opening[[1L]]
    later <- closing[closing > start]
    if (!length(later)) {
      return(integer())
    }
    end <- later[[1L]]
    which(
      leaf_positions > start & leaf_positions < end
    )
  }
  first_value <- function(tag, indices = seq_along(tags)) {
    hit <- indices[tags[indices] == tag]
    if (!length(hit)) NA_character_ else trimws(values[[hit[[1L]]]])
  }
  attribute_indices <- scope("FinInstrmAttrbts")
  instrument_indices <- scope("FinInstrmId")
  trade_date_indices <- scope("TradDt")
  value <- function(tag) first_value(tag, attribute_indices)
  list(
    row = list(
      date = first_value("Dt", trade_date_indices),
      contract_code = contract,
      root = .brf_b3_price_contract_root(contract),
      available_at = first_value("CreDt"),
      source_message_id = first_value("BizMsgIdr"),
      source_instrument_id = first_value("Id", instrument_indices),
      market_data_stream_id = value("MktDataStrmId"),
      trade_quantity = first_value("TradQty"),
      open_interest = value("OpnIntrst"),
      contracts_traded = value("FinInstrmQty"),
      contracts_regular = value("RglrTraddCtrcts"),
      contracts_nonregular = value("NonRglrTraddCtrcts"),
      trade_count = value("RglrTxsQty"),
      nonregular_trade_count = value("NonRglrTxsQty"),
      volume = value("NtlFinVol"),
      volume_regular = value("NtlRglrVol"),
      volume_nonregular = value("NtlNonRglrVol"),
      international_volume = value("IntlFinVol"),
      international_volume_regular = value("IntlRglrVol"),
      international_volume_nonregular = value("IntlNonRglrVol"),
      open = value("FrstPric"),
      low = value("MinPric"),
      high = value("MaxPric"),
      average_price = value("TradAvrgPric"),
      close = value("LastPric"),
      upper_trading_limit = value("MaxTradLmt"),
      lower_trading_limit = value("MinTradLmt"),
      equivalent_value = value("EqvtVal"),
      settlement_price = value("AdjstdQt"),
      settlement_rate = value("AdjstdQtTax"),
      settlement_status = value("AdjstdQtStin"),
      previous_settlement = value("PrvsAdjstdQt"),
      previous_settlement_rate = value("PrvsAdjstdQtTax"),
      previous_settlement_status = value("PrvsAdjstdQtStin"),
      change_percent = value("OscnPctg"),
      change_points = value("VartnPts"),
      last_bid = value("BestBidPric"),
      last_ask = value("BestAskPric"),
      adjustment_value = value("AdjstdValCtrct")
    ),
    attribute_names = unique(tags[attribute_indices])
  )
}

.brf_b3_price_group_row <- function(block, contract = NULL) {
  if (length(block) > 1L && !is.null(contract)) {
    parsed <- .brf_b3_price_group_row_lines(block, contract)
    if (!is.null(parsed) &&
        !is.na(parsed$row$date) &&
        !is.na(parsed$row$source_instrument_id)) {
      return(parsed)
    }
  }
  block <- paste(block, collapse = "\n")
  if (!grepl("<(?:[[:alnum:]_.-]+:)?PricRpt(?:\\s[^>]*)?>", block, perl = TRUE)) {
    return(NULL)
  }
  if (is.null(contract)) {
    contract <- toupper(.brf_b3_xml_tag_value(block, "TckrSymb"))
  }
  if (is.na(contract) || !.brf_b3_price_is_future_code(contract)) {
    return(NULL)
  }
  instrument <- .brf_b3_xml_tag_block(block, "FinInstrmId")
  attributes <- .brf_b3_xml_tag_block(block, "FinInstrmAttrbts")
  trade_details <- .brf_b3_xml_tag_block(block, "TradDtls")
  value <- function(tag) .brf_b3_xml_tag_value(attributes, tag)
  list(
    row = list(
      date = .brf_b3_xml_tag_value(
        .brf_b3_xml_tag_block(block, "TradDt"),
        "Dt"
      ),
      contract_code = contract,
      root = .brf_b3_price_contract_root(contract),
      available_at = .brf_b3_xml_tag_value(block, "CreDt"),
      source_message_id = .brf_b3_xml_tag_value(block, "BizMsgIdr"),
      source_instrument_id = .brf_b3_xml_tag_value(instrument, "Id"),
      market_data_stream_id = value("MktDataStrmId"),
      trade_quantity = .brf_b3_xml_tag_value(trade_details, "TradQty"),
      open_interest = value("OpnIntrst"),
      contracts_traded = value("FinInstrmQty"),
      contracts_regular = value("RglrTraddCtrcts"),
      contracts_nonregular = value("NonRglrTraddCtrcts"),
      trade_count = value("RglrTxsQty"),
      nonregular_trade_count = value("NonRglrTxsQty"),
      volume = value("NtlFinVol"),
      volume_regular = value("NtlRglrVol"),
      volume_nonregular = value("NtlNonRglrVol"),
      international_volume = value("IntlFinVol"),
      international_volume_regular = value("IntlRglrVol"),
      international_volume_nonregular = value("IntlNonRglrVol"),
      open = value("FrstPric"),
      low = value("MinPric"),
      high = value("MaxPric"),
      average_price = value("TradAvrgPric"),
      close = value("LastPric"),
      upper_trading_limit = value("MaxTradLmt"),
      lower_trading_limit = value("MinTradLmt"),
      equivalent_value = value("EqvtVal"),
      settlement_price = value("AdjstdQt"),
      settlement_rate = value("AdjstdQtTax"),
      settlement_status = value("AdjstdQtStin"),
      previous_settlement = value("PrvsAdjstdQt"),
      previous_settlement_rate = value("PrvsAdjstdQtTax"),
      previous_settlement_status = value("PrvsAdjstdQtStin"),
      change_percent = value("OscnPctg"),
      change_points = value("VartnPts"),
      last_bid = value("BestBidPric"),
      last_ask = value("BestAskPric"),
      adjustment_value = value("AdjstdValCtrct")
    ),
    attribute_names = .brf_b3_price_attribute_names(attributes)
  )
}

.brf_b3_price_rows_frame <- function(rows) {
  if (!length(rows)) {
    return(.brf_b3_price_empty()[, c(
      "date", "contract_code", "root", "available_at",
      "settlement_available_at",
      "source_message_id", "source_instrument_id",
      "market_data_stream_id", "trade_quantity", "open_interest",
      "contracts_traded", "contracts_regular", "contracts_nonregular",
      "trade_count", "nonregular_trade_count", "volume", "volume_regular",
      "volume_nonregular", "international_volume",
      "international_volume_regular", "international_volume_nonregular",
      "open", "low", "high", "average_price", "close",
      "upper_trading_limit", "lower_trading_limit", "equivalent_value",
      "settlement_price", "settlement_rate", "settlement_status",
      "previous_settlement", "previous_settlement_rate",
      "previous_settlement_status",
      "change_percent", "change_points", "last_bid", "last_ask",
      "adjustment_value"
    ), drop = FALSE])
  }
  fields <- names(rows[[1L]])
  out <- lapply(fields, function(field) {
    vapply(rows, function(row) row[[field]] %||% NA_character_, character(1L))
  })
  names(out) <- fields
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out$date <- as.Date(out$date)
  out$available_at <- .brf_b3_parse_timestamp(out$available_at)
  out$settlement_available_at <- out$available_at
  numeric_fields <- c(
    "trade_quantity", "open_interest", "contracts_traded",
    "contracts_regular", "contracts_nonregular", "trade_count",
    "nonregular_trade_count", "volume", "volume_regular",
    "volume_nonregular", "international_volume",
    "international_volume_regular", "international_volume_nonregular",
    "open", "low", "high", "average_price", "close",
    "upper_trading_limit", "lower_trading_limit", "equivalent_value",
    "settlement_price", "settlement_rate", "previous_settlement",
    "previous_settlement_rate", "change_percent", "change_points",
    "last_bid", "last_ask", "adjustment_value"
  )
  out[numeric_fields] <- lapply(out[numeric_fields], .brf_bvbg_xml_number)
  out$settlement_available_at[is.na(out$settlement_price)] <- as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = "UTC"
  )
  out
}

.brf_b3_price_parse <- function(path,
                                report,
                                source_file,
                                source_sha256,
                                source_archive_file,
                                source_archive_sha256,
                                source_url,
                                quiet = FALSE) {
  if (!file.exists(path)) {
    stop("File '", path, "' not found.", call. = FALSE)
  }
  report <- match.arg(report, c("full", "simplified"))
  connection <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt", encoding = "UTF-8")
  } else {
    file(path, open = "rt", encoding = "UTF-8")
  }
  on.exit(close(connection), add = TRUE)
  group_open <- "<(?:[[:alnum:]_.-]+:)?BizGrp(?:\\s[^>]*)?>"
  group_close <- "</(?:[[:alnum:]_.-]+:)?BizGrp\\s*>"
  ticker_pattern <- paste0(
    ".*<(?:[[:alnum:]_.-]+:)?TckrSymb(?:\\s[^>]*)?>\\s*",
    "([^<]+?)\\s*</(?:[[:alnum:]_.-]+:)?TckrSymb\\s*>.*"
  )
  pending <- character()
  header <- character()
  header_complete <- FALSE
  tail_lines <- character()
  rows <- list()
  attribute_names <- character()
  groups_seen <- 0L

  repeat {
    lines <- readLines(
      connection,
      n = 100000L,
      warn = FALSE,
      encoding = "UTF-8"
    )
    if (!length(lines)) {
      break
    }
    tail_lines <- utils::tail(c(tail_lines, lines), 20L)
    combined <- c(pending, lines)
    starts <- grep(group_open, combined, perl = TRUE)
    ends <- grep(group_close, combined, perl = TRUE)
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

    last_complete_end <- 0L
    unmatched_start <- NA_integer_
    if (length(starts) && length(ends)) {
      complete_starts <- starts[vapply(
        starts,
        function(start) any(ends >= start),
        logical(1L)
      )]
      groups_seen <- groups_seen + length(complete_starts)
      if (length(complete_starts)) {
        last_complete_end <- max(ends[
          ends >= utils::tail(complete_starts, 1L)
        ])
      }
      incomplete <- starts[!vapply(
        starts,
        function(start) any(ends >= start),
        logical(1L)
      )]
      if (length(incomplete)) {
        unmatched_start <- incomplete[[1L]]
      }
      ticker_hits <- grep(ticker_pattern, combined, perl = TRUE)
      if (length(ticker_hits)) {
        contracts <- toupper(sub(
          ticker_pattern,
          "\\1",
          combined[ticker_hits],
          perl = TRUE
        ))
        keep <- .brf_b3_price_is_future_code(contracts)
        ticker_hits <- ticker_hits[keep]
        contracts <- contracts[keep]
        block_keys <- character()
        for (ticker_index in seq_along(ticker_hits)) {
          hit <- ticker_hits[[ticker_index]]
          start_candidates <- starts[starts <= hit]
          end_candidates <- ends[ends >= hit]
          if (!length(start_candidates) || !length(end_candidates)) {
            next
          }
          start <- utils::tail(start_candidates, 1L)
          end <- end_candidates[[1L]]
          key <- paste(start, end, sep = ":")
          if (key %in% block_keys) {
            next
          }
          parsed <- .brf_b3_price_group_row(
            combined[start:end],
            contract = contracts[[ticker_index]]
          )
          if (!is.null(parsed)) {
            rows[[length(rows) + 1L]] <- parsed$row
            attribute_names <- union(
              attribute_names,
              parsed$attribute_names
            )
          }
          block_keys <- c(block_keys, key)
        }
      }
    } else if (length(starts)) {
      unmatched_start <- starts[[1L]]
    }
    if (!is.na(unmatched_start)) {
      pending <- combined[unmatched_start:length(combined)]
    } else {
      later_starts <- starts[starts > last_complete_end]
      pending <- if (length(later_starts)) {
        combined[later_starts[[1L]]:length(combined)]
      } else {
        character()
      }
    }
    if (!quiet && groups_seen > 0L && groups_seen %% 20000L == 0L) {
      message("B3 price report: scanned ", groups_seen, " groups.")
    }
  }
  if (length(pending)) {
    stop("Truncated B3 price-report BizGrp at end of file.", call. = FALSE)
  }
  final_non_empty <- trimws(tail_lines[nzchar(trimws(tail_lines))])
  final_line <- if (length(final_non_empty)) {
    utils::tail(final_non_empty, 1L)
  } else {
    ""
  }
  if (!header_complete || !grepl(
    "</(?:[[:alnum:]_.-]+:)?Document\\s*>\\s*$",
    final_line,
    perl = TRUE
  )) {
    stop("Malformed or truncated B3 price-report XML.", call. = FALSE)
  }
  header_text <- paste(header, collapse = "\n")
  source_report_type <- .brf_b3_xml_tag_value(header_text, "BizGrpTp")
  if (is.na(source_report_type) || !grepl(
    .brf_b3_price_expected_report(report),
    source_report_type
  )) {
    stop(
      "Unexpected B3 price-report type '",
      source_report_type,
      "' for report='",
      report,
      "'.",
      call. = FALSE
    )
  }
  out <- .brf_b3_price_rows_frame(rows)
  if (nrow(out)) {
    out$source_report_type <- source_report_type
    out$source_group_id <- .brf_b3_xml_tag_value(header_text, "BizGrpIdr")
    out$source_group_created_at <- .brf_b3_parse_timestamp(
      .brf_b3_xml_tag_value(header_text, "CreDtAndTm")
    )
    out$source_file <- source_file
    out$source_sha256 <- source_sha256
    out$source_archive_file <- source_archive_file
    out$source_archive_sha256 <- source_archive_sha256
    out$source_url <- source_url
    out$source_parser <- paste0(
      "bounded_bizgrp_price_stream_v",
      .brf_b3_price_parser_version()
    )
    canonical <- names(.brf_b3_price_empty())
    out <- out[, canonical, drop = FALSE]
    invalid <- is.na(out$date) | is.na(out$contract_code) |
      !nzchar(out$contract_code) | is.na(out$source_instrument_id) |
      !nzchar(out$source_instrument_id)
    if (any(invalid)) {
      stop(
        "B3 price report contains futures rows without a complete key.",
        call. = FALSE
      )
    }
    key <- paste(
      out$date,
      out$contract_code,
      out$source_instrument_id,
      sep = "|"
    )
    if (anyDuplicated(key)) {
      duplicate <- unique(key[duplicated(key) | duplicated(key, fromLast = TRUE)])
      stop(
        "B3 price report contains duplicate futures key(s): ",
        paste(utils::head(duplicate, 5L), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    out <- out[order(out$date, out$root, out$contract_code), , drop = FALSE]
    rownames(out) <- NULL
  }
  attr(out, "brf_price_field_inventory") <- sort(attribute_names)
  attr(out, "brf_price_groups_scanned") <- groups_seen
  attr(out, "brf_price_parser_version") <- .brf_b3_price_parser_version()
  out
}

.brf_b3_price_atomic_gzip <- function(source, target) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(target)) {
    return(invisible(target))
  }
  staged <- tempfile(".partial-", tmpdir = dirname(target), fileext = ".gz")
  input <- file(source, open = "rb")
  output <- gzfile(staged, open = "wb", compression = 9L)
  completed <- FALSE
  on.exit({
    try(close(input), silent = TRUE)
    try(close(output), silent = TRUE)
    if (!completed && file.exists(staged)) {
      unlink(staged)
    }
  }, add = TRUE)
  repeat {
    bytes <- readBin(input, what = "raw", n = 1024L * 1024L)
    if (!length(bytes)) {
      break
    }
    writeBin(bytes, output)
  }
  close(input)
  close(output)
  if (!file.rename(staged, target)) {
    if (file.exists(target)) {
      unlink(staged)
    } else {
      stop("Unable to atomically publish compressed B3 snapshot.", call. = FALSE)
    }
  }
  completed <- TRUE
  invisible(target)
}

.brf_b3_price_store_parsed <- function(data, day_dir) {
  parsed_dir <- file.path(day_dir, "parsed")
  dir.create(parsed_dir, recursive = TRUE, showWarnings = FALSE)
  staged <- tempfile(".partial-", tmpdir = parsed_dir, fileext = ".rds")
  saveRDS(data, staged, compress = "xz")
  sha256 <- .brf_b3_source_file_sha256(staged)
  target <- file.path(parsed_dir, paste0(sha256, ".rds"))
  if (file.exists(target)) {
    unlink(staged)
  } else if (!file.rename(staged, target)) {
    unlink(staged)
    stop("Unable to atomically publish parsed B3 price cache.", call. = FALSE)
  }
  list(
    path = target,
    relative_path = file.path("parsed", basename(target)),
    sha256 = sha256
  )
}

.brf_b3_price_manifest_fingerprint <- function(manifest) {
  fields <- c(
    "schema_version", "parser_version", "report", "report_date",
    "source_report_type", "source_archive_file", "source_archive_sha256",
    "source_url", "selected_snapshot_file", "selected_snapshot_created_at",
    "snapshot_raw_sha256", "snapshot_compressed_path",
    "snapshot_compressed_sha256", "parsed_path", "parsed_sha256",
    "parsed_rows", "parsed_trade_dates", "field_inventory"
  )
  missing <- setdiff(fields, names(manifest))
  if (length(missing)) {
    stop(
      "B3 price manifest is missing fingerprint field(s): ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  value <- manifest[fields]
  value$report_date <- format(as.Date(value$report_date), "%Y-%m-%d")
  value$selected_snapshot_created_at <- format(
    .brf_b3_parse_timestamp(value$selected_snapshot_created_at),
    "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  )
  value$parsed_trade_dates <- format(
    as.Date(value$parsed_trade_dates),
    "%Y-%m-%d"
  )
  digest::digest(value, algo = "sha256", serialize = TRUE)
}

.brf_b3_price_manifest_read <- function(day_dir) {
  path <- file.path(day_dir, "manifest.rds")
  if (!file.exists(path)) {
    return(NULL)
  }
  value <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.list(value)) value else NULL
}

.brf_b3_price_manifest_status <- function(manifest, day_dir, report, date) {
  result <- list(snapshot_valid = FALSE, parsed_valid = FALSE)
  if (!is.list(manifest)) {
    return(result)
  }
  required <- c(
    "schema_version", "parser_version", "report", "report_date",
    "source_report_type", "snapshot_raw_sha256",
    "snapshot_compressed_path", "snapshot_compressed_sha256",
    "parsed_path", "parsed_sha256", "parsed_trade_dates",
    "manifest_fingerprint"
  )
  if (length(setdiff(required, names(manifest))) ||
      !identical(manifest$schema_version, .brf_b3_price_cache_schema_version()) ||
      !identical(manifest$report, report) ||
      !identical(as.Date(manifest$report_date), as.Date(date)) ||
      !grepl(.brf_b3_price_expected_report(report), manifest$source_report_type)) {
    return(result)
  }
  fingerprint <- tryCatch(
    .brf_b3_price_manifest_fingerprint(manifest),
    error = function(e) NA_character_
  )
  if (is.na(fingerprint) || !identical(fingerprint, manifest$manifest_fingerprint)) {
    return(result)
  }
  if (!as.Date(date) %in% as.Date(manifest$parsed_trade_dates)) {
    return(result)
  }
  snapshot <- file.path(day_dir, manifest$snapshot_compressed_path)
  result$snapshot_path <- snapshot
  result$snapshot_valid <- file.exists(snapshot) && identical(
    .brf_b3_source_file_sha256(snapshot),
    manifest$snapshot_compressed_sha256
  )
  if (!result$snapshot_valid) {
    return(result)
  }
  parsed <- file.path(day_dir, manifest$parsed_path)
  result$parsed_path <- parsed
  result$parsed_valid <- identical(
    manifest$parser_version,
    .brf_b3_price_parser_version()
  ) && file.exists(parsed) && identical(
    .brf_b3_source_file_sha256(parsed),
    manifest$parsed_sha256
  )
  result
}

.brf_b3_price_manifest_publish <- function(manifest, day_dir) {
  manifest$manifest_fingerprint <- .brf_b3_price_manifest_fingerprint(manifest)
  .brf_b3_atomic_save_rds(manifest, file.path(day_dir, "manifest.rds"))
  snapshot_keep <- normalizePath(
    file.path(day_dir, manifest$snapshot_compressed_path),
    mustWork = TRUE
  )
  parsed_keep <- normalizePath(
    file.path(day_dir, manifest$parsed_path),
    mustWork = TRUE
  )
  snapshot_files <- list.files(
    file.path(day_dir, "snapshot"),
    pattern = "\\.xml\\.gz$",
    full.names = TRUE
  )
  parsed_files <- list.files(
    file.path(day_dir, "parsed"),
    pattern = "\\.rds$",
    full.names = TRUE
  )
  stale <- c(
    setdiff(normalizePath(snapshot_files, mustWork = FALSE), snapshot_keep),
    setdiff(normalizePath(parsed_files, mustWork = FALSE), parsed_keep)
  )
  stale <- stale[file.exists(stale)]
  if (length(stale)) {
    unlink(stale)
  }
  manifest
}

.brf_b3_price_parse_from_manifest <- function(manifest,
                                               day_dir,
                                               quiet = FALSE) {
  snapshot <- file.path(day_dir, manifest$snapshot_compressed_path)
  data <- .brf_b3_price_parse(
    snapshot,
    report = manifest$report,
    source_file = manifest$selected_snapshot_file,
    source_sha256 = manifest$snapshot_raw_sha256,
    source_archive_file = manifest$source_archive_file,
    source_archive_sha256 = manifest$source_archive_sha256,
    source_url = manifest$source_url,
    quiet = quiet
  )
  stored <- .brf_b3_price_store_parsed(data, day_dir)
  manifest$parser_version <- .brf_b3_price_parser_version()
  manifest$parsed_path <- stored$relative_path
  manifest$parsed_sha256 <- stored$sha256
  manifest$parsed_rows <- nrow(data)
  manifest$parsed_trade_dates <- sort(unique(as.Date(data$date)))
  manifest$field_inventory <- attr(
    data,
    "brf_price_field_inventory",
    exact = TRUE
  ) %||% character()
  manifest$parsed_at <- Sys.time()
  manifest <- .brf_b3_price_manifest_publish(manifest, day_dir)
  list(data = data, manifest = manifest)
}

.brf_b3_price_download_and_build <- function(date,
                                              report,
                                              cache_dir,
                                              day_dir,
                                              quiet = FALSE) {
  work_dir <- tempfile("price-fetch-", tmpdir = cache_dir)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    if (dir.exists(work_dir) && startsWith(
      normalizePath(work_dir, mustWork = FALSE),
      normalizePath(cache_dir, mustWork = TRUE)
    )) {
      unlink(work_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  kind <- .brf_b3_price_report_kind(report)
  archive <- .brf_b3_download_daily_archive(
    date,
    kind,
    work_dir,
    quiet = quiet
  )
  payload <- .brf_b3_find_nested_payload(archive$path, kind, work_dir)
  raw_sha256 <- .brf_b3_source_file_sha256(payload$path)
  snapshot_dir <- file.path(day_dir, "snapshot")
  snapshot <- file.path(snapshot_dir, paste0(raw_sha256, ".xml.gz"))
  .brf_b3_price_atomic_gzip(payload$path, snapshot)
  compressed_sha256 <- .brf_b3_source_file_sha256(snapshot)
  data <- .brf_b3_price_parse(
    payload$path,
    report = report,
    source_file = payload$source_file,
    source_sha256 = raw_sha256,
    source_archive_file = archive$source_file,
    source_archive_sha256 = archive$source_archive_sha256,
    source_url = archive$source_url,
    quiet = quiet
  )
  stored <- .brf_b3_price_store_parsed(data, day_dir)
  manifest <- list(
    schema_version = .brf_b3_price_cache_schema_version(),
    parser_version = .brf_b3_price_parser_version(),
    report = report,
    report_date = as.Date(date),
    source_report_type = payload$report_type,
    source_archive_file = archive$source_file,
    source_archive_sha256 = archive$source_archive_sha256,
    source_url = archive$source_url,
    snapshots = payload$snapshots,
    snapshot_count = nrow(payload$snapshots),
    selected_snapshot_file = payload$source_file,
    selected_snapshot_created_at = payload$created_at,
    snapshot_raw_bytes = as.numeric(file.info(payload$path)$size),
    snapshot_raw_sha256 = raw_sha256,
    snapshot_compressed_path = file.path("snapshot", basename(snapshot)),
    snapshot_compressed_bytes = as.numeric(file.info(snapshot)$size),
    snapshot_compressed_sha256 = compressed_sha256,
    parsed_path = stored$relative_path,
    parsed_sha256 = stored$sha256,
    parsed_rows = nrow(data),
    parsed_trade_dates = sort(unique(as.Date(data$date))),
    field_inventory = attr(
      data,
      "brf_price_field_inventory",
      exact = TRUE
    ) %||% character(),
    parsed_at = Sys.time()
  )
  manifest <- .brf_b3_price_manifest_publish(manifest, day_dir)
  list(data = data, manifest = manifest)
}

.brf_b3_price_legacy_simplified_path <- function(date, cache_dir) {
  path <- file.path(
    dirname(cache_dir),
    "BVBG",
    format(as.Date(date), "%Y"),
    paste0(format(as.Date(date), "%Y-%m-%d"), "-raw.xml")
  )
  if (file.exists(path)) path else NA_character_
}

.brf_b3_price_import_legacy_simplified <- function(date,
                                                    path,
                                                    day_dir,
                                                    quiet = FALSE) {
  header <- paste(
    readLines(path, n = 200L, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  report_type <- .brf_b3_xml_tag_value(header, "BizGrpTp")
  if (is.na(report_type) || !grepl("^BVBG\\.187(?:\\.|$)", report_type)) {
    stop(
      "Legacy simplified price cache is not a BVBG.187 XML: ",
      path,
      ".",
      call. = FALSE
    )
  }
  raw_sha256 <- .brf_b3_source_file_sha256(path)
  snapshot_dir <- file.path(day_dir, "snapshot")
  snapshot <- file.path(snapshot_dir, paste0(raw_sha256, ".xml.gz"))
  .brf_b3_price_atomic_gzip(path, snapshot)
  compressed_sha256 <- .brf_b3_source_file_sha256(snapshot)
  source_archive_file <- .brf_b3_daily_file_name(date, "price_simplified")
  source_url <- .brf_b3_daily_file_url(date, "price_simplified")
  data <- .brf_b3_price_parse(
    path,
    report = "simplified",
    source_file = basename(path),
    source_sha256 = raw_sha256,
    source_archive_file = source_archive_file,
    source_archive_sha256 = NA_character_,
    source_url = source_url,
    quiet = quiet
  )
  if (!as.Date(date) %in% as.Date(data$date)) {
    stop(
      "Legacy simplified cache for ",
      as.Date(date),
      " contains different embedded trade date(s): ",
      paste(sort(unique(as.Date(data$date))), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  selected_at <- .brf_b3_parse_timestamp(
    .brf_b3_xml_tag_value(header, "CreDt")
  )
  if (length(selected_at) != 1L || is.na(selected_at)) {
    observed_available <- data$available_at[!is.na(data$available_at)]
    selected_at <- if (length(observed_available)) {
      max(observed_available)
    } else {
      as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
    }
  }
  stored <- .brf_b3_price_store_parsed(data, day_dir)
  snapshots <- data.frame(
    source_file = basename(path),
    report_type = report_type,
    created_at = selected_at,
    uncompressed_bytes = as.numeric(file.info(path)$size),
    selected = TRUE,
    stringsAsFactors = FALSE
  )
  manifest <- list(
    schema_version = .brf_b3_price_cache_schema_version(),
    parser_version = .brf_b3_price_parser_version(),
    report = "simplified",
    report_date = as.Date(date),
    source_report_type = report_type,
    source_archive_file = source_archive_file,
    source_archive_sha256 = NA_character_,
    source_url = source_url,
    source_origin = "legacy_bvbg_raw_xml",
    snapshots = snapshots,
    snapshot_count = 1L,
    selected_snapshot_file = basename(path),
    selected_snapshot_created_at = selected_at,
    snapshot_raw_bytes = as.numeric(file.info(path)$size),
    snapshot_raw_sha256 = raw_sha256,
    snapshot_compressed_path = file.path("snapshot", basename(snapshot)),
    snapshot_compressed_bytes = as.numeric(file.info(snapshot)$size),
    snapshot_compressed_sha256 = compressed_sha256,
    parsed_path = stored$relative_path,
    parsed_sha256 = stored$sha256,
    parsed_rows = nrow(data),
    parsed_trade_dates = sort(unique(as.Date(data$date))),
    field_inventory = attr(
      data,
      "brf_price_field_inventory",
      exact = TRUE
    ) %||% character(),
    parsed_at = Sys.time()
  )
  manifest <- .brf_b3_price_manifest_publish(manifest, day_dir)
  list(data = data, manifest = manifest)
}

#' Fetch a complete or simplified official B3 daily price report
#'
#' Downloads the daily full `PRyymmdd.zip` (`BVBG.086`) or simplified
#' `SPRDyymmdd.zip` (`BVBG.187`) report. A B3 archive may contain multiple
#' complete snapshots published during the same session. All candidates are
#' audited by their embedded `AppHdr/CreDt`, but only the newest snapshot is
#' selected. The outer and nested ZIP files are temporary: the durable cache
#' retains one compressed XML snapshot, one compact futures-only RDS and a
#' hash-verified manifest.
#'
#' A valid manifest prevents another download. If the parsed RDS is missing or
#' belongs to an older parser version, it is rebuilt from the retained
#' compressed snapshot without contacting B3.
#'
#' @param date One B3 archive/report date.
#' @param report Either `"full"` for `PR/BVBG.086` or `"simplified"` for
#'   `SPRD/BVBG.187`.
#' @param root Optional futures root or vector of roots to retain.
#' @param cache_dir Optional B3 reference cache root.
#' @param refresh Force a new archive download and manifest publication.
#' @param quiet Suppress progress messages.
#' @param all_trade_dates Keep every trade date embedded in the selected
#'   snapshot. The default keeps only rows whose `TradDt` equals `date`, because
#'   a daily archive can also carry a small number of next-session rows.
#' @return A futures-only data frame. Its `brf_b3_price_manifest` attribute
#'   contains snapshot, publication and content-hash metadata.
#' @export
brf_b3_prices_fetch <- function(date,
                                report = c("full", "simplified"),
                                root = NULL,
                                cache_dir = NULL,
                                refresh = FALSE,
                                quiet = FALSE,
                                all_trade_dates = FALSE) {
  date <- .brf_normalize_date(date)
  report <- match.arg(report)
  cache_dir <- .brf_b3_reference_cache_dir(cache_dir)
  day_dir <- file.path(
    cache_dir,
    "prices",
    report,
    format(date, "%Y-%m-%d")
  )
  dir.create(day_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- .brf_b3_price_manifest_read(day_dir)
  status <- .brf_b3_price_manifest_status(manifest, day_dir, report, date)
  built <- NULL
  if (!isTRUE(refresh) && isTRUE(status$parsed_valid)) {
    data <- tryCatch(readRDS(status$parsed_path), error = function(e) NULL)
    if (!inherits(data, "data.frame") ||
        !identical(
          attr(data, "brf_price_parser_version", exact = TRUE),
          .brf_b3_price_parser_version()
        )) {
      status$parsed_valid <- FALSE
    }
  }
  if (!isTRUE(refresh) && isTRUE(status$parsed_valid)) {
    built <- list(data = data, manifest = manifest)
  } else if (!isTRUE(refresh) && isTRUE(status$snapshot_valid)) {
    built <- .brf_b3_price_parse_from_manifest(
      manifest,
      day_dir,
      quiet = quiet
    )
  } else if (!isTRUE(refresh) && identical(report, "simplified") &&
      !is.na(.brf_b3_price_legacy_simplified_path(date, cache_dir))) {
    legacy_path <- .brf_b3_price_legacy_simplified_path(date, cache_dir)
    built <- tryCatch(
      .brf_b3_price_import_legacy_simplified(
        date,
        legacy_path,
        day_dir,
        quiet = quiet
      ),
      error = function(error) {
        if (!quiet) {
          message(
            "Ignoring invalid legacy simplified cache for ",
            date,
            ": ",
            conditionMessage(error)
          )
        }
        NULL
      }
    )
    if (is.null(built)) {
      built <- .brf_b3_price_download_and_build(
        date,
        report,
        cache_dir,
        day_dir,
        quiet = quiet
      )
    }
  } else {
    built <- .brf_b3_price_download_and_build(
      date,
      report,
      cache_dir,
      day_dir,
      quiet = quiet
    )
  }
  out <- built$data
  if (!"settlement_available_at" %in% names(out)) {
    out$settlement_available_at <- out$available_at
    out$settlement_available_at[is.na(out$settlement_price)] <- as.POSIXct(
      NA_real_,
      origin = "1970-01-01",
      tz = "UTC"
    )
  }
  if (!isTRUE(all_trade_dates) && nrow(out)) {
    out <- out[as.Date(out$date) == date, , drop = FALSE]
  }
  if (!is.null(root) && nrow(out)) {
    roots <- unique(vapply(root, .brf_normalize_root, character(1L)))
    out <- out[out$root %in% roots, , drop = FALSE]
  }
  rownames(out) <- NULL
  attr(out, "brf_b3_price_manifest") <- built$manifest
  out
}

.brf_b3_price_compare_fields <- function() {
  c(
    "market_data_stream_id", "trade_quantity", "open_interest",
    "contracts_traded", "contracts_regular", "contracts_nonregular",
    "trade_count", "nonregular_trade_count", "volume", "volume_regular",
    "volume_nonregular", "international_volume",
    "international_volume_regular", "international_volume_nonregular",
    "open", "low", "high", "average_price", "close",
    "upper_trading_limit", "lower_trading_limit", "equivalent_value",
    "settlement_price", "settlement_rate", "settlement_status",
    "previous_settlement", "previous_settlement_rate",
    "previous_settlement_status", "change_percent", "change_points",
    "last_bid", "last_ask", "adjustment_value"
  )
}

.brf_b3_price_value_equal <- function(full, simplified, tolerance) {
  both_missing <- is.na(full) & is.na(simplified)
  both_present <- !is.na(full) & !is.na(simplified)
  equal <- both_missing
  if (is.numeric(full) && is.numeric(simplified)) {
    equal[both_present] <- abs(full[both_present] - simplified[both_present]) <=
      tolerance * pmax(
        1,
        abs(full[both_present]),
        abs(simplified[both_present])
      )
  } else {
    equal[both_present] <- as.character(full[both_present]) ==
      as.character(simplified[both_present])
  }
  equal
}

.brf_b3_price_compare_one <- function(date,
                                      root,
                                      cache_dir,
                                      refresh,
                                      quiet,
                                      tolerance) {
  full <- brf_b3_prices_fetch(
    date,
    report = "full",
    root = root,
    cache_dir = cache_dir,
    refresh = refresh,
    quiet = quiet
  )
  full_manifest <- attr(full, "brf_b3_price_manifest", exact = TRUE)
  simplified <- brf_b3_prices_fetch(
    date,
    report = "simplified",
    root = root,
    cache_dir = cache_dir,
    refresh = refresh,
    quiet = quiet
  )
  simplified_manifest <- attr(
    simplified,
    "brf_b3_price_manifest",
    exact = TRUE
  )
  keys <- c("date", "root", "contract_code", "source_instrument_id")
  full$key_present <- rep(TRUE, nrow(full))
  simplified$key_present <- rep(TRUE, nrow(simplified))
  fields <- .brf_b3_price_compare_fields()
  joined <- merge(
    full[, c(keys, "key_present", fields), drop = FALSE],
    simplified[, c(keys, "key_present", fields), drop = FALSE],
    by = keys,
    all = TRUE,
    suffixes = c("_full", "_simplified"),
    sort = TRUE
  )
  matched <- !is.na(joined$key_present_full) &
    !is.na(joined$key_present_simplified)
  field_rows <- vector("list", length(fields))
  differences <- list()
  for (index in seq_along(fields)) {
    field <- fields[[index]]
    full_value <- joined[[paste0(field, "_full")]][matched]
    simplified_value <- joined[[paste0(field, "_simplified")]][matched]
    both_present <- !is.na(full_value) & !is.na(simplified_value)
    equal <- .brf_b3_price_value_equal(
      full_value,
      simplified_value,
      tolerance
    )
    field_rows[[index]] <- data.frame(
      field = field,
      matched_rows = length(full_value),
      full_present = sum(!is.na(full_value)),
      simplified_present = sum(!is.na(simplified_value)),
      both_present = sum(both_present),
      equal_when_both = sum(equal & both_present),
      different_when_both = sum(!equal & both_present),
      full_only = sum(!is.na(full_value) & is.na(simplified_value)),
      simplified_only = sum(is.na(full_value) & !is.na(simplified_value)),
      both_missing = sum(is.na(full_value) & is.na(simplified_value)),
      stringsAsFactors = FALSE
    )
    noteworthy <- !equal
    if (any(noteworthy)) {
      keys_matched <- joined[matched, keys, drop = FALSE]
      differences[[length(differences) + 1L]] <- data.frame(
        keys_matched[noteworthy, , drop = FALSE],
        field = field,
        full = full_value[noteworthy],
        simplified = simplified_value[noteworthy],
        status = ifelse(
          is.na(simplified_value[noteworthy]),
          "full_only",
          ifelse(is.na(full_value[noteworthy]), "simplified_only", "different")
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  unmatched <- joined[!matched, keys, drop = FALSE]
  if (nrow(unmatched)) {
    unmatched$report <- ifelse(
      is.na(joined$key_present_full[!matched]),
      "simplified_only",
      "full_only"
    )
  } else {
    unmatched$report <- character()
  }
  daily <- data.frame(
    report_date = as.Date(date),
    full_rows = nrow(full),
    simplified_rows = nrow(simplified),
    matched_rows = sum(matched),
    full_only_rows = sum(!is.na(joined$key_present_full) &
      is.na(joined$key_present_simplified)),
    simplified_only_rows = sum(is.na(joined$key_present_full) &
      !is.na(joined$key_present_simplified)),
    full_snapshot_count = full_manifest$snapshot_count,
    simplified_snapshot_count = simplified_manifest$snapshot_count,
    full_selected_at = .brf_b3_parse_timestamp(
      full_manifest$selected_snapshot_created_at
    ),
    simplified_selected_at = .brf_b3_parse_timestamp(
      simplified_manifest$selected_snapshot_created_at
    ),
    stringsAsFactors = FALSE
  )
  list(
    daily = daily,
    field = do.call(rbind, field_rows),
    differences = if (length(differences)) {
      do.call(rbind, differences)
    } else {
      data.frame(
        date = as.Date(character()),
        root = character(),
        contract_code = character(),
        source_instrument_id = character(),
        field = character(),
        full = numeric(),
        simplified = numeric(),
        status = character(),
        stringsAsFactors = FALSE
      )
    },
    unmatched = unmatched,
    matched = joined[matched, setdiff(names(joined), c(
      "key_present_full", "key_present_simplified"
    )), drop = FALSE]
  )
}

#' Compare complete and simplified B3 price reports
#'
#' Fetches the final `BVBG.086` and `BVBG.187` snapshots for every requested
#' report date, joins futures by trade date, ticker and B3 instrument id, and
#' audits both row coverage and field equality. Full-only liquidity fields such
#' as `FinInstrmQty` and `NtlFinVol` are reported explicitly rather than being
#' confused with `RglrTxsQty` (the number of trades).
#'
#' @param dates Vector of B3 report dates.
#' @param root Optional futures root or vector of roots. Defaults to every
#'   maturity-looking futures row in the reports.
#' @param cache_dir Optional B3 reference cache root.
#' @param refresh Force downloads even for valid completed cache entries.
#' @param quiet Suppress progress messages.
#' @param tolerance Relative numeric comparison tolerance.
#' @return A list with daily coverage, aggregate field coverage, exact
#'   differences, unmatched rows and matched wide rows.
#' @export
brf_b3_prices_compare <- function(dates,
                                  root = NULL,
                                  cache_dir = NULL,
                                  refresh = FALSE,
                                  quiet = FALSE,
                                  tolerance = 1e-10) {
  dates <- sort(unique(as.Date(dates)))
  if (!length(dates) || anyNA(dates)) {
    stop("dates must contain at least one valid Date.", call. = FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || tolerance < 0) {
    stop("tolerance must be one non-negative number.", call. = FALSE)
  }
  cache_dir <- .brf_b3_reference_cache_dir(cache_dir)
  results <- vector("list", length(dates))
  for (index in seq_along(dates)) {
    if (!quiet) {
      message(
        "B3 price comparison ",
        index,
        "/",
        length(dates),
        ": ",
        dates[[index]]
      )
    }
    results[[index]] <- .brf_b3_price_compare_one(
      dates[[index]],
      root,
      cache_dir,
      refresh,
      quiet,
      tolerance
    )
  }
  daily <- do.call(rbind, lapply(results, `[[`, "daily"))
  field_daily <- do.call(rbind, lapply(seq_along(results), function(index) {
    data.frame(
      report_date = dates[[index]],
      results[[index]]$field,
      stringsAsFactors = FALSE
    )
  }))
  count_fields <- setdiff(names(field_daily), c("report_date", "field"))
  field <- stats::aggregate(
    field_daily[count_fields],
    by = list(field = field_daily$field),
    FUN = sum
  )
  differences <- do.call(rbind, lapply(results, `[[`, "differences"))
  unmatched <- do.call(rbind, lapply(results, `[[`, "unmatched"))
  matched <- do.call(rbind, lapply(results, `[[`, "matched"))
  rownames(daily) <- NULL
  rownames(field) <- NULL
  rownames(differences) <- NULL
  rownames(unmatched) <- NULL
  rownames(matched) <- NULL
  structure(
    list(
      daily = daily,
      fields = field,
      fields_daily = field_daily,
      differences = differences,
      unmatched = unmatched,
      matched = matched
    ),
    class = "brf_b3_price_comparison"
  )
}

#' @export
print.brf_b3_price_comparison <- function(x, ...) {
  cat(
    "B3 PR x SPRD comparison: ",
    nrow(x$daily),
    " report date(s), ",
    sum(x$daily$matched_rows),
    " matched futures row(s), ",
    nrow(x$unmatched),
    " unmatched row(s).\n",
    sep = ""
  )
  differing <- x$fields$different_when_both > 0L
  cat(
    "Fields with unequal values when present in both reports: ",
    sum(differing),
    ".\n",
    sep = ""
  )
  invisible(x)
}
