.brf_bvbg_xml_cache_env <- new.env(parent = emptyenv())

.brf_bvbg_xml_text <- function(node, path) {
  found <- xml2::xml_find_first(node, path)
  if (inherits(found, "xml_missing")) {
    return(NA_character_)
  }
  text <- trimws(xml2::xml_text(found))
  if (!nzchar(text)) {
    return(NA_character_)
  }
  text
}

.brf_bvbg_xml_number <- function(x) {
  if (is.null(x)) {
    return(numeric())
  }
  text <- trimws(as.character(x))
  text[!nzchar(text)] <- NA_character_
  text[text == "-"] <- NA_character_
  has_comma <- grepl(",", text, fixed = TRUE)
  has_dot <- grepl("\\.", text)
  has_comma[is.na(has_comma)] <- FALSE
  has_dot[is.na(has_dot)] <- FALSE
  cleaned <- text
  cleaned[has_comma & has_dot] <- gsub(",", "", cleaned[has_comma & has_dot], fixed = TRUE)
  cleaned[has_comma & !has_dot] <- gsub(",", ".", cleaned[has_comma & !has_dot], fixed = TRUE)
  suppressWarnings(as.numeric(cleaned))
}

.brf_bvbg_is_future_code <- function(code) {
  grepl("^[A-Z0-9]{3,}[A-Z][0-9]{2}$", code, perl = TRUE)
}

.brf_bvbg_filter_futures <- function(df) {
  if (!inherits(df, "data.frame") || !nrow(df) || !"contract_code" %in% names(df)) {
    return(df)
  }
  code <- toupper(trimws(as.character(df$contract_code)))
  code <- gsub("\\s+", "", code, perl = TRUE)
  keep <- .brf_bvbg_is_future_code(code)
  df$contract_code <- code
  df <- df[keep, , drop = FALSE]
  df
}

.brf_bvbg_prepare_shared_data <- function(df, file_date = NA) {
  if (!inherits(df, "data.frame") || !nrow(df)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  df$contract_code <- toupper(trimws(as.character(df$contract_code)))
  df$contract_code <- gsub("\\s+", "", df$contract_code, perl = TRUE)
  df$date <- as.Date(df$date)
  if (all(is.na(df$date)) && !is.na(file_date)) {
    df$date <- rep(as.Date(file_date), nrow(df))
  } else if (!is.na(file_date)) {
    df$date[is.na(df$date)] <- as.Date(file_date)
  }
  df <- .brf_bvbg_filter_futures(df)
  if (!nrow(df)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  df$source <- "xml"
  df <- .brf_align_bulletin_schema(df)
  df
}

.brf_bvbg_filter_root_shared <- function(df, root) {
  root_norm <- .brf_normalize_root(root)
  if (!inherits(df, "data.frame") || !nrow(df)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  df$contract_code <- toupper(trimws(as.character(df$contract_code)))
  df$contract_code <- gsub("\\s+", "", df$contract_code, perl = TRUE)
  keep <- startsWith(df$contract_code, root_norm)
  df <- df[keep, , drop = FALSE]
  if (nrow(df)) {
    escaped_root <- gsub("([][{}()+*^$.|\\\\])", "\\\\\\1", root_norm)
    futures_pattern <- sprintf("^%s[A-Z][0-9]{2}$", escaped_root)
    df <- df[grepl(futures_pattern, df$contract_code), , drop = FALSE]
  }
  if (!nrow(df)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  df$root <- root_norm
  df$ticker <- df$contract_code
  df <- .brf_align_bulletin_schema(df)
  df <- df[order(df$date, df$contract_code, df$ticker), , drop = FALSE]
  df <- .brf_deduplicate_contract_rows(df)
  df
}

.brf_parse_bvbg_xml <- function(path) {
  if (!file.exists(path)) {
    stop("File '", path, "' not found.", call. = FALSE)
  }
  doc <- xml2::read_xml(path)
  source_report_type <- .brf_bvbg_xml_text(
    doc,
    ".//*[local-name()='BizGrpDtls']/*[local-name()='BizGrpTp']"
  )
  source_group_id <- .brf_bvbg_xml_text(
    doc,
    ".//*[local-name()='BizGrpDtls']/*[local-name()='BizGrpIdr']"
  )
  source_group_created_at <- .brf_bvbg_xml_text(
    doc,
    ".//*[local-name()='BizGrpDtls']/*[local-name()='CreDtAndTm']"
  )
  source_file <- basename(path)
  source_sha256 <- .brf_b3_source_file_sha256(path)
  groups <- xml2::xml_find_all(
    doc,
    ".//*[local-name()='BizGrp']"
  )
  available_raw <- character()
  message_raw <- character()
  if (length(groups)) {
    node_list <- vector("list", length(groups))
    available_raw <- rep(NA_character_, length(groups))
    message_raw <- rep(NA_character_, length(groups))
    kept <- 0L
    for (group_index in seq_along(groups)) {
      group_children <- xml2::xml_children(groups[[group_index]])
      group_names <- xml2::xml_name(group_children)
      document_index <- match("Document", group_names)
      if (is.na(document_index)) {
        next
      }
      document_children <- xml2::xml_children(
        group_children[[document_index]]
      )
      price_index <- match("PricRpt", xml2::xml_name(document_children))
      if (is.na(price_index)) {
        next
      }
      kept <- kept + 1L
      node_list[[kept]] <- document_children[[price_index]]
      app_index <- match("AppHdr", group_names)
      if (!is.na(app_index)) {
        app_children <- xml2::xml_children(group_children[[app_index]])
        app_names <- xml2::xml_name(app_children)
        created_index <- match("CreDt", app_names)
        message_index <- match("BizMsgIdr", app_names)
        if (!is.na(created_index)) {
          available_raw[[kept]] <- trimws(xml2::xml_text(
            app_children[[created_index]]
          ))
        }
        if (!is.na(message_index)) {
          message_raw[[kept]] <- trimws(xml2::xml_text(
            app_children[[message_index]]
          ))
        }
      }
    }
    nodes <- node_list[seq_len(kept)]
    available_raw <- available_raw[seq_len(kept)]
    message_raw <- message_raw[seq_len(kept)]
  } else {
    fallback <- xml2::xml_find_all(doc, ".//*[local-name()='PricRpt']")
    nodes <- lapply(seq_along(fallback), function(i) fallback[[i]])
    available_raw <- rep(NA_character_, length(nodes))
    message_raw <- rep(NA_character_, length(nodes))
  }
  if (!length(nodes)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  n <- length(nodes)
  date_vals <- rep(NA_character_, n)
  contract_code <- rep(NA_character_, n)
  open_interest <- rep(NA_character_, n)
  open <- rep(NA_character_, n)
  low <- rep(NA_character_, n)
  high <- rep(NA_character_, n)
  average_price <- rep(NA_character_, n)
  close <- rep(NA_character_, n)
  trade_count <- rep(NA_character_, n)
  contracts_traded <- rep(NA_character_, n)
  contracts_regular <- rep(NA_character_, n)
  volume <- rep(NA_character_, n)
  volume_regular <- rep(NA_character_, n)
  settlement_price <- rep(NA_character_, n)
  previous_settlement <- rep(NA_character_, n)
  change_percent <- rep(NA_character_, n)
  change_points <- rep(NA_character_, n)
  last_bid <- rep(NA_character_, n)
  last_ask <- rep(NA_character_, n)
  adjustment_value <- rep(NA_character_, n)
  available_at <- as.POSIXct(
    rep(NA_real_, n),
    origin = "1970-01-01",
    tz = "UTC"
  )
  source_message_id <- rep(NA_character_, n)
  source_instrument_id <- rep(NA_character_, n)
  settlement_status <- rep(NA_character_, n)
  previous_settlement_status <- rep(NA_character_, n)
  available_at <- .brf_b3_parse_timestamp(available_raw)
  source_message_id <- message_raw

  for (i in seq_len(n)) {
    node <- nodes[[i]]
    children <- xml2::xml_children(node)
    if (!length(children)) {
      next
    }
    child_names <- xml2::xml_name(children)
    trad_node <- children[child_names == "TradDt"]
    if (length(trad_node)) {
      trad_children <- xml2::xml_children(trad_node[[1]])
      if (length(trad_children)) {
        trad_names <- xml2::xml_name(trad_children)
        idx <- match("Dt", trad_names)
        if (!is.na(idx)) {
          date_vals[i] <- trimws(xml2::xml_text(trad_children[[idx]]))
        } else {
          date_vals[i] <- trimws(xml2::xml_text(trad_children[[1]]))
        }
      }
    }
    scty_node <- children[child_names == "SctyId"]
    if (length(scty_node)) {
      scty_children <- xml2::xml_children(scty_node[[1]])
      if (length(scty_children)) {
        scty_names <- xml2::xml_name(scty_children)
        idx <- match("TckrSymb", scty_names)
        if (!is.na(idx)) {
          contract_code[i] <- trimws(xml2::xml_text(scty_children[[idx]]))
        } else {
          contract_code[i] <- trimws(xml2::xml_text(scty_children[[1]]))
        }
      }
    }
    instrument_node <- children[child_names == "FinInstrmId"]
    if (length(instrument_node)) {
      instrument_children <- xml2::xml_children(instrument_node[[1L]])
      other_index <- match("OthrId", xml2::xml_name(instrument_children))
      if (!is.na(other_index)) {
        other_children <- xml2::xml_children(
          instrument_children[[other_index]]
        )
        id_index <- match("Id", xml2::xml_name(other_children))
        if (!is.na(id_index)) {
          source_instrument_id[i] <- trimws(xml2::xml_text(
            other_children[[id_index]]
          ))
        }
      }
    }
    attr_node <- children[child_names == "FinInstrmAttrbts"]
    if (length(attr_node)) {
      attr_children <- xml2::xml_children(attr_node[[1]])
      if (length(attr_children)) {
        attr_names <- xml2::xml_name(attr_children)
        attr_text <- trimws(xml2::xml_text(attr_children))
        attr_map <- stats::setNames(attr_text, attr_names)
        open_interest[i] <- attr_map["OpnIntrst"]
        open[i] <- attr_map["FrstPric"]
        low[i] <- attr_map["MinPric"]
        high[i] <- attr_map["MaxPric"]
        average_price[i] <- attr_map["TradAvrgPric"]
        close[i] <- attr_map["LastPric"]
        trade_count[i] <- attr_map["RglrTxsQty"]
        contracts_traded[i] <- attr_map["FinInstrmQty"]
        contracts_regular[i] <- attr_map["RglrTraddCtrcts"]
        volume[i] <- attr_map["NtlFinVol"]
        volume_regular[i] <- attr_map["NtlRglrVol"]
        settlement_price[i] <- attr_map["AdjstdQt"]
        settlement_status[i] <- attr_map["AdjstdQtStin"]
        previous_settlement[i] <- attr_map["PrvsAdjstdQt"]
        previous_settlement_status[i] <- attr_map["PrvsAdjstdQtStin"]
        change_percent[i] <- attr_map["OscnPctg"]
        change_points[i] <- attr_map["VartnPts"]
        last_bid[i] <- attr_map["BestBidPric"]
        last_ask[i] <- attr_map["BestAskPric"]
        adjustment_value[i] <- attr_map["AdjstdValCtrct"]
      }
    }
  }

  open_interest <- .brf_bvbg_xml_number(open_interest)
  open <- .brf_bvbg_xml_number(open)
  low <- .brf_bvbg_xml_number(low)
  high <- .brf_bvbg_xml_number(high)
  average_price <- .brf_bvbg_xml_number(average_price)
  close <- .brf_bvbg_xml_number(close)
  trade_count <- .brf_bvbg_xml_number(trade_count)
  contracts_traded <- .brf_bvbg_xml_number(contracts_traded)
  contracts_regular <- .brf_bvbg_xml_number(contracts_regular)
  if (length(contracts_traded)) {
    contracts_traded[is.na(contracts_traded)] <- contracts_regular[is.na(contracts_traded)]
  } else {
    contracts_traded <- contracts_regular
  }
  volume <- .brf_bvbg_xml_number(volume)
  volume_regular <- .brf_bvbg_xml_number(volume_regular)
  if (length(volume)) {
    volume[is.na(volume)] <- volume_regular[is.na(volume)]
  } else {
    volume <- volume_regular
  }
  settlement_price <- .brf_bvbg_xml_number(settlement_price)
  settlement_available_at <- available_at
  settlement_available_at[is.na(settlement_price)] <- as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = "UTC"
  )
  previous_settlement <- .brf_bvbg_xml_number(previous_settlement)
  change_percent <- .brf_bvbg_xml_number(change_percent)
  change_points <- .brf_bvbg_xml_number(change_points)
  last_bid <- .brf_bvbg_xml_number(last_bid)
  last_ask <- .brf_bvbg_xml_number(last_ask)
  adjustment_value <- .brf_bvbg_xml_number(adjustment_value)

  df <- data.frame(
    date = as.Date(date_vals),
    contract_code = toupper(trimws(as.character(contract_code))),
    available_at = available_at,
    settlement_available_at = settlement_available_at,
    source_report_type = rep(source_report_type, n),
    source_group_id = rep(source_group_id, n),
    source_group_created_at = rep(source_group_created_at, n),
    source_message_id = source_message_id,
    source_instrument_id = source_instrument_id,
    source_file = rep(source_file, n),
    source_sha256 = rep(source_sha256, n),
    open_interest = open_interest,
    trade_count = trade_count,
    contracts_traded = contracts_traded,
    volume = volume,
    open = open,
    low = low,
    high = high,
    average_price = average_price,
    close = close,
    settlement_price = settlement_price,
    settlement_status = settlement_status,
    previous_settlement = previous_settlement,
    previous_settlement_status = previous_settlement_status,
    change_percent = change_percent,
    change_points = change_points,
    last_bid = last_bid,
    last_ask = last_ask,
    adjustment_value = adjustment_value,
    stringsAsFactors = FALSE
  )
  df$contract_code <- gsub("\\s+", "", df$contract_code, perl = TRUE)
  df <- df[nzchar(df$contract_code), , drop = FALSE]
  if (!nrow(df)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  df
}

.brf_parse_bvbg_xml_cached <- function(path) {
  key <- normalizePath(path, mustWork = FALSE)
  entry <- .brf_bvbg_xml_cache_env[[key]]
  mtime <- tryCatch(file.info(path)$mtime, error = function(e) NA)
  if (!is.null(entry) && !is.na(mtime) && identical(entry$mtime, mtime)) {
    return(entry$data)
  }
  parsed <- .brf_parse_bvbg_xml(path)
  .brf_bvbg_xml_cache_env[[key]] <- list(data = parsed, mtime = mtime)
  parsed
}

.brf_parse_bvbg_xml_for_root <- function(path, root) {
  root_norm <- .brf_normalize_root(root)
  parsed <- .brf_parse_bvbg_xml_cached(path)
  if (!inherits(parsed, "data.frame") || !nrow(parsed) || isTRUE(attr(parsed, "brf_no_data"))) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  file_date <- .brf_extract_report_date_from_name(path)
  parsed <- .brf_bvbg_prepare_shared_data(parsed, file_date = file_date)
  if (isTRUE(attr(parsed, "brf_no_data"))) {
    return(parsed)
  }
  .brf_bvbg_filter_root_shared(parsed, root_norm)
}

.brf_bvbg_load_parsed_day <- function(date) {
  date <- .brf_normalize_date(date)
  path <- .brf_bvbg_parsed_path(date, create = FALSE)
  if (!file.exists(path)) {
    return(NULL)
  }
  data <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!inherits(data, "data.frame")) {
    return(NULL)
  }
  version <- attr(data, "brf_parser_version", exact = TRUE)
  if (is.null(version) || !identical(version, .brf_parser_version())) {
    return(NULL)
  }
  if (!nrow(data) || !date %in% as.Date(data$date)) {
    return(NULL)
  }
  data
}

.brf_bvbg_save_parsed_day <- function(date, data) {
  if (!inherits(data, "data.frame")) {
    stop("Parsed BVBG data must be a data frame.", call. = FALSE)
  }
  date <- .brf_normalize_date(date)
  if (!nrow(data) || !date %in% as.Date(data$date)) {
    stop(
      "Parsed BVBG data does not contain the requested report date ",
      date,
      ".",
      call. = FALSE
    )
  }
  path <- .brf_bvbg_parsed_path(date, create = TRUE)
  attr(data, "brf_parser_version") <- .brf_parser_version()
  attr(data, "brf_parsed_at") <- Sys.time()
  attr(data, "brf_report_date") <- .brf_normalize_date(date)
  saveRDS(data, path, compress = "xz")
  invisible(path)
}

.brf_bvbg_load_year <- function(year) {
  path <- .brf_bvbg_year_path(year, create = FALSE)
  if (!file.exists(path)) {
    return(NULL)
  }
  data <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!inherits(data, "data.frame")) {
    return(NULL)
  }
  version <- attr(data, "brf_parser_version", exact = TRUE)
  if (is.null(version) || !identical(version, .brf_parser_version())) {
    return(NULL)
  }
  data
}

.brf_bvbg_save_year <- function(year, data) {
  if (!inherits(data, "data.frame")) {
    stop("Yearly BVBG data must be a data frame.", call. = FALSE)
  }
  path <- .brf_bvbg_year_path(year, create = TRUE)
  attr(data, "brf_parser_version") <- .brf_parser_version()
  attr(data, "brf_parsed_at") <- Sys.time()
  saveRDS(data, path, compress = "xz")
  invisible(path)
}

.brf_bvbg_update_year <- function(date, day_data) {
  if (!inherits(day_data, "data.frame")) {
    return(invisible(NULL))
  }
  date <- .brf_normalize_date(date)
  year <- format(date, "%Y")
  existing <- .brf_bvbg_load_year(year)
  if (!inherits(existing, "data.frame")) {
    existing <- .brf_empty_bulletin()
  }
  if (nrow(existing)) {
    existing$date <- as.Date(existing$date)
    existing <- existing[!(existing$date %in% date), , drop = FALSE]
  }
  combined <- .brf_bind_rows(list(existing, day_data))
  if (nrow(combined)) {
    combined$date <- as.Date(combined$date)
    combined <- .brf_align_bulletin_schema(combined)
    combined <- combined[order(combined$date, combined$contract_code), , drop = FALSE]
  } else {
    combined <- .brf_align_bulletin_schema(combined)
  }
  .brf_bvbg_save_year(year, combined)
  combined
}

.brf_bvbg_year_data <- function(year, quiet = FALSE) {
  data <- .brf_bvbg_load_year(year)
  if (inherits(data, "data.frame")) {
    return(data)
  }
  year_dir <- .brf_bvbg_year_dir(year, create = TRUE)
  parsed_files <- list.files(year_dir, pattern = "-parsed\\.rds$", full.names = TRUE, ignore.case = TRUE)
  frames <- list()
  parsed_dates <- as.Date(character())
  for (path in parsed_files) {
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!inherits(obj, "data.frame")) {
      next
    }
    version <- attr(obj, "brf_parser_version", exact = TRUE)
    if (!identical(version, .brf_parser_version())) {
      next
    }
    frames[[length(frames) + 1L]] <- obj
    parsed_dates <- unique(c(parsed_dates, .brf_extract_report_date_from_name(path)))
  }
  raw_files <- list.files(year_dir, pattern = "-raw\\.xml$", full.names = TRUE, ignore.case = TRUE)
  raw_dates <- as.Date(vapply(
    raw_files,
    function(path) as.numeric(.brf_extract_report_date_from_name(path)),
    numeric(1L)
  ), origin = "1970-01-01")
  valid_raw <- !is.na(raw_dates) & vapply(
    seq_along(raw_files),
    function(index) {
      .brf_bvbg_xml_has_trade_date(raw_files[[index]], raw_dates[[index]])
    },
    logical(1L)
  )
  raw_dates <- raw_dates[valid_raw]
  full_root <- file.path(
    .brf_b3_reference_cache_dir(),
    "prices",
    "full"
  )
  full_days <- if (dir.exists(full_root)) {
    list.dirs(full_root, recursive = FALSE, full.names = FALSE)
  } else {
    character()
  }
  full_dates <- suppressWarnings(as.Date(full_days))
  full_dates <- full_dates[
    !is.na(full_dates) & format(full_dates, "%Y") == as.character(year)
  ]
  candidate_dates <- sort(unique(c(raw_dates, full_dates)))
  candidate_dates <- setdiff(candidate_dates, parsed_dates)
  if (length(candidate_dates)) {
    for (date_index in seq_along(candidate_dates)) {
      file_date <- as.Date(
        candidate_dates[[date_index]],
        origin = "1970-01-01"
      )
      parsed <- .brf_bvbg_ensure_parsed_day(file_date, quiet = quiet)
      if (isTRUE(attr(parsed, "brf_no_data")) || !nrow(parsed)) {
        next
      }
      frames[[length(frames) + 1L]] <- parsed
    }
  }
  if (!length(frames)) {
    data <- .brf_empty_bulletin()
  } else {
    data <- .brf_bind_rows(frames)
  }
  if (nrow(data)) {
    data$date <- as.Date(data$date)
    data <- .brf_align_bulletin_schema(data)
    data <- data[order(data$date, data$contract_code), , drop = FALSE]
  } else {
    data <- .brf_align_bulletin_schema(data)
  }
  .brf_bvbg_save_year(year, data)
  if (!quiet) {
    message("BVBG: cached year ", year, " with ", nrow(data), " row(s).")
  }
  data
}

.brf_bvbg_ensure_parsed_day <- function(date, quiet = FALSE) {
  date <- .brf_normalize_date(date)
  cached <- .brf_bvbg_load_parsed_day(date)
  if (inherits(cached, "data.frame")) {
    return(cached)
  }
  parsed <- tryCatch(
    brf_b3_prices_fetch(
      date,
      report = "full",
      quiet = quiet
    ),
    error = function(error) {
      if (!quiet) {
        message(
          "B3 full price report unavailable for ",
          date,
          ": ",
          conditionMessage(error)
        )
      }
      NULL
    }
  )
  if (!inherits(parsed, "data.frame")) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    attr(out, "brf_download_failed") <- TRUE
    return(out)
  }
  parsed <- .brf_bvbg_prepare_shared_data(parsed, file_date = date)
  if (isTRUE(attr(parsed, "brf_no_data"))) {
    return(parsed)
  }
  .brf_bvbg_save_parsed_day(date, parsed)
  .brf_bvbg_update_year(date, parsed)
  parsed
}
