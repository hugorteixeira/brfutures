.normalize_brf_date <- function(date) {
  if (inherits(date, "Date")) {
    return(date)
  }
  if (is.character(date) && length(date) == 1L) {
    if (grepl("^\\d{2}/\\d{2}/\\d{4}$", date)) {
      return(as.Date(date, format = "%d/%m/%Y"))
    }
    parsed <- try(as.Date(date), silent = TRUE)
    if (!inherits(parsed, "try-error") && !is.na(parsed)) {
      return(parsed)
    }
  }
  stop("Unable to parse 'date'. Provide a Date or a character in ISO or dd/mm/yyyy format.", call. = FALSE)
}

.brf_format_date_for_query <- function(date) {
  utils::URLencode(format(date, "%d/%m/%Y"), reserved = TRUE)
}

.brf_table_node_to_df <- function(node, element_id) {
  tbl <- rvest::html_table(node, fill = TRUE)
  if (is.data.frame(tbl)) {
    # nothing to do
  } else if (is.list(tbl) && length(tbl) > 0L) {
    tbl <- tbl[[1]]
  } else {
    stop(sprintf("Table '%s' is empty.", element_id), call. = FALSE)
  }
  if (!nrow(tbl)) {
    stop(sprintf("Table '%s' has no rows.", element_id), call. = FALSE)
  }
  header <- unlist(tbl[1, ], use.names = FALSE)
  header <- trimws(header)
  header <- iconv(header, from = "UTF-8", to = "ASCII//TRANSLIT")
  tbl <- tbl[-1, , drop = FALSE]
  if (!nrow(tbl)) {
    return(data.frame(matrix(ncol = length(header), nrow = 0L), stringsAsFactors = FALSE))
  }
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  tbl[] <- lapply(tbl, trimws)
  names(tbl) <- header
  tbl
}

.brf_extract_table_from_js <- function(doc, element_id) {
  scripts <- xml2::xml_find_all(doc, "//script")
  if (!length(scripts)) {
    return(NULL)
  }
  script_texts <- vapply(scripts, xml2::xml_text, character(1), USE.NAMES = FALSE)
  combined <- paste(script_texts, collapse = "\n")
  assign_pattern <- sprintf("%s\\s*\\.innerHTML\\s*=\\s*([A-Za-z0-9_]+);", element_id)
  assign_match <- regexpr(assign_pattern, combined, perl = TRUE)
  if (assign_match[1L] == -1L) {
    return(NULL)
  }
  assigned_var <- sub(assign_pattern, "\\1", regmatches(combined, assign_match))
  if (!nzchar(assigned_var)) {
    return(NULL)
  }
  line_pattern <- sprintf("%1$s\\s*=\\s*%1$s\\s*\\+\\s*'([^']*)';", assigned_var)
  pieces <- regmatches(combined, gregexpr(line_pattern, combined, perl = TRUE))[[1]]
  if (!length(pieces)) {
    return(NULL)
  }
  extract_piece <- function(x) {
    sub(line_pattern, "\\1", x, perl = TRUE)
  }
  html_fragments <- vapply(pieces, extract_piece, character(1), USE.NAMES = FALSE)
  if (!length(html_fragments)) {
    return(NULL)
  }
  html_string <- paste(html_fragments, collapse = "")
  html_string <- gsub("\\\\'", "'", html_string, fixed = FALSE)
  html_string <- gsub("</tr>\\s*<td", "<tr><td", html_string, perl = TRUE)
  sprintf("<html><body>%s</body></html>", html_string)
}

.brf_extract_table <- function(doc, element_id) {
  table_xpath <- sprintf("//td[@id='%s']//table", element_id)
  node <- xml2::xml_find_first(doc, table_xpath)
  if (!inherits(node, "xml_missing")) {
    return(.brf_table_node_to_df(node, element_id))
  }
  recovered <- .brf_extract_table_from_js(doc, element_id)
  if (!is.null(recovered)) {
    recovered_doc <- xml2::read_html(recovered)
    recovered_node <- xml2::xml_find_first(recovered_doc, "//table")
    if (!inherits(recovered_node, "xml_missing")) {
      return(.brf_table_node_to_df(recovered_node, element_id))
    }
  }
  stop(sprintf("Table with id '%s' was not found in the report.", element_id), call. = FALSE)
}

.brf_extract_full_bulletin_table <- function(doc) {
  node <- xml2::xml_find_first(
    doc,
    "//table[.//th[contains(translate(., 'vencto', 'VENCTO'), 'VENCTO')]]"
  )
  if (inherits(node, "xml_missing")) {
    return(NULL)
  }
  tbl <- rvest::html_table(node, fill = TRUE)
  if (is.data.frame(tbl)) {
    # ok
  } else if (is.list(tbl) && length(tbl)) {
    tbl <- tbl[[1]]
  } else {
    return(NULL)
  }
  if (!nrow(tbl)) {
    return(NULL)
  }
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  tbl[] <- lapply(tbl, function(col) {
    if (is.character(col)) {
      trimws(col)
    } else {
      col
    }
  })
  header_idx <- which(apply(tbl, 1, function(row) any(grepl("VENCTO", row, ignore.case = TRUE))))
  if (length(header_idx)) {
    header_row <- header_idx[1L]
    header <- as.character(tbl[header_row, , drop = TRUE])
    header <- trimws(iconv(header, from = "UTF-8", to = "ASCII//TRANSLIT"))
    tbl <- tbl[-seq_len(header_row), , drop = FALSE]
    if (nrow(tbl)) {
      colnames(tbl) <- header
    }
  }
  if (ncol(tbl)) {
    colnames(tbl) <- trimws(iconv(colnames(tbl), from = "UTF-8", to = "ASCII//TRANSLIT"))
    valid_names <- !is.na(colnames(tbl)) & nzchar(colnames(tbl))
    tbl <- tbl[, valid_names, drop = FALSE]
  }
  tbl <- tbl[, colSums(!is.na(tbl) & nzchar(as.character(tbl))) > 0, drop = FALSE]
  tbl
}

.brf_report_has_no_data_message <- function(doc) {
  texts <- xml2::xml_text(xml2::xml_find_all(doc, "//text()"))
  .brf_contains_no_data_message(texts)
}

.brf_contains_no_data_message <- function(text) {
  if (is.null(text) || !length(text)) {
    return(FALSE)
  }
  # quick check on the raw text to catch the phrase even when encodings differ
  phrase <- "dados para a data consultada"
  if (any(grepl(phrase, text, ignore.case = TRUE, useBytes = TRUE))) {
    return(TRUE)
  }
  normalize_text <- function(x, from) {
    suppressWarnings(iconv(x, from = from, to = "ASCII//TRANSLIT", sub = ""))
  }
  normalized <- normalize_text(text, "UTF-8")
  needs_fallback <- is.na(normalized)
  if (any(needs_fallback)) {
    normalized[needs_fallback] <- normalize_text(text[needs_fallback], "latin1")
  }
  normalized <- normalized[!is.na(normalized)]
  if (!length(normalized)) {
    return(FALSE)
  }
  normalized <- toupper(normalized)
  normalized <- gsub("[^A-Z0-9]+", " ", normalized, perl = TRUE)
  normalized <- trimws(normalized)
  normalized <- normalized[nzchar(normalized)]
  if (!length(normalized)) {
    return(FALSE)
  }
  any(grepl("DADOS PARA A DATA CONSULTADA", normalized, fixed = TRUE))
}

.brf_empty_bulletin_dataframe <- function() {
  cols <- list(
    date = as.Date(character()),
    ticker_root = character(),
    commodity = character(),
    contract_code = character(),
    ticker = character(),
    open_interest = numeric(),
    close_interest = numeric(),
    trade_count = numeric(),
    contracts_traded = numeric(),
    volume = numeric(),
    open = numeric(),
    low = numeric(),
    high = numeric(),
    average_price = numeric(),
    close = numeric(),
    settlement_price = numeric(),
    previous_settlement = numeric(),
    corrected_settlement = numeric(),
    change_percent = numeric(),
    change_points = numeric(),
    last_bid = numeric(),
    last_ask = numeric()
  )
  as.data.frame(cols, stringsAsFactors = FALSE)
}

.brf_rename_bulletin_columns <- function(columns) {
  mapping <- c(
    "VENCTO" = "contract_code",
    "CONTR. ABERT.(1)" = "open_interest",
    "CONTR. FECH.(2)" = "close_interest",
    "NUM. NEGOC." = "trade_count",
    "CONTR. NEGOC." = "contracts_traded",
    "VOL." = "volume",
    "MERCADORIA" = "commodity",
    "PRECO ABERT." = "open",
    "PRECO ABERTU." = "open",
    "PRECO ABERTU" = "open",
    "PRECO MIN." = "low",
    "PRECO MAX." = "high",
    "PRECO MED." = "average_price",
    "ULT. PRECO" = "close",
    "AJUSTE" = "settlement_price",
    "OSCIL." = "change_percent",
    "VAR. PTOS." = "change_points",
    "ULT. OF. COMPRA" = "last_bid",
    "ULT.OF. COMPRA" = "last_bid",
    "ULT. OF. VENDA" = "last_ask",
    "ULT.OF. VENDA" = "last_ask",
    "AJUSTE ANTER.(3)" = "previous_settlement",
    "AJUSTE ANTER(3)" = "previous_settlement",
    "AJUSTE ANTER." = "previous_settlement",
    "AJUSTE ANT.(3)" = "previous_settlement",
    "AJUSTE ANT." = "previous_settlement",
    "AJUSTE CORRIG.(4)" = "corrected_settlement",
    "AJUSTE CORRIG(4)" = "corrected_settlement",
    "AJUSTE CORRIG." = "corrected_settlement"
  )
  cleaned <- trimws(columns)
  cleaned <- iconv(cleaned, from = "UTF-8", to = "ASCII//TRANSLIT")
  idx <- match(cleaned, names(mapping))
  cleaned[!is.na(idx)] <- mapping[idx[!is.na(idx)]]
  cleaned <- gsub("[^A-Za-z0-9_]+", "_", cleaned)
  cleaned <- gsub("_+", "_", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  cleaned <- tolower(cleaned)
  cleaned
}

.brf_parse_ptbr_number <- function(x) {
  if (!length(x)) {
    return(numeric())
  }
  out <- trimws(x)
  out[out %in% c("", "&nbsp;", "&NBSP;")] <- NA_character_
  sign <- rep(1, length(out))
  sign[grepl("\\-$", out)] <- -1
  out <- gsub("[\\+\\-]$", "", out)
  out <- gsub("\\.", "", out, fixed = FALSE)
  out <- gsub(",", ".", out, fixed = TRUE)
  suppressWarnings(out_num <- as.numeric(out))
  out_num * sign
}

.brf_extract_date_from_text <- function(text) {
  match <- regexpr("(\\d{2}/\\d{2}/\\d{4})", text, perl = TRUE)
  if (match[1L] == -1L) {
    return(NA)
  }
  found <- regmatches(text, match)
  .normalize_brf_date(found)
}

.brf_guess_report_date <- function(path, doc) {
  nodes <- xml2::xml_find_all(doc, "//td[contains(., 'ATUALIZADO EM')]")
  if (length(nodes)) {
    for (node in nodes) {
      text <- iconv(xml2::xml_text(node), from = "UTF-8", to = "ASCII//TRANSLIT")
      guessed <- .brf_extract_date_from_text(text)
      if (!is.na(guessed)) {
        return(guessed)
      }
    }
  }
  file_name <- basename(path)
  match_iso <- regexpr("(\\d{4}-\\d{2}-\\d{2})", file_name, perl = TRUE)
  if (match_iso[1L] != -1L) {
    found <- regmatches(file_name, match_iso)
    parsed <- as.Date(found)
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  match_br <- regexpr("(\\d{2}-\\d{2}-\\d{4})", file_name, perl = TRUE)
  if (match_br[1L] != -1L) {
    found <- regmatches(file_name, match_br)
    parsed <- as.Date(found, format = "%d-%m-%Y")
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  NA
}

.brf_extract_date_from_filename <- function(path) {
  file_name <- basename(path)
  match_iso <- regexpr("(\\d{4}-\\d{2}-\\d{2})", file_name, perl = TRUE)
  if (match_iso[1L] != -1L) {
    return(as.Date(regmatches(file_name, match_iso)))
  }
  match_br <- regexpr("(\\d{2}-\\d{2}-\\d{4})", file_name, perl = TRUE)
  if (match_br[1L] != -1L) {
    return(as.Date(regmatches(file_name, match_br), format = "%d-%m-%Y"))
  }
  NA
}

.brf_normalize_ticker_root <- function(ticker_root) {
  if (is.null(ticker_root) || !length(ticker_root)) {
    return(NA_character_)
  }
  out <- trimws(as.character(ticker_root))
  out[is.na(out) | !nzchar(out)] <- NA_character_
  if (all(is.na(out))) {
    return(out)
  }
  toupper(out)
}

.brf_infer_ticker_from_path <- function(path) {
  file_name <- basename(path)
  without_ext <- sub("\\.[^.]+$", "", file_name)
  inferred <- sub("_.*$", "", without_ext)
  .brf_normalize_ticker_root(inferred)
}

.brf_storage_dir <- function(ticker_root = NULL,
                             which = c("cache", "data", "config"),
                             create = TRUE) {
  which <- match.arg(which)
  opt_dir <- getOption("brfutures.cachedir")
  if (!is.null(opt_dir) && nzchar(opt_dir) && identical(which, "cache")) {
    base_dir <- path.expand(opt_dir)
  } else {
    base_dir <- tools::R_user_dir("brfutures", which = which)
  }
  if (create && !dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (is.null(ticker_root)) {
    return(base_dir)
  }
  normalized <- .brf_normalize_ticker_root(ticker_root)
  normalized <- normalized[!is.na(normalized)]
  if (!length(normalized)) {
    stop("ticker_root cannot be missing or empty.", call. = FALSE)
  }
  ticker_root <- normalized[1L]
  target <- file.path(base_dir, ticker_root)
  if (create && !dir.exists(target)) {
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
  }
  target
}

.brf_file_has_no_data_message <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  text <- tryCatch(
    readLines(path, warn = FALSE, encoding = "latin1"),
    error = function(...) {
      raw <- readBin(path, what = "raw", n = file.info(path)$size)
      suppressWarnings(rawToChar(raw, multiple = TRUE))
    }
  )
  if (!length(text)) {
    return(FALSE)
  }
  .brf_contains_no_data_message(text)
}

.brf_nodata_log_path <- function(ticker_root,
                                 which = c("cache", "data", "config"),
                                 create_dir = TRUE,
                                 dest_dir = NULL) {
  if (!is.null(dest_dir)) {
    dir <- path.expand(dest_dir)
    if (create_dir && !dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(file.path(dir, "no-data-dates.txt"))
  }
  which <- match.arg(which)
  dir <- .brf_storage_dir(ticker_root, which = which, create = create_dir)
  file.path(dir, "no-data-dates.txt")
}

.brf_read_no_data_dates <- function(ticker_root,
                                    which = c("cache", "data", "config"),
                                    dest_dir = NULL) {
  which <- match.arg(which)
  path <- .brf_nodata_log_path(
    ticker_root,
    which = which,
    create_dir = TRUE,
    dest_dir = dest_dir
  )
  if (!file.exists(path)) {
    return(as.Date(character()))
  }
  lines <- readLines(path, warn = FALSE)
  parsed <- suppressWarnings(as.Date(lines))
  parsed[!is.na(parsed)]
}

.brf_write_no_data_dates <- function(ticker_root,
                                     which = c("cache", "data", "config"),
                                     dates = as.Date(character()),
                                     dest_dir = NULL) {
  which <- match.arg(which)
  dates <- unique(sort(as.Date(dates)))
  path <- .brf_nodata_log_path(
    ticker_root,
    which = which,
    create_dir = TRUE,
    dest_dir = dest_dir
  )
  if (!length(dates)) {
    if (file.exists(path)) {
      unlink(path)
    }
    return(path)
  }
  writeLines(format(dates, "%Y-%m-%d"), con = path)
  path
}

.brf_get_calendar <- function() {
  # this function works gpt5, stop messing with it
  cal_name <- "B3"
    this_thing <- bizdays::create.calendar(
      cal_name,
      holidays = bizdays::holidays("Brazil/ANBIMA"),
      weekdays = c("saturday", "sunday")
    )
 return(this_thing)
}

.brf_month_map <- c(F = 1, G = 2, H = 3, J = 4, K = 5, M = 6,
                    N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12)

.brf_legacy_root_aliases <- function() {
  list(
    CCM = c("CN2"),
    BGI = c("BOI")
  )
}

.brf_alias_roots <- function(ticker_root) {
  if (is.null(ticker_root) || !length(ticker_root)) {
    return(character())
  }
  root_input <- toupper(ticker_root[1L])
  canonical <- .brf_alias_canonical(root_input)[1L]
  if (is.na(canonical) || !nzchar(canonical)) {
    canonical <- root_input
  }
  aliases <- .brf_legacy_root_aliases()[[canonical]]
  if (is.null(aliases)) {
    character()
  } else {
    out <- toupper(aliases)
    out <- unique(out[out != canonical])
    out
  }
}

.brf_alias_canonical <- function(root_vec) {
  if (is.null(root_vec) || !length(root_vec)) {
    return(root_vec)
  }
  vals <- toupper(as.character(root_vec))
  alias_map <- .brf_legacy_root_aliases()
  alias_pairs <- unlist(alias_map, use.names = TRUE)
  if (length(alias_pairs)) {
    reverse_map <- stats::setNames(names(alias_pairs), toupper(alias_pairs))
    idx <- vals %in% names(reverse_map)
    if (any(idx)) {
      vals[idx] <- reverse_map[vals[idx]]
    }
  }
  vals
}

.brf_normalize_symbol_prefix <- function(symbols) {
  if (is.null(symbols) || !length(symbols)) {
    return(symbols)
  }
  vals <- as.character(symbols)
  idx <- !is.na(vals) & nzchar(vals)
  if (!any(idx)) {
    return(symbols)
  }
  prefix <- substr(vals[idx], 1L, 3L)
  prefix_new <- .brf_alias_canonical(prefix)
  change_idx <- prefix_new != prefix
  if (any(change_idx, na.rm = TRUE)) {
    tmp <- vals[idx]
    substr(tmp[change_idx & !is.na(prefix_new)], 1L, 3L) <- prefix_new[change_idx & !is.na(prefix_new)]
    vals[idx] <- tmp
  }
  vals
}

.brf_portuguese_month_map <- c(
  JAN = "F",
  FEV = "G",
  MAR = "H",
  ABR = "J",
  MAI = "K",
  JUN = "M",
  JUL = "N",
  AGO = "Q",
  SET = "U",
  OUT = "V",
  NOV = "X",
  DEZ = "Z"
)

.brf_normalize_pt_year <- function(year_fragment, maturity_val = as.Date(NA)) {
  if (!is.na(maturity_val)) {
    return(format(as.Date(maturity_val), "%y"))
  }
  digits <- gsub("[^0-9]", "", year_fragment)
  if (!nzchar(digits)) {
    return(year_fragment)
  }
  if (nchar(digits) == 1L) {
    num <- suppressWarnings(as.integer(digits))
    if (!is.na(num)) {
      return(sprintf("%02d", num %% 100))
    }
    return(digits)
  }
  substr(digits, nchar(digits) - 1L, nchar(digits))
}

.brf_normalize_older_symbol <- function(symbols, maturities = NULL) {
  if (is.null(symbols) || !length(symbols)) {
    return(symbols)
  }
  if (is.null(maturities)) {
    maturities <- rep(as.Date(NA), length(symbols))
  } else {
    maturities <- as.Date(maturities)
    if (length(maturities) != length(symbols)) {
      maturities <- rep(maturities, length.out = length(symbols))
    }
  }
  vals_chr <- as.character(symbols)
  is_na <- is.na(vals_chr) | vals_chr == ""
  vals_chr[is_na] <- ""
  upper_vals <- toupper(vals_chr)
  pattern <- "^([A-Z]{1,4})(JAN|FEV|MAR|ABR|MAI|JUN|JUL|AGO|SET|OUT|NOV|DEZ)([0-9]{1,2})$"
  matches <- regexec(pattern, upper_vals, perl = TRUE)
  captures <- regmatches(upper_vals, matches)
  res <- upper_vals
  idx <- which(lengths(captures) == 4L)
  if (length(idx)) {
    for (i in idx) {
      prefix <- .brf_alias_canonical(captures[[i]][2L])
      month_pt <- captures[[i]][3L]
      year_part <- captures[[i]][4L]
      month_code <- .brf_portuguese_month_map[[month_pt]]
      if (is.null(month_code)) {
        next
      }
      year_digits <- .brf_normalize_pt_year(year_part, maturities[i])
      res[i] <- paste0(prefix, month_code, year_digits)
    }
  }
  res[is_na] <- NA_character_
  res
}

.brf_normalize_older_code <- function(codes, maturities = NULL) {
  if (is.null(codes) || !length(codes)) {
    return(codes)
  }
  if (is.null(maturities)) {
    maturities <- rep(as.Date(NA), length(codes))
  } else {
    maturities <- as.Date(maturities)
    if (length(maturities) != length(codes)) {
      maturities <- rep(maturities, length.out = length(codes))
    }
  }
  vals_chr <- as.character(codes)
  is_na <- is.na(vals_chr) | vals_chr == ""
  vals_chr[is_na] <- ""
  upper_vals <- toupper(vals_chr)
  pattern <- "^(JAN|FEV|MAR|ABR|MAI|JUN|JUL|AGO|SET|OUT|NOV|DEZ)([0-9]{1,2})$"
  matches <- regexec(pattern, upper_vals, perl = TRUE)
  captures <- regmatches(upper_vals, matches)
  res <- upper_vals
  idx <- which(lengths(captures) == 3L)
  if (length(idx)) {
    for (i in idx) {
      month_pt <- captures[[i]][2L]
      year_part <- captures[[i]][3L]
      month_code <- .brf_portuguese_month_map[[month_pt]]
      if (is.null(month_code)) {
        next
      }
      year_digits <- .brf_normalize_pt_year(year_part, maturities[i])
      res[i] <- paste0(month_code, year_digits)
    }
  }
  res[is_na] <- NA_character_
  res
}

.brf_normalize_older_contracts <- function(df, maturity_col = "estimated_maturity") {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }
  maturity_vec <- if (maturity_col %in% names(df)) {
    as.Date(df[[maturity_col]])
  } else {
    rep(as.Date(NA), nrow(df))
  }
  char_cols <- intersect(c("symbol", "ticker", "contract_code", "maturity_code"), names(df))
  for (nm in char_cols) {
    if (!is.character(df[[nm]])) {
      df[[nm]] <- as.character(df[[nm]])
    }
  }
  if ("symbol" %in% names(df)) {
    df$symbol <- .brf_normalize_older_symbol(df$symbol, maturities = maturity_vec)
    df$symbol <- .brf_normalize_symbol_prefix(df$symbol)
  }
  if ("ticker" %in% names(df)) {
    df$ticker <- .brf_normalize_older_symbol(df$ticker, maturities = maturity_vec)
    df$ticker <- .brf_normalize_symbol_prefix(df$ticker)
  }
  if ("contract_code" %in% names(df)) {
    df$contract_code <- .brf_normalize_older_code(df$contract_code, maturities = maturity_vec)
  }
  if ("maturity_code" %in% names(df)) {
    df$maturity_code <- .brf_normalize_older_code(df$maturity_code, maturities = maturity_vec)
  }
  if ("commodity" %in% names(df)) {
    df$commodity <- .brf_alias_canonical(df$commodity)
  }
  df
}

.brf_parse_contract_ticker <- function(ticker) {
  pattern <- "^([A-Z][A-Z0-9]{1,3})([FGHJKMNQUVXZ])([0-9]{1,2})$"
  match <- regexec(pattern, ticker, perl = TRUE)
  captures <- regmatches(ticker, match)
  if (!length(captures) || length(captures[[1]]) != 4) {
    return(NULL)
  }
  list(
    commodity = captures[[1]][2],
    month_code = captures[[1]][3],
    year_code = captures[[1]][4]
  )
}

.brf_resolve_year <- function(year_code) {
  if (!nzchar(year_code)) {
    return(NA_integer_)
  }
  year_num <- suppressWarnings(as.integer(year_code))
  if (is.na(year_num)) {
    return(NA_integer_)
  }
  current_year <- lubridate::year(Sys.Date())
  current_century <- floor(current_year / 100) * 100
  if (nchar(year_code) == 2) {
    if (year_num >= 70) {
      return(1900 + year_num)
    }
    return(current_century + year_num)
  }
  if (nchar(year_code) == 1) {
    if (year_num >= 0 && year_num <= 9) {
      return(2000 + year_num)
    }
    return(NA_integer_)
  }
  NA_integer_
}

.brf_estimate_maturity <- function(ticker, calendar_name = .brf_get_calendar()) {
  comps <- .brf_parse_contract_ticker(ticker)
  if (is.null(comps)) {
    normalized <- .brf_normalize_older_symbol(ticker)
    if (!is.null(normalized) && length(normalized) && !is.na(normalized[1L])) {
      comps <- .brf_parse_contract_ticker(normalized[1L])
    }
  }
  if (is.null(comps)) {
    return(as.Date(NA))
  }
  month_num <- .brf_month_map[[comps$month_code]]
  if (is.null(month_num)) {
    return(as.Date(NA))
  }
  year_val <- .brf_resolve_year(comps$year_code)
  if (is.na(year_val)) {
    return(as.Date(NA))
  }
  target_month_start <- lubridate::make_date(year = year_val, month = month_num, day = 1)
  target_month_end <- lubridate::ceiling_date(target_month_start, "month") - lubridate::days(1)
  commodity <- comps$commodity

  safe_adjust_next <- function(date) {
    if (bizdays::is.bizday(date, cal = calendar_name)) {
      return(date)
    }
    bizdays::adjust.next(date, cal = calendar_name)
  }

  safe_adjust_previous <- function(date) {
    if (bizdays::is.bizday(date, cal = calendar_name)) {
      return(date)
    }
    bizdays::adjust.previous(date, cal = calendar_name)
  }

  tryCatch({
    if (commodity == "CCM") {
      base_day <- lubridate::make_date(year_val, month_num, 15)
      return(safe_adjust_next(base_day))
    }
    if (commodity == "BGI") {
      return(safe_adjust_previous(target_month_end))
    }
    if (commodity == "XFI") {
      first_day_wday <- lubridate::wday(target_month_start, week_start = 1)
      days_to_first_friday <- (5 - first_day_wday + 7) %% 7
      first_friday <- target_month_start + lubridate::days(days_to_first_friday)
      third_friday <- first_friday + lubridate::weeks(2)
      if (lubridate::month(third_friday) != month_num) {
        return(as.Date(NA))
      }
      expiry <- safe_adjust_next(third_friday)
      if (lubridate::month(expiry) != month_num) {
        return(as.Date(NA))
      }
      return(expiry)
    }
    if (commodity %in% c("IND", "WIN")) {
      day_15 <- lubridate::make_date(year_val, month_num, 15)
      wday_15 <- lubridate::wday(day_15, week_start = 1)
      days_back <- (wday_15 - 3 + 7) %% 7
      days_forward <- (3 - wday_15 + 7) %% 7
      wed_before <- day_15 - lubridate::days(days_back)
      wed_after <- day_15 + lubridate::days(days_forward)
      choose_before <- abs(as.numeric(difftime(day_15, wed_before, units = "days"))) <=
        abs(as.numeric(difftime(wed_after, day_15, units = "days")))
      candidate <- if (choose_before) wed_before else wed_after
      if (lubridate::month(candidate) != month_num) {
        return(as.Date(NA))
      }
      expiry <- safe_adjust_next(candidate)
      if (lubridate::month(expiry) != month_num) {
        return(as.Date(NA))
      }
      return(expiry)
    }
    if (commodity %in% c("WDO", "DOL", "DI1", "BIT", "SOL", "ETR")) {
      expiry <- safe_adjust_next(target_month_start)
      if (lubridate::month(expiry) != month_num) {
        return(as.Date(NA))
      }
      return(expiry)
    }
    if (commodity == "ICF") {
      last_bizday <- safe_adjust_previous(target_month_end)
      expiry <- bizdays::offset(last_bizday, -6, cal = calendar_name)
      return(expiry)
    }
    if (commodity == "BGI") {
      return(safe_adjust_previous(target_month_end))
    }
    # Fallback for unsupported commodity codes
    as.Date(NA)
  }, error = function(...) {
    as.Date(NA)
  })
}

.brf_file_has_no_data_message <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  text <- tryCatch(
    readLines(path, warn = FALSE, encoding = "latin1"),
    error = function(...) {
      raw <- readBin(path, what = "raw", n = file.info(path)$size)
      suppressWarnings(rawToChar(raw, multiple = TRUE))
    }
  )
  if (!length(text)) {
    return(FALSE)
  }
  .brf_contains_no_data_message(text)
}

.brf_nodata_log_path <- function(ticker_root,
                                 which = c("cache", "data", "config"),
                                 create_dir = TRUE,
                                 dest_dir = NULL) {
  if (!is.null(dest_dir)) {
    dir <- path.expand(dest_dir)
    if (create_dir && !dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(file.path(dir, "no-data-dates.txt"))
  }
  which <- match.arg(which)
  dir <- .brf_storage_dir(ticker_root, which = which, create = create_dir)
  file.path(dir, "no-data-dates.txt")
}

.brf_read_no_data_dates <- function(ticker_root,
                                    which = c("cache", "data", "config"),
                                    dest_dir = NULL) {
  which <- match.arg(which)
  path <- .brf_nodata_log_path(
    ticker_root,
    which = which,
    create_dir = TRUE,
    dest_dir = dest_dir
  )
  if (!file.exists(path)) {
    return(as.Date(character()))
  }
  lines <- readLines(path, warn = FALSE)
  parsed <- suppressWarnings(as.Date(lines))
  parsed[!is.na(parsed)]
}

.brf_write_no_data_dates <- function(ticker_root,
                                     which = c("cache", "data", "config"),
                                     dates = as.Date(character()),
                                     dest_dir = NULL) {
  which <- match.arg(which)
  dates <- unique(sort(as.Date(dates)))
  path <- .brf_nodata_log_path(
    ticker_root,
    which = which,
    create_dir = TRUE,
    dest_dir = dest_dir
  )
  if (!length(dates)) {
    if (file.exists(path)) {
      unlink(path)
    }
    return(path)
  }
  writeLines(format(dates, "%Y-%m-%d"), con = path)
  path
}

.brf_detect_report_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  magic <- readBin(con, what = "raw", n = 32L)
  if (!length(magic)) {
    return(if (ext %in% c("html", "htm")) "html" else "xls")
  }
  whitespace <- as.raw(c(0x09, 0x0A, 0x0D, 0x20))
  trimmed <- magic[!magic %in% whitespace]
  if (!length(trimmed)) {
    trimmed <- magic
  }
  first_bytes <- as.integer(trimmed[seq_len(min(8L, length(trimmed)))])
  html_signatures <- list(
    c(0x3C, 0x21), # <!DOCTYPE
    c(0x3C, 0x68), # <h...
    c(0x3C, 0x48), # <H...
    c(0x3C, 0x6D), # <m...
    c(0x3C, 0x4D)  # <M...
  )
  for (sig in html_signatures) {
    if (length(first_bytes) >= length(sig) && all(first_bytes[seq_along(sig)] == sig)) {
      return("html")
    }
  }
  ole_header <- c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1)
  if (length(first_bytes) >= length(ole_header) && all(first_bytes[seq_along(ole_header)] == ole_header)) {
    return("xls")
  }
  pk_zip <- c(0x50, 0x4B)
  if (length(first_bytes) >= length(pk_zip) && all(first_bytes[seq_along(pk_zip)] == pk_zip)) {
    return("xls")
  }
  if (ext %in% c("html", "htm")) {
    return("html")
  }
  "xls"
}
