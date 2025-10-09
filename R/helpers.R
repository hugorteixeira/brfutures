.normalize_bmf_date <- function(date) {
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

.bmf_format_date_for_query <- function(date) {
  utils::URLencode(format(date, "%d/%m/%Y"), reserved = TRUE)
}

.bmf_table_node_to_df <- function(node, element_id) {
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

.bmf_extract_table_from_js <- function(doc, element_id) {
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

.bmf_extract_table <- function(doc, element_id) {
  table_xpath <- sprintf("//td[@id='%s']//table", element_id)
  node <- xml2::xml_find_first(doc, table_xpath)
  if (!inherits(node, "xml_missing")) {
    return(.bmf_table_node_to_df(node, element_id))
  }
  recovered <- .bmf_extract_table_from_js(doc, element_id)
  if (!is.null(recovered)) {
    recovered_doc <- xml2::read_html(recovered)
    recovered_node <- xml2::xml_find_first(recovered_doc, "//table")
    if (!inherits(recovered_node, "xml_missing")) {
      return(.bmf_table_node_to_df(recovered_node, element_id))
    }
  }
  stop(sprintf("Table with id '%s' was not found in the report.", element_id), call. = FALSE)
}

.bmf_extract_full_bulletin_table <- function(doc) {
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

.bmf_report_has_no_data_message <- function(doc) {
  nodes <- xml2::xml_find_all(doc, "//td")
  if (!length(nodes)) {
    return(FALSE)
  }
  texts <- xml2::xml_text(nodes)
  texts <- iconv(texts, from = "UTF-8", to = "ASCII//TRANSLIT")
  any(grepl("Nao ha dados", texts, ignore.case = TRUE))
}

.bmf_empty_bulletin_dataframe <- function() {
  cols <- list(
    date = as.Date(character()),
    ticker_root = character(),
    mercadoria = character(),
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

.bmf_rename_bulletin_columns <- function(columns) {
  mapping <- c(
    "VENCTO" = "contract_code",
    "CONTR. ABERT.(1)" = "open_interest",
    "CONTR. FECH.(2)" = "close_interest",
    "NUM. NEGOC." = "trade_count",
    "CONTR. NEGOC." = "contracts_traded",
    "VOL." = "volume",
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

.bmf_parse_ptbr_number <- function(x) {
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

.bmf_extract_date_from_text <- function(text) {
  match <- regexpr("(\\d{2}/\\d{2}/\\d{4})", text, perl = TRUE)
  if (match[1L] == -1L) {
    return(NA)
  }
  found <- regmatches(text, match)
  .normalize_bmf_date(found)
}

.bmf_guess_report_date <- function(path, doc) {
  nodes <- xml2::xml_find_all(doc, "//td[contains(., 'ATUALIZADO EM')]")
  if (length(nodes)) {
    for (node in nodes) {
      text <- iconv(xml2::xml_text(node), from = "UTF-8", to = "ASCII//TRANSLIT")
      guessed <- .bmf_extract_date_from_text(text)
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

.bmf_extract_date_from_filename <- function(path) {
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

.bmf_normalize_ticker_root <- function(ticker_root) {
  if (is.null(ticker_root) || !length(ticker_root)) {
    return(NA_character_)
  }
  na_idx <- is.na(ticker_root)
  out <- trimws(as.character(ticker_root))
  if (length(out) != length(na_idx)) {
    na_idx <- rep(FALSE, length(out))
  }
  out[na_idx] <- NA_character_
  out[is.na(out) | !nzchar(out)] <- NA_character_
  if (all(is.na(out))) {
    return(out)
  }
  toupper(out)
}

.bmf_infer_ticker_from_path <- function(path) {
  file_name <- basename(path)
  without_ext <- sub("\\.[^.]+$", "", file_name)
  inferred <- sub("_.*$", "", without_ext)
  .bmf_normalize_ticker_root(inferred)
}

.bmf_storage_dir <- function(ticker_root = NULL,
                             which = c("cache", "data", "config"),
                             create = TRUE) {
  which <- match.arg(which)
  base_dir <- tools::R_user_dir("brfutures", which = which)
  if (create && !dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (is.null(ticker_root)) {
    return(base_dir)
  }
  normalized <- .bmf_normalize_ticker_root(ticker_root)
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

.bmf_detect_report_format <- function(path) {
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
