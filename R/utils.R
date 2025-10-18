.brf_normalize_root <- function(root) {
  if (missing(root) || is.null(root)) {
    stop("Ticker root is required.", call. = FALSE)
  }
  root <- toupper(trimws(as.character(root)))
  root <- root[nzchar(root)]
  if (!length(root)) {
    stop("Ticker root is required.", call. = FALSE)
  }
  root[1L]
}

.brf_normalize_root_vector <- function(root) {
  if (is.null(root)) {
    return(character())
  }
  vals <- toupper(trimws(as.character(root)))
  vals[nzchar(vals)]
}

.brf_normalize_date <- function(x, allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) {
      return(NULL)
    }
    stop("Date value is required.", call. = FALSE)
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
      parsed <- as.Date(x)
      if (!is.na(parsed)) {
        return(parsed)
      }
    }
    if (grepl("^\\d{2}/\\d{2}/\\d{4}$", x)) {
      parsed <- as.Date(x, format = "%d/%m/%Y")
      if (!is.na(parsed)) {
        return(parsed)
      }
    }
  }
  parsed <- try(as.Date(x), silent = TRUE)
  if (!inherits(parsed, "try-error") && !is.na(parsed)) {
    return(parsed)
  }
  stop("Unable to parse date input '", x, "'. Use Date objects or ISO strings (YYYY-MM-DD).", call. = FALSE)
}

.brf_normalize_date_bounds <- function(start, end) {
  if (is.null(end)) {
    end <- Sys.Date()
  }
  end <- .brf_normalize_date(end)
  if (is.null(start)) {
    start <- NULL
  } else {
    start <- .brf_normalize_date(start)
  }
  if (!is.null(start) && start > end) {
    stop("Start date must be on or before end date.", call. = FALSE)
  }
  list(start = start, end = end)
}

.brf_date_seq <- function(start, end) {
  seq(from = start, to = end, by = "day")
}

.brf_empty_bulletin <- function() {
  data.frame(
    date = as.Date(character()),
    root = character(),
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
    last_ask = numeric(),
    stringsAsFactors = FALSE
  )
}

.brf_pt_number <- function(x) {
  if (is.null(x)) {
    return(numeric())
  }
  out <- trimws(as.character(x))
  out[out %in% c("", "&nbsp;", "&NBSP;")] <- NA_character_
  negative <- grepl("\\-$", out)
  out <- gsub("[\\+\\-]$", "", out)
  out <- gsub("\\.", "", out, fixed = FALSE)
  out <- gsub(",", ".", out, fixed = TRUE)
  suppressWarnings(num <- as.numeric(out))
  num[negative] <- -abs(num[negative])
  num
}

.brf_extract_iso_date <- function(text) {
  match <- regexpr("(\\d{2}/\\d{2}/\\d{4})", text, perl = TRUE)
  if (match[1L] == -1L) {
    return(NA)
  }
  found <- regmatches(text, match)
  .brf_normalize_date(found)
}

.brf_contains_no_data_phrase <- function(text) {
  if (is.null(text) || !length(text)) {
    return(FALSE)
  }
  text <- text[!is.na(text)]
  if (!length(text)) {
    return(FALSE)
  }
  phrase <- "dados para a data consultada"
  if (any(grepl(phrase, text, ignore.case = TRUE, useBytes = TRUE))) {
    return(TRUE)
  }
  normalize_text <- function(x, from) {
    suppressWarnings(iconv(x, from = from, to = "ASCII//TRANSLIT", sub = ""))
  }
  candidates <- list(
    normalize_text(text, ""),
    normalize_text(text, "UTF-8"),
    normalize_text(text, "latin1"),
    normalize_text(text, "windows-1252")
  )
  normalized <- unlist(candidates, use.names = FALSE)
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
  target <- "DADOS PARA A DATA CONSULTADA"
  any(vapply(normalized, function(entry) grepl(target, entry, fixed = TRUE), logical(1)))
}

.brf_no_data_message <- function(doc) {
  snippets <- xml2::xml_text(xml2::xml_find_all(doc, "//text()"))
  .brf_contains_no_data_phrase(snippets)
}

.brf_deduplicate_contract_rows <- function(df) {
  if (!nrow(df)) {
    return(df)
  }
  key <- paste(df$date, df$ticker, sep = "|")
  keep <- !duplicated(key, fromLast = TRUE)
  df[keep, , drop = FALSE]
}

.brf_sanitize_colname <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  cleaned <- trimws(as.character(x))
  cleaned <- iconv(cleaned, from = "UTF-8", to = "ASCII//TRANSLIT")
  cleaned[is.na(cleaned)] <- ""
  cleaned <- gsub("[^A-Za-z0-9]+", "_", cleaned, perl = TRUE)
  cleaned <- gsub("_+", "_", cleaned, perl = TRUE)
  cleaned <- gsub("^_|_$", "", cleaned, perl = TRUE)
  tolower(cleaned)
}
