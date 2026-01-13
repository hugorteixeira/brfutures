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

.brf_xml_cutover_date <- function() {
  override <- getOption("brfutures.xml_cutover_date", NULL)
  if (is.null(override)) {
    override <- getOption("brfutures.bdi_cutover_date", NULL)
  }
  if (!is.null(override)) {
    return(.brf_normalize_date(override))
  }
  as.Date("2025-12-16")
}

.brf_source_for_date <- function(date) {
  date <- .brf_normalize_date(date)
  if (is.na(date)) {
    return("html")
  }
  if (date >= .brf_xml_cutover_date()) "xml" else "html"
}

.brf_xts_apply_timezone <- function(x, tz, keep_time = TRUE) {
  if (!xts::is.xts(x)) {
    return(x)
  }
  if (is.null(tz) || is.na(tz) || !nzchar(tz)) {
    return(x)
  }
  if (is.null(keep_time)) {
    keep_time <- TRUE
  }
  keep_time <- isTRUE(keep_time)
  idx <- zoo::index(x)
  if (inherits(idx, "POSIXt")) {
    idx <- if (keep_time) {
      lubridate::force_tz(idx, tzone = tz)
    } else {
      lubridate::with_tz(idx, tzone = tz)
    }
  } else {
    if (keep_time) {
      idx <- lubridate::force_tz(idx, tzone = tz)
    } else {
      idx <- as.POSIXct(idx, tz = "UTC")
      idx <- lubridate::with_tz(idx, tzone = tz)
    }
  }
  attr(x, "index") <- as.numeric(idx)
  attr(x, "tzone") <- tz
  attr(x, ".indexCLASS") <- c("POSIXct", "POSIXt")
  attr(x, ".indexTZ") <- tz
  attr(attr(x, "index"), "tclass") <- c("POSIXct", "POSIXt")
  attr(attr(x, "index"), "tzone") <- tz
  x
}

.brf_is_xml_path <- function(path) {
  is.character(path) && length(path) == 1L && grepl("\\.xml$", path, ignore.case = TRUE)
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
    source = character(),
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
    reference_settlement = numeric(),
    reference_price = numeric(),
    adjustment_value = numeric(),
    stringsAsFactors = FALSE
  )
}

.brf_align_bulletin_schema <- function(df) {
  if (!inherits(df, "data.frame")) {
    return(df)
  }
  template <- .brf_empty_bulletin()
  base_cols <- names(template)
  missing <- setdiff(base_cols, names(df))
  if (length(missing)) {
    for (col in missing) {
      base_col <- template[[col]]
      if (inherits(base_col, "Date")) {
        df[[col]] <- as.Date(rep(NA, nrow(df)))
      } else if (is.numeric(base_col)) {
        df[[col]] <- rep(NA_real_, nrow(df))
      } else {
        df[[col]] <- rep(NA_character_, nrow(df))
      }
    }
  }
  extra <- setdiff(names(df), base_cols)
  df <- df[, c(base_cols, extra), drop = FALSE]
  df
}

.brf_pt_number <- function(x) {
  if (is.null(x)) {
    return(numeric())
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
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

.brf_coalesce_duplicate_columns <- function(df, prefer_large = character()) {
  if (!inherits(df, "data.frame")) {
    return(df)
  }
  if (!nrow(df) || !length(names(df))) {
    return(df)
  }
  names_df <- names(df)
  base <- sub("\\.\\d+$", "", names_df)
  if (!any(duplicated(base))) {
    return(df)
  }
  base_order <- unique(base)
  out <- vector("list", length(base_order))
  names(out) <- base_order
  for (base_name in base_order) {
    idx <- which(base == base_name)
    if (length(idx) == 1L) {
      out[[base_name]] <- df[[idx]]
      next
    }
    cols <- lapply(idx, function(i) df[[i]])
    num_cols <- lapply(cols, function(col) suppressWarnings(as.numeric(col)))
    non_na <- vapply(num_cols, function(col) sum(!is.na(col)), integer(1))
    main_pos <- idx[which.max(non_na)]
    if (base_name %in% prefer_large) {
      med <- vapply(num_cols, function(col) {
        col <- col[is.finite(col)]
        if (!length(col)) {
          return(NA_real_)
        }
        median(abs(col), na.rm = TRUE)
      }, numeric(1))
      if (any(is.finite(med))) {
        main_pos <- idx[which.max(med)]
      }
    }
    main_vals <- df[[main_pos]]
    for (j in idx[idx != main_pos]) {
      other_vals <- df[[j]]
      missing <- is.na(main_vals)
      if (is.character(main_vals)) {
        missing <- missing | !nzchar(main_vals)
      }
      if (any(missing)) {
        main_vals[missing] <- other_vals[missing]
      }
    }
    out[[base_name]] <- main_vals
  }
  out_df <- as.data.frame(out, stringsAsFactors = FALSE)
  rownames(out_df) <- NULL
  out_df
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

.brf_is_business_day <- function(date) {
  if (is.null(date)) {
    return(logical())
  }
  date <- as.Date(date)
  wday <- as.POSIXlt(date)$wday
  !is.na(wday) & wday >= 1 & wday <= 5
}

.brf_next_business_day <- function(date, n = 1) {
  if (is.null(date) || is.na(date)) {
    return(as.Date(NA))
  }
  out <- as.Date(date)
  for (i in seq_len(n)) {
    repeat {
      out <- out + 1
      if (.brf_is_business_day(out)) break
    }
  }
  out
}

.brf_previous_business_day <- function(date, n = 1) {
  if (is.null(date) || is.na(date)) {
    return(as.Date(NA))
  }
  out <- as.Date(date)
  for (i in seq_len(n)) {
    repeat {
      out <- out - 1
      if (.brf_is_business_day(out)) break
    }
  }
  out
}

.brf_first_business_day <- function(year, month) {
  start <- as.Date(sprintf("%04d-%02d-01", year, month))
  if (.brf_is_business_day(start)) {
    return(start)
  }
  .brf_next_business_day(start, n = 1)
}

.brf_last_business_day <- function(year, month) {
  next_month <- if (month == 12) 1 else month + 1
  next_year <- if (month == 12) year + 1 else year
  start <- as.Date(sprintf("%04d-%02d-01", next_year, next_month)) - 1
  if (.brf_is_business_day(start)) {
    return(start)
  }
  .brf_previous_business_day(start, n = 1)
}

.brf_business_day_before <- function(year, month, day, offset = 1) {
  reference <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  if (!.brf_is_business_day(reference)) {
    reference <- .brf_previous_business_day(reference, n = 1)
  }
  if (offset <= 0) {
    return(reference)
  }
  .brf_previous_business_day(reference, n = offset)
}

.brf_nearest_weekday <- function(year, month, day, weekday) {
  target <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  window <- seq(target - 7, target + 7, by = "day")
  wday <- as.POSIXlt(window)$wday
  candidates <- window[wday == weekday]
  if (!length(candidates)) {
    return(as.Date(NA))
  }
  distances <- abs(as.numeric(candidates - target))
  selected <- candidates[distances == min(distances)]
  min(selected)
}

.brf_infer_contract_year <- function(two_digit, reference_date = NULL) {
  if (is.null(two_digit) || is.na(two_digit)) {
    return(NA_integer_)
  }
  year_val <- suppressWarnings(as.integer(two_digit))
  if (is.na(year_val)) {
    return(NA_integer_)
  }
  if (!inherits(reference_date, "Date") || is.na(reference_date)) {
    return(if (year_val <= 40) 2000 + year_val else 1900 + year_val)
  }
  ref_year <- as.integer(format(reference_date, "%Y"))
  base_century <- ref_year - (ref_year %% 100)
  candidate <- base_century + year_val
  while (candidate < ref_year - 10) {
    candidate <- candidate + 100
  }
  while (candidate > ref_year + 50) {
    candidate <- candidate - 100
  }
  candidate
}
