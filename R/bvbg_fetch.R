.brf_bvbg_dir <- function(create = TRUE) {
  base <- .brf_cache_dir(create = create)
  path <- file.path(base, "BDI", "BVBG")
  if (create && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

.brf_bvbg_year_dir <- function(date_or_year, create = TRUE) {
  year <- if (inherits(date_or_year, "Date")) {
    format(date_or_year, "%Y")
  } else {
    as.character(date_or_year)
  }
  year <- year[nzchar(year)][1]
  path <- file.path(.brf_bvbg_dir(create = create), year)
  if (create && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

.brf_bvbg_raw_path <- function(date, create = TRUE) {
  date <- .brf_normalize_date(date)
  dir <- .brf_bvbg_year_dir(date, create = create)
  file.path(dir, paste0(format(date, "%Y-%m-%d"), "-raw.xml"))
}

.brf_bvbg_parsed_path <- function(date, create = TRUE) {
  date <- .brf_normalize_date(date)
  dir <- .brf_bvbg_year_dir(date, create = create)
  file.path(dir, paste0(format(date, "%Y-%m-%d"), "-parsed.rds"))
}

.brf_bvbg_year_path <- function(year, create = TRUE) {
  year <- as.character(year)
  year <- year[nzchar(year)][1]
  dir <- .brf_bvbg_year_dir(year, create = create)
  file.path(dir, paste0(year, ".rds"))
}

.brf_bvbg_list_years <- function() {
  base <- .brf_bvbg_dir(create = FALSE)
  if (is.null(base) || !dir.exists(base)) {
    return(character())
  }
  entries <- list.dirs(base, recursive = FALSE, full.names = FALSE)
  entries <- entries[grepl("^\\d{4}$", entries)]
  entries[nzchar(entries)]
}

.brf_bvbg_xml_has_trade_date <- function(path, date) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  date <- .brf_normalize_date(date)
  namespace <- "(?:[[:alnum:]_.-]+:)?"
  pattern <- paste0(
    "<", namespace, "Dt(?:\\s[^>]*)?>\\s*",
    format(date, "%Y-%m-%d"),
    "\\s*</", namespace, "Dt\\s*>"
  )
  connection <- file(path, open = "rt", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  repeat {
    lines <- readLines(
      connection,
      n = 50000L,
      warn = FALSE,
      encoding = "UTF-8"
    )
    if (!length(lines)) {
      return(FALSE)
    }
    if (any(grepl(pattern, lines, perl = TRUE))) {
      return(TRUE)
    }
  }
}

.brf_bvbg_user_agent <- function() {
  ua <- getOption("brfutures.bvbg_user_agent", NULL)
  if (is.null(ua) || !nzchar(ua)) {
    ua <- paste(
      "Mozilla/5.0 (X11; Linux x86_64)",
      "AppleWebKit/537.36 (KHTML, like Gecko)",
      "Chrome/120.0.0.0 Safari/537.36"
    )
  }
  ua
}
