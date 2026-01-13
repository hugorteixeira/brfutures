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

.brf_bvbg_zip_names <- function(date) {
  date <- .brf_normalize_date(date)
  stamp_ymd <- format(date, "%y%m%d")
  stamp_ydm <- format(date, "%y%d%m")
  unique(paste0("SPRD", c(stamp_ymd, stamp_ydm), ".zip"))
}

.brf_bvbg_urls <- function(date) {
  paste0("https://www.b3.com.br/pesquisapregao/download?filelist=", .brf_bvbg_zip_names(date))
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

.brf_bvbg_select_xml <- function(files, date) {
  if (!length(files)) {
    return(NA_character_)
  }
  date <- .brf_normalize_date(date)
  patterns <- c(format(date, "%Y%m%d"), format(date, "%Y-%m-%d"))
  hits <- files[grepl(paste(patterns, collapse = "|"), basename(files), ignore.case = TRUE)]
  if (length(hits)) {
    return(hits[1])
  }
  files[1]
}

.brf_bvbg_find_xml_in_dir <- function(dir, date) {
  if (!dir.exists(dir)) {
    return(NA_character_)
  }
  files <- list.files(dir, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  .brf_bvbg_select_xml(files, date)
}

.brf_bvbg_find_xml <- function(date) {
  date <- .brf_normalize_date(date)
  raw_path <- .brf_bvbg_raw_path(date, create = FALSE)
  if (file.exists(raw_path)) {
    return(raw_path)
  }
  date_patterns <- c(format(date, "%Y%m%d"), format(date, "%Y-%m-%d"))
  candidates <- getOption("brfutures.bvbg_xml_path", NULL)
  if (!is.null(candidates)) {
    candidates <- as.character(candidates)
    candidates <- candidates[nzchar(candidates)][1]
  }
  if (!is.null(candidates) && nzchar(candidates)) {
    candidates <- path.expand(candidates)
    if (dir.exists(candidates)) {
      files <- list.files(candidates, pattern = paste(date_patterns, collapse = "|"), full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
      files <- files[grepl("\\.xml$", files, ignore.case = TRUE)]
      if (length(files)) {
        return(files[1])
      }
    } else if (file.exists(candidates)) {
      base <- basename(candidates)
      if (any(grepl(paste(date_patterns, collapse = "|"), base, ignore.case = TRUE))) {
        return(candidates)
      }
    }
  }
  cwd <- getwd()
  if (!is.null(cwd) && dir.exists(cwd)) {
    files <- list.files(cwd, pattern = paste(date_patterns, collapse = "|"), full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
    files <- files[grepl("\\.xml$", files, ignore.case = TRUE)]
    if (length(files)) {
      return(files[1])
    }
  }
  shared_dir <- .brf_bvbg_dir(create = FALSE)
  if (dir.exists(shared_dir)) {
    files <- list.files(shared_dir, pattern = paste(date_patterns, collapse = "|"), full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
    files <- files[grepl("\\.xml$", files, ignore.case = TRUE)]
    if (length(files)) {
      return(files[1])
    }
  }
  NA_character_
}

.brf_bvbg_download_zip <- function(date, quiet = FALSE) {
  urls <- .brf_bvbg_urls(date)
  for (url in urls) {
    zip_path <- tempfile(fileext = ".zip")
    ua <- .brf_bvbg_user_agent()
    resp <- tryCatch(
      httr::RETRY(
        verb = "GET",
        url = url,
        httr::user_agent(ua),
        httr::accept("application/zip,application/octet-stream,*/*"),
        times = 3,
        pause_base = 1,
        httr::write_disk(zip_path, overwrite = TRUE)
      ),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      if (file.exists(zip_path)) {
        unlink(zip_path)
      }
      next
    }
    status <- httr::status_code(resp)
    if (!is.null(status) && status >= 400) {
      if (file.exists(zip_path)) {
        unlink(zip_path)
      }
      next
    }
    listing <- suppressWarnings(tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL))
    if (is.null(listing) || !nrow(listing)) {
      if (file.exists(zip_path)) {
        unlink(zip_path)
      }
      next
    }
    return(zip_path)
  }
  if (!quiet) {
    message("No BVBG zip found for ", format(.brf_normalize_date(date), "%Y-%m-%d"))
  }
  NA_character_
}

.brf_bvbg_extract_xml_from_zip <- function(zip_path, date) {
  extract_dir <- file.path(
    tempdir(),
    paste0("brfutures_bvbg_", format(.brf_normalize_date(date), "%Y%m%d"), "_", sprintf("%05d", sample.int(99999, 1)))
  )
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  queue <- zip_path
  processed <- character()
  while (length(queue)) {
    current_zip <- queue[1]
    queue <- queue[-1]
    if (current_zip %in% processed) {
      next
    }
    processed <- c(processed, current_zip)
    dest_dir <- file.path(extract_dir, tools::file_path_sans_ext(basename(current_zip)))
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    ok <- tryCatch(
      utils::unzip(current_zip, exdir = dest_dir),
      warning = function(w) TRUE,
      error = function(e) FALSE
    )
    if (identical(ok, FALSE)) {
      next
    }
    xml_files <- list.files(dest_dir, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    xml_hit <- .brf_bvbg_select_xml(xml_files, date)
    if (!is.na(xml_hit) && nzchar(xml_hit) && file.exists(xml_hit)) {
      return(list(xml = xml_hit, dir = extract_dir))
    }
    zip_files <- list.files(dest_dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(zip_files)) {
      queue <- c(queue, zip_files)
    }
  }
  list(xml = NA_character_, dir = extract_dir)
}

.brf_download_bvbg_xml <- function(date, root, quiet = FALSE) {
  date <- .brf_normalize_date(date)
  dest <- .brf_bvbg_raw_path(date, create = TRUE)
  if (file.exists(dest)) {
    if (!quiet) {
      message("Already cached: ", dest)
    }
    return(dest)
  }
  source <- .brf_bvbg_find_xml(date)
  extract_dir <- NULL
  if (is.na(source) || !nzchar(source) || !file.exists(source)) {
    zip_path <- .brf_bvbg_download_zip(date, quiet = quiet)
    if (is.na(zip_path) || !nzchar(zip_path) || !file.exists(zip_path)) {
      return(NA_character_)
    }
    extracted <- .brf_bvbg_extract_xml_from_zip(zip_path, date)
    unlink(zip_path)
    if (is.na(extracted$xml) || !nzchar(extracted$xml) || !file.exists(extracted$xml)) {
      if (dir.exists(extracted$dir)) {
        unlink(extracted$dir, recursive = TRUE, force = TRUE)
      }
      return(NA_character_)
    }
    source <- extracted$xml
    extract_dir <- extracted$dir
  }
  if (is.na(source) || !nzchar(source) || !file.exists(source)) {
    return(NA_character_)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(source, dest, overwrite = TRUE)
  if (!is.null(extract_dir) && dir.exists(extract_dir)) {
    unlink(extract_dir, recursive = TRUE, force = TRUE)
  }
  if (!quiet) {
    message("Saved BVBG XML report for ", format(date, "%Y-%m-%d"))
  }
  dest
}
