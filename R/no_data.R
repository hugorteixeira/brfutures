.brf_no_data_empty <- function() {
  data.frame(
    filename = character(),
    root = character(),
    date = as.Date(character()),
    stringsAsFactors = FALSE
  )
}

.brf_no_data_user_path <- function(create = TRUE, source = c("html", "xml", "zip")) {
  source <- match.arg(source)
  base <- .brf_cache_dir(create = create)
  file_name <- if (identical(source, "xml")) {
    "no-data-xml.csv"
  } else if (identical(source, "zip")) {
    "no-data-zip.csv"
  } else {
    "no-data-html.csv"
  }
  path <- file.path(base, file_name)
  if (identical(source, "html")) {
    legacy <- file.path(base, "no-data.csv")
    if (file.exists(legacy) && !file.exists(path)) {
      file.rename(legacy, path)
    }
  }
  path
}

.brf_no_data_default_path <- function(source = c("html", "xml", "zip")) {
  source <- match.arg(source)
  if (identical(source, "xml") || identical(source, "zip")) {
    return("")
  }
  system.file("no-data.csv", package = "brfutures", mustWork = FALSE)
}

.brf_parse_no_data_filenames <- function(filenames, extensions = c("html", "xml")) {
  if (!length(filenames)) {
    return(.brf_no_data_empty())
  }
  ext_pattern <- paste(extensions, collapse = "|")
  pattern <- sprintf("^([A-Za-z0-9]+)_(\\d{4}-\\d{2}-\\d{2}|\\d{8})\\.(%s)$", ext_pattern)
  matches <- regexec(pattern, filenames, perl = TRUE)
  captures <- regmatches(filenames, matches)
  roots <- vapply(
    captures,
    function(parts) if (length(parts) >= 3) toupper(parts[2]) else NA_character_,
    character(1)
  )
  dates <- vapply(
    captures,
    function(parts) if (length(parts) >= 3) parts[3] else NA_character_,
    character(1)
  )
  parsed_dates <- suppressWarnings(as.Date(dates))
  compact <- is.na(parsed_dates) & nzchar(dates)
  if (any(compact)) {
    parsed_dates[compact] <- suppressWarnings(as.Date(dates[compact], format = "%Y%m%d"))
  }
  valid <- !is.na(roots) & !is.na(parsed_dates)
  if (!any(valid)) {
    return(.brf_no_data_empty())
  }
  out <- data.frame(
    filename = filenames[valid],
    root = roots[valid],
    date = parsed_dates[valid],
    stringsAsFactors = FALSE
  )
  unique(out)
}

.brf_read_no_data_filenames <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(character())
  }
  entries <- tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      colClasses = c(filename = "character"),
      comment.char = "#"
    ),
    error = function(e) data.frame(filename = character(), stringsAsFactors = FALSE)
  )
  if (!nrow(entries) || !"filename" %in% names(entries)) {
    return(character())
  }
  filenames <- trimws(as.character(entries$filename))
  unique(filenames[nzchar(filenames)])
}

.brf_default_no_data_filenames <- function(source = c("html", "xml", "zip")) {
  source <- match.arg(source)
  if (identical(source, "xml") || identical(source, "zip")) {
    return(character())
  }
  .brf_read_no_data_filenames(.brf_no_data_default_path(source = source))
}

.brf_user_no_data_filenames <- function(source = c("html", "xml", "zip")) {
  source <- match.arg(source)
  if (identical(source, "xml") || identical(source, "zip")) {
    return(character())
  }
  .brf_read_no_data_filenames(.brf_no_data_user_path(create = FALSE, source = source))
}

.brf_read_no_data_xml_entries <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(.brf_no_data_empty())
  }
  entries <- tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      comment.char = "#"
    ),
    error = function(e) data.frame(root = character(), date = character(), stringsAsFactors = FALSE)
  )
  if (!nrow(entries)) {
    return(.brf_no_data_empty())
  }
  if ("filename" %in% names(entries) && !all(c("root", "date") %in% names(entries))) {
    filenames <- trimws(as.character(entries$filename))
    filenames <- filenames[nzchar(filenames)]
    return(.brf_parse_no_data_filenames(filenames, extensions = c("html", "xml")))
  }
  if (!all(c("root", "date") %in% names(entries))) {
    return(.brf_no_data_empty())
  }
  roots <- toupper(trimws(as.character(entries$root)))
  dates <- suppressWarnings(as.Date(entries$date))
  valid <- nzchar(roots) & !is.na(dates)
  if (!any(valid)) {
    return(.brf_no_data_empty())
  }
  data.frame(
    filename = character(sum(valid)),
    root = roots[valid],
    date = dates[valid],
    stringsAsFactors = FALSE
  )
}

.brf_read_no_data_zip_entries <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(.brf_no_data_empty())
  }
  entries <- tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      comment.char = "#"
    ),
    error = function(e) data.frame(root = character(), date = character(), stringsAsFactors = FALSE)
  )
  if (!nrow(entries)) {
    return(.brf_no_data_empty())
  }
  if ("filename" %in% names(entries) && !all(c("root", "date") %in% names(entries))) {
    filenames <- trimws(as.character(entries$filename))
    filenames <- filenames[nzchar(filenames)]
    return(.brf_parse_no_data_filenames(filenames, extensions = c("html", "xml")))
  }
  if (!all(c("root", "date") %in% names(entries))) {
    return(.brf_no_data_empty())
  }
  roots <- toupper(trimws(as.character(entries$root)))
  dates <- suppressWarnings(as.Date(entries$date))
  valid <- nzchar(roots) & !is.na(dates)
  if (!any(valid)) {
    return(.brf_no_data_empty())
  }
  data.frame(
    filename = character(sum(valid)),
    root = roots[valid],
    date = dates[valid],
    stringsAsFactors = FALSE
  )
}

.brf_user_no_data_entries <- function(source = c("html", "xml", "zip")) {
  source <- match.arg(source)
  if (identical(source, "xml")) {
    path <- .brf_no_data_user_path(create = FALSE, source = source)
    return(.brf_read_no_data_xml_entries(path))
  }
  if (identical(source, "zip")) {
    path <- .brf_no_data_user_path(create = FALSE, source = source)
    return(.brf_read_no_data_zip_entries(path))
  }
  filenames <- .brf_user_no_data_filenames(source = source)
  .brf_parse_no_data_filenames(filenames)
}

.brf_no_data_entries <- function(root = NULL, source = c("html", "xml", "zip")) {
  source <- match.arg(source)
  entries <- if (identical(source, "xml") || identical(source, "zip")) {
    .brf_user_no_data_entries(source = source)
  } else {
    filenames <- unique(c(
      .brf_default_no_data_filenames(source = source),
      .brf_user_no_data_filenames(source = source)
    ))
    .brf_parse_no_data_filenames(filenames)
  }
  if (!nrow(entries)) {
    return(.brf_no_data_empty())
  }
  if (is.null(root)) {
    return(entries)
  }
  filter_roots <- .brf_normalize_root_vector(root)
  if (!length(filter_roots)) {
    return(.brf_no_data_empty())
  }
  global_roots <- c("ALL", "*")
  keep <- entries$root %in% filter_roots
  if (identical(source, "xml") || identical(source, "zip")) {
    keep <- keep | entries$root %in% global_roots
  }
  entries[keep, , drop = FALSE]
}

.brf_register_no_data_xml <- function(root, dates, quiet = FALSE) {
  root <- .brf_normalize_root(root)
  dates <- suppressWarnings(as.Date(dates))
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return(invisible(FALSE))
  }
  path <- .brf_no_data_user_path(create = TRUE, source = "xml")
  existing <- .brf_read_no_data_xml_entries(path)
  new_entries <- data.frame(
    root = rep(root, length(dates)),
    date = dates,
    stringsAsFactors = FALSE
  )
  merged <- if (nrow(existing)) {
    unique(rbind(existing[, c("root", "date")], new_entries))
  } else {
    unique(new_entries)
  }
  utils::write.csv(merged, file = path, row.names = FALSE, quote = TRUE)
  if (!isTRUE(quiet)) {
    message("Root ", root, ": recorded ", length(dates), " no-data XML day(s).")
  }
  invisible(TRUE)
}

.brf_register_no_data_zip <- function(root, dates, quiet = FALSE) {
  root <- .brf_normalize_root(root)
  dates <- suppressWarnings(as.Date(dates))
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return(invisible(FALSE))
  }
  path <- .brf_no_data_user_path(create = TRUE, source = "zip")
  existing <- .brf_read_no_data_zip_entries(path)
  new_entries <- data.frame(
    root = rep(root, length(dates)),
    date = dates,
    stringsAsFactors = FALSE
  )
  merged <- if (nrow(existing)) {
    unique(rbind(existing[, c("root", "date")], new_entries))
  } else {
    unique(new_entries)
  }
  utils::write.csv(merged, file = path, row.names = FALSE, quote = TRUE)
  if (!isTRUE(quiet)) {
    message("Root ", root, ": recorded ", length(dates), " no-data ZIP day(s).")
  }
  invisible(TRUE)
}

.brf_register_no_data_files <- function(files, quiet = FALSE, root = NULL) {
  if (!length(files)) {
    return(invisible(FALSE))
  }
  filenames <- unique(basename(files))
  filenames <- filenames[nzchar(filenames)]
  if (!length(filenames)) {
    return(invisible(FALSE))
  }
  existing <- .brf_user_no_data_filenames(source = "html")
  new_files <- setdiff(filenames, existing)
  if (!length(new_files)) {
    return(invisible(FALSE))
  }
  merged <- unique(c(existing, new_files))
  path <- .brf_no_data_user_path(create = TRUE, source = "html")
  utils::write.csv(
    data.frame(filename = merged, stringsAsFactors = FALSE),
    file = path,
    row.names = FALSE,
    quote = TRUE
  )
  if (!isTRUE(quiet)) {
    label <- if (is.null(root)) "" else paste0("Root ", root, ": ")
    message(label, "recorded ", length(new_files), " no-data report(s).")
  }
  invisible(TRUE)
}

.brf_handle_no_data_paths <- function(paths, root, quiet = FALSE) {
  paths <- unique(paths[file.exists(paths)])
  if (!length(paths)) {
    return(.brf_no_data_empty())
  }
  detect_no_data <- function(path) {
    if (!file.exists(path)) {
      return(FALSE)
    }
    if (.brf_file_has_no_data_message(path)) {
      return(TRUE)
    }
    doc <- tryCatch(
      xml2::read_html(path, encoding = "windows-1252"),
      error = function(e) NULL
    )
    if (is.null(doc)) {
      return(FALSE)
    }
    !.brf_root_available_in_doc(doc, root)
  }
  flagged <- paths[vapply(paths, detect_no_data, logical(1), USE.NAMES = FALSE)]
  if (!length(flagged)) {
    return(.brf_no_data_empty())
  }
  info <- .brf_parse_no_data_filenames(basename(flagged))
  if (!nrow(info)) {
    return(.brf_no_data_empty())
  }
  .brf_register_no_data_files(flagged, quiet = TRUE, root = root)
  to_remove <- flagged[file.exists(flagged)]
  if (length(to_remove)) {
    unlink(to_remove)
    if (!quiet) {
      message(
        "Root ", root, ": removed ", length(to_remove),
        " downloaded no-data report(s)."
      )
    }
  }
  info
}
