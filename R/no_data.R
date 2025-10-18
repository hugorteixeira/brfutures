.brf_no_data_empty <- function() {
  data.frame(
    filename = character(),
    root = character(),
    date = as.Date(character()),
    stringsAsFactors = FALSE
  )
}

.brf_no_data_user_path <- function(create = TRUE) {
  base <- .brf_cache_dir(create = create)
  file.path(base, "no-data.csv")
}

.brf_no_data_default_path <- function() {
  system.file("no-data.csv", package = "brfutures", mustWork = FALSE)
}

.brf_parse_no_data_filenames <- function(filenames) {
  if (!length(filenames)) {
    return(.brf_no_data_empty())
  }
  pattern <- "^([A-Za-z0-9]+)_(\\d{4}-\\d{2}-\\d{2})\\.html$"
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

.brf_default_no_data_filenames <- function() {
  .brf_read_no_data_filenames(.brf_no_data_default_path())
}

.brf_user_no_data_filenames <- function() {
  .brf_read_no_data_filenames(.brf_no_data_user_path(create = FALSE))
}

.brf_user_no_data_entries <- function() {
  filenames <- .brf_user_no_data_filenames()
  .brf_parse_no_data_filenames(filenames)
}

.brf_no_data_entries <- function(root = NULL) {
  filenames <- unique(c(
    .brf_default_no_data_filenames(),
    .brf_user_no_data_filenames()
  ))
  entries <- .brf_parse_no_data_filenames(filenames)
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
  entries[entries$root %in% filter_roots, , drop = FALSE]
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
  existing <- .brf_user_no_data_filenames()
  new_files <- setdiff(filenames, existing)
  if (!length(new_files)) {
    return(invisible(FALSE))
  }
  merged <- unique(c(existing, new_files))
  path <- .brf_no_data_user_path(create = TRUE)
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
