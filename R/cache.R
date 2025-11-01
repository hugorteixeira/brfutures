.brf_cache_dir <- function(create = TRUE) {
  cache <- getOption("brfutures.cache_dir", default = NULL)
  if (is.null(cache) || !nzchar(cache)) {
    stop(
      "Cache directory not configured. ",
      "Call options(brfutures.cache_dir = '/path/to/cache') before using brfutures.",
      call. = FALSE
    )
  }
  cache <- path.expand(cache)
  if (create && !dir.exists(cache)) {
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }
  cache
}

.brf_root_dir <- function(root, create = TRUE) {
  root <- .brf_normalize_root(root)
  base <- .brf_cache_dir(create = create)
  path <- file.path(base, root)
  if (create && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

.brf_raw_dir <- function(root, create = TRUE) {
  dir <- file.path(.brf_root_dir(root, create = create), "raw")
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

.brf_parsed_dir <- function(root, create = TRUE) {
  dir <- file.path(.brf_root_dir(root, create = create), "parsed")
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

.brf_parsed_path <- function(root, identifier, create = TRUE) {
  root_norm <- .brf_normalize_root(root)
  if (inherits(identifier, "Date")) {
    file_name <- paste0(root_norm, "_", format(identifier, "%Y-%m-%d"), ".rds")
  } else {
    file_name <- basename(identifier)
    file_name <- sub("\\.html$", ".rds", file_name, ignore.case = TRUE)
    if (!grepl("\\.rds$", file_name, ignore.case = TRUE)) {
      file_name <- paste0(file_name, ".rds")
    }
  }
  file.path(.brf_parsed_dir(root_norm, create = create), file_name)
}

.brf_root_data_path <- function(root, create = TRUE) {
  root_norm <- .brf_normalize_root(root)
  dir <- .brf_root_dir(root_norm, create = create)
  file.path(dir, paste0(root_norm, ".rds"))
}

.brf_aggregate_path <- function(create = TRUE) {
  base <- .brf_cache_dir(create = create)
  file.path(base, "aggregate.rds")
}

.brf_cache_mark_corrupt <- function(data, path, label) {
  if (!inherits(data, "data.frame")) {
    data <- .brf_empty_bulletin()
  }
  attr(data, "brf_cache_corrupt") <- TRUE
  attr(data, "brf_cache_path") <- path
  attr(data, "brf_cache_label") <- label
  data
}

.brf_cache_is_corrupt <- function(data) {
  isTRUE(attr(data, "brf_cache_corrupt", exact = TRUE))
}

.brf_cache_strip_attrs <- function(data) {
  if (!inherits(data, "data.frame")) {
    return(data)
  }
  attr(data, "brf_cache_corrupt") <- NULL
  attr(data, "brf_cache_path") <- NULL
  attr(data, "brf_cache_label") <- NULL
  data
}

.brf_read_cache_rds <- function(path, label) {
  if (!file.exists(path)) {
    return(.brf_empty_bulletin())
  }
  obj <- tryCatch(
    readRDS(path),
    error = function(e) {
      warning(
        "Failed to read ", label, " at ", path, ": ", conditionMessage(e),
        ". The cache will be rebuilt.",
        call. = FALSE
      )
      if (file.exists(path)) {
        unlink(path)
      }
      .brf_cache_mark_corrupt(.brf_empty_bulletin(), path, label)
    }
  )
  if (.brf_cache_is_corrupt(obj)) {
    return(obj)
  }
  if (!inherits(obj, "data.frame")) {
    warning(
      "Unexpected object stored in ", label, " at ", path, ". The cache will be rebuilt.",
      call. = FALSE
    )
    if (file.exists(path)) {
      unlink(path)
    }
    return(.brf_cache_mark_corrupt(.brf_empty_bulletin(), path, label))
  }
  .brf_cache_strip_attrs(obj)
}

.brf_list_cached_roots <- function() {
  base <- tryCatch(.brf_cache_dir(create = FALSE), error = function(e) NULL)
  if (is.null(base) || !dir.exists(base)) {
    return(character())
  }
  entries <- list.dirs(base, recursive = FALSE, full.names = FALSE)
  entries[nzchar(entries)]
}

.brf_extract_report_date_from_name <- function(name) {
  if (inherits(name, "Date")) {
    return(as.Date(name))
  }
  base <- basename(name)
  match <- regexpr("(\\d{4}-\\d{2}-\\d{2})", base, perl = TRUE)
  if (match[1L] == -1L) {
    return(as.Date(NA))
  }
  found <- regmatches(base, match)
  suppressWarnings(as.Date(found))
}

.brf_list_parsed_files <- function(root) {
  dir <- .brf_parsed_dir(root, create = FALSE)
  if (!dir.exists(dir)) {
    return(character())
  }
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  files[file.exists(files)]
}

.brf_read_parsed_rds <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  obj <- tryCatch(
    readRDS(path),
    error = function(e) {
      warning(
        "Failed to read parsed cache at ", path, ": ", conditionMessage(e),
        ". The file will be discarded.",
        call. = FALSE
      )
      unlink(path)
      NULL
    }
  )
  if (!inherits(obj, "data.frame")) {
    warning(
      "Unexpected object stored at ", path, ". Removing parsed cache entry.",
      call. = FALSE
    )
    unlink(path)
    return(NULL)
  }
  obj
}

.brf_collect_parsed_data <- function(root, skip_dates = as.Date(character())) {
  files <- .brf_list_parsed_files(root)
  if (!length(files)) {
    return(list())
  }
  frames <- lapply(files, .brf_read_parsed_rds)
  frames <- Filter(function(x) inherits(x, "data.frame"), frames)
  if (!length(frames)) {
    return(list())
  }
  if (length(skip_dates)) {
    frames <- lapply(frames, function(df) {
      if (!"date" %in% names(df)) {
        return(df)
      }
      keep <- !(as.Date(df$date) %in% skip_dates)
      df[keep, , drop = FALSE]
    })
  }
  frames <- Filter(function(df) inherits(df, "data.frame") && nrow(df) > 0 && !isTRUE(attr(df, "brf_no_data")), frames)
  if (!length(frames)) {
    return(list())
  }
  lapply(frames, .brf_parsed_strip_attrs)
}

.brf_parsed_strip_attrs <- function(df) {
  if (!inherits(df, "data.frame")) {
    return(df)
  }
  attrs_to_drop <- c(
    "brf_parser_version",
    "brf_source_html",
    "brf_root",
    "brf_report_date",
    "brf_parsed_at",
    "brf_raw_header_map",
    "brf_raw_headers",
    "brf_no_data"
  )
  for (name in attrs_to_drop) {
    attr(df, name) <- NULL
  }
  df
}

.brf_parsed_is_current <- function(df) {
  version <- attr(df, "brf_parser_version", exact = TRUE)
  !is.null(version) && identical(version, .brf_parser_version())
}

.brf_save_parsed_day <- function(root, date, data) {
  if (!inherits(data, "data.frame")) {
    stop("Parsed data must be a data frame.", call. = FALSE)
  }
  path <- .brf_parsed_path(root, date, create = TRUE)
  saveRDS(data, path, compress = "xz")
  invisible(path)
}

.brf_remove_parsed_day <- function(root, date) {
  path <- .brf_parsed_path(root, date, create = FALSE)
  if (file.exists(path)) {
    unlink(path)
  }
  invisible(path)
}

.brf_existing_dates <- function(root) {
  raw_dir <- .brf_raw_dir(root, create = FALSE)
  if (!dir.exists(raw_dir)) {
    return(as.Date(character()))
  }
  files <- list.files(raw_dir, pattern = "\\.html$", full.names = FALSE)
  if (!length(files)) {
    return(as.Date(character()))
  }
  dates <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.html$", "\\1", files, perl = TRUE)
  parsed <- suppressWarnings(as.Date(dates))
  parsed[!is.na(parsed)]
}

.brf_load_root_data <- function(root) {
  root_norm <- .brf_normalize_root(root)
  path <- .brf_root_data_path(root_norm, create = FALSE)
  legacy_path <- file.path(.brf_root_dir(root_norm, create = FALSE), "data.rds")
  if (!file.exists(path) && file.exists(legacy_path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (!file.rename(legacy_path, path)) {
      legacy_data <- .brf_read_cache_rds(legacy_path, sprintf("legacy root cache for '%s'", root_norm))
      .brf_save_root_data(root_norm, .brf_cache_strip_attrs(legacy_data))
    }
  }
  data <- .brf_read_cache_rds(path, sprintf("root cache for '%s'", root_norm))
  if (.brf_cache_is_corrupt(data)) {
    rebuilt <- .brf_rebuild_root_cache(root_norm, quiet = TRUE)
    if (inherits(rebuilt, "data.frame")) {
      return(.brf_cache_strip_attrs(rebuilt))
    }
    return(.brf_empty_bulletin())
  }
  data
}

.brf_save_root_data <- function(root, data) {
  root_norm <- .brf_normalize_root(root)
  path <- .brf_root_data_path(root_norm, create = TRUE)
  legacy_path <- file.path(.brf_root_dir(root_norm, create = TRUE), "data.rds")
  saveRDS(data, path, compress = "xz")
  if (file.exists(legacy_path)) {
    unlink(legacy_path)
  }
  invisible(path)
}

.brf_load_aggregate <- function() {
  path <- .brf_aggregate_path(create = FALSE)
  data <- .brf_read_cache_rds(path, "aggregate cache")
  if (.brf_cache_is_corrupt(data)) {
    rebuilt <- .brf_update_aggregate_from_roots()
    if (inherits(rebuilt, "data.frame")) {
      return(.brf_cache_strip_attrs(rebuilt))
    }
    return(.brf_empty_bulletin())
  }
  data
}

.brf_save_aggregate <- function(data) {
  path <- .brf_aggregate_path(create = TRUE)
  saveRDS(data, path, compress = "xz")
  invisible(path)
}

.brf_update_aggregate_from_roots <- function() {
  roots <- .brf_list_cached_roots()
  if (!length(roots)) {
    .brf_save_aggregate(.brf_empty_bulletin())
    return(invisible(.brf_empty_bulletin()))
  }
  combined <- lapply(roots, .brf_load_root_data)
  combined <- combined[lengths(combined) > 0]
  if (!length(combined)) {
    agg <- .brf_empty_bulletin()
  } else {
    agg <- .brf_bind_rows(combined)
  }
  .brf_save_aggregate(agg)
  invisible(agg)
}

.brf_bind_rows <- function(data_list) {
  if (!length(data_list)) {
    return(.brf_empty_bulletin())
  }
  cols <- unique(unlist(lapply(data_list, names), use.names = FALSE))
  filled <- lapply(data_list, function(df) {
    missing <- setdiff(cols, names(df))
    if (length(missing)) {
      for (col in missing) {
        df[[col]] <- rep(NA, nrow(df))
      }
    }
    df[cols]
  })
  out <- do.call(rbind, filled)
  rownames(out) <- NULL
  out
}
