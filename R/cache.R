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

.brf_root_data_path <- function(root, create = TRUE) {
  root_norm <- .brf_normalize_root(root)
  dir <- .brf_root_dir(root_norm, create = create)
  file.path(dir, paste0(root_norm, ".rds"))
}

.brf_aggregate_path <- function(create = TRUE) {
  base <- .brf_cache_dir(create = create)
  file.path(base, "aggregate.rds")
}

.brf_list_cached_roots <- function() {
  base <- tryCatch(.brf_cache_dir(create = FALSE), error = function(e) NULL)
  if (is.null(base) || !dir.exists(base)) {
    return(character())
  }
  entries <- list.dirs(base, recursive = FALSE, full.names = FALSE)
  entries[nzchar(entries)]
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
      legacy_data <- readRDS(legacy_path)
      .brf_save_root_data(root_norm, legacy_data)
    }
  }
  if (!file.exists(path)) {
    return(.brf_empty_bulletin())
  }
  data <- readRDS(path)
  if (!is.data.frame(data)) {
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
  if (!file.exists(path)) {
    return(.brf_empty_bulletin())
  }
  data <- readRDS(path)
  if (!is.data.frame(data)) {
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
