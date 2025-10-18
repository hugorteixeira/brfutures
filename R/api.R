#' Update cached B3 futures bulletins
#'
#' Downloads HTML bulletins for the requested commodity roots, parses them,
#' and stores both the raw files and tidy daily observations in the configured
#' cache directory.
#'
#' @param root Optional character vector with commodity roots (e.g. `"WIN"`).
#'   When omitted the function updates every root already present inside the
#'   cache directory.
#' @param start,end Optional date bounds. When `start` is `NULL` the update
#'   resumes from the day after the latest cached report for each root. The end
#'   date defaults to today.
#' @param quiet Set to `TRUE` to silence informational messages.
#' @param rebuild_agg When `TRUE` (default) rebuild cached aggregates after the
#'   update. Set to `FALSE` to defer rebuilding and call `update_brfut_agg()`
#'   later.
#'
#' @return Invisibly returns the merged aggregate data frame after the update.
#' @examples
#' \dontrun{
#' options(brfutures.cache_dir = "~/data/brfutures")
#' update_brfut("WIN", start = "2024-01-01", end = "2024-02-29")
#' }
#' @export
update_brfut <- function(root = NULL,
                         start = NULL,
                         end = Sys.Date(),
                         quiet = FALSE,
                         rebuild_agg = TRUE) {
  .brf_cache_dir()
  roots <- if (is.null(root)) {
    .brf_list_cached_roots()
  } else {
    unique(.brf_normalize_root_vector(root))
  }
  if (!length(roots)) {
    stop(
      "No roots selected. ",
      "Pass a `root` argument (e.g. 'WIN') or create folders inside the cache directory first.",
      call. = FALSE
    )
  }
  bounds <- .brf_normalize_date_bounds(start, end)
  for (item in roots) {
    .brf_update_root(item, bounds$start, bounds$end, quiet = quiet)
  }
  if (isTRUE(rebuild_agg)) {
    update_brfut_agg(all = TRUE, rebuild_roots = FALSE, quiet = quiet)
  }
  invisible(NULL)
}

.brf_prepare_root_data <- function(root, combined, skip_dates = as.Date(character())) {
  root_norm <- .brf_normalize_root(root)
  if (!nrow(combined)) {
    combined <- .brf_empty_bulletin()
  }
  if (!nrow(combined)) {
    combined$root <- combined$root
  } else {
    combined$contract_code <- trimws(as.character(combined$contract_code))
    combined$contract_code <- toupper(combined$contract_code)
    combined$contract_code <- gsub("\\s+", "", combined$contract_code, perl = TRUE)
    has_root_prefix <- startsWith(combined$contract_code, root_norm)
    combined$contract_code[!has_root_prefix & nzchar(combined$contract_code)] <- paste0(root_norm, combined$contract_code[!has_root_prefix & nzchar(combined$contract_code)])
    if (!"ticker" %in% names(combined)) {
      combined$ticker <- NA_character_
    }
    valid_codes <- nzchar(combined$contract_code)
    ticker <- combined$contract_code
    add_prefix <- valid_codes & !startsWith(ticker, root_norm)
    ticker[add_prefix] <- paste0(root_norm, ticker[add_prefix])
    ticker[!valid_codes] <- NA_character_
    combined$ticker <- ticker
    combined$date <- as.Date(combined$date)
    if (length(skip_dates)) {
      combined <- combined[!(combined$date %in% skip_dates), , drop = FALSE]
    }
    combined$root <- rep(root_norm, nrow(combined))
    combined <- combined[order(combined$date, combined$contract_code, combined$ticker), , drop = FALSE]
    combined <- .brf_deduplicate_contract_rows(combined)
    non_empty_cols <- vapply(combined, function(col) {
      if (!length(col)) {
        FALSE
      } else {
        any(!is.na(col))
      }
    }, logical(1), USE.NAMES = TRUE)
    if (any(non_empty_cols)) {
      combined <- combined[, non_empty_cols, drop = FALSE]
    }
  }
  combined
}

.brf_update_root <- function(root, start, end, quiet = FALSE) {
  root <- .brf_normalize_root(root)
  raw_dir <- .brf_raw_dir(root)
  skip_entries <- .brf_no_data_entries(root)
  skip_files <- unique(skip_entries$filename)
  skip_dates <- unique(skip_entries$date)
  skip_dates <- skip_dates[!is.na(skip_dates)]
  register_no_data <- function(paths) {
    paths <- unique(paths)
    if (!length(paths)) {
      return()
    }
    if (isTRUE(getOption("brfutures.debug_no_data", FALSE))) {
      message("Registering no-data report(s): ", paste(basename(paths), collapse = ", "))
    }
    .brf_register_no_data_files(paths, quiet = quiet, root = root)
    info <- .brf_parse_no_data_filenames(basename(paths))
    if (!nrow(info)) {
      return()
    }
    skip_files <<- unique(c(skip_files, info$filename))
    skip_dates <<- unique(c(skip_dates, info$date[!is.na(info$date)]))
    unlink(paths[file.exists(paths)])
  }
  if (length(skip_files)) {
    skip_paths <- file.path(raw_dir, skip_files)
    existing_skip_paths <- skip_paths[file.exists(skip_paths)]
    if (length(existing_skip_paths)) {
      unlink(existing_skip_paths)
      if (!quiet) {
        message(
          "Root ", root, ": removed ", length(existing_skip_paths),
          " cached no-data report(s)."
        )
      }
    }
  }
  existing_files <- .brf_existing_dates(root)
  if (length(skip_dates)) {
    existing_files <- setdiff(existing_files, skip_dates)
  }
  current <- .brf_load_root_data(root)
  start_date <- start
  if (is.null(start_date)) {
    if (length(existing_files)) {
      start_date <- max(existing_files) + 1
    } else {
      stop(
        "No cached data for root '", root, "'. Provide `start` to seed the history.",
        call. = FALSE
      )
    }
  }
  if (is.na(start_date)) {
    start_date <- end
  }
  if (start_date > end) {
    if (!quiet) {
      message("Root ", root, ": nothing to update (start after end).")
    }
    return(invisible(NULL))
  }
  target_days <- .brf_date_seq(start_date, end)
  if (length(skip_dates)) {
    target_days <- target_days[!(target_days %in% skip_dates)]
  }
  business_mask <- as.integer(format(target_days, "%u")) <= 5L
  target_days <- target_days[business_mask]
  if (!length(target_days)) {
    if (!quiet) {
      message("Root ", root, ": nothing to update (no business days in range).")
    }
    return(invisible(NULL))
  }
  newly_downloaded <- character()
  parsed_files <- character()
  new_parsed <- list()
  preexisting_needed <- character()
  parse_path <- function(path) {
    if (!file.exists(path)) {
      return()
    }
    if (isTRUE(getOption("brfutures.debug_no_data", FALSE))) {
      message("Parsing ", basename(path))
      if (file.exists(path)) {
        preview <- tryCatch(readLines(path, n = 5L), error = function(e) character())
        if (length(preview)) {
          message("First lines: ", paste(preview, collapse = " | "))
        }
      }
    }
    if (.brf_file_has_no_data_message(path)) {
      register_no_data(path)
      return()
    }
    doc_check <- tryCatch(
      xml2::read_html(path, encoding = "windows-1252"),
      error = function(e) NULL
    )
    if (!is.null(doc_check) && !.brf_root_available_in_doc(doc_check, root)) {
      register_no_data(path)
      return()
    }
    result <- tryCatch(.brf_parse_html_report(path, root), error = function(e) {
      warning("Failed to parse ", basename(path), ": ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (is.null(result)) {
      return()
    }
    if (isTRUE(getOption("brfutures.debug_no_data", FALSE))) {
      message("brf_no_data attr for ", basename(path), ": ", paste0(attr(result, "brf_no_data")), "; rows=", nrow(result))
    }
    if (isTRUE(attr(result, "brf_no_data"))) {
      if (isTRUE(getOption("brfutures.debug_no_data", FALSE))) {
        message("Detected no-data via parser for ", basename(path))
      }
      register_no_data(path)
      return()
    }
    if (!nrow(result)) {
      return()
    }
    new_parsed[[length(new_parsed) + 1L]] <<- result
    parsed_files <<- c(parsed_files, path)
  }
  for (raw_day in target_days) {
    day_date <- as.Date(raw_day, origin = "1970-01-01")
    file_name <- paste0(root, "_", format(day_date, "%Y-%m-%d"), ".html")
    dest <- file.path(raw_dir, file_name)
    if (file.exists(dest)) {
      already_cached <- nrow(current) && day_date %in% current$date
      already_skipped <- length(skip_dates) && day_date %in% skip_dates
      if (!already_cached && !already_skipped) {
        preexisting_needed <- c(preexisting_needed, dest)
      }
      next
    }
    newly_downloaded <- c(newly_downloaded, .brf_download_html(day_date, root, quiet = quiet))
    parse_path(dest)
  }
  if (!quiet && length(newly_downloaded)) {
    message("Root ", root, ": downloaded ", length(newly_downloaded), " report(s).")
  }
  parsed_needed <- newly_downloaded
  data_path <- .brf_root_data_path(root, create = FALSE)
  if (!file.exists(data_path)) {
    parsed_needed <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE)
  }
  parsed_needed <- unique(c(parsed_needed, preexisting_needed))
  parsed_needed <- setdiff(parsed_needed, parsed_files)
  parsed_needed <- parsed_needed[file.exists(parsed_needed)]
  if (length(skip_files)) {
    parsed_needed <- parsed_needed[!(basename(parsed_needed) %in% skip_files)]
  }
  if (length(parsed_needed)) {
    for (path in parsed_needed) {
      parse_path(path)
    }
  }
  combined_sources <- list(current)
  if (length(new_parsed)) {
    combined_sources[[length(combined_sources) + 1L]] <- .brf_bind_rows(new_parsed)
  }
  combined <- .brf_bind_rows(combined_sources)
  combined <- .brf_prepare_root_data(root, combined, skip_dates = skip_dates)
  .brf_save_root_data(root, combined)
  if (!quiet) {
    message("Root ", root, ": cache now has ", nrow(combined), " rows.")
  }
  invisible(NULL)
}

.brf_rebuild_root_cache <- function(root, quiet = FALSE) {
  root <- .brf_normalize_root(root)
  raw_dir <- .brf_raw_dir(root, create = FALSE)
  if (!dir.exists(raw_dir)) {
    combined <- .brf_prepare_root_data(root, .brf_empty_bulletin())
    .brf_save_root_data(root, combined)
    if (!quiet) {
      message("Root ", root, ": rebuilt cache with ", nrow(combined), " rows.")
    }
    return(invisible(combined))
  }
  html_files <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE)
  if (!length(html_files)) {
    combined <- .brf_prepare_root_data(root, .brf_empty_bulletin())
    .brf_save_root_data(root, combined)
    if (!quiet) {
      message("Root ", root, ": rebuilt cache with ", nrow(combined), " rows.")
    }
    return(invisible(combined))
  }
  .brf_handle_no_data_paths(html_files, root, quiet = quiet)
  html_files <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE)
  if (!length(html_files)) {
    combined <- .brf_prepare_root_data(root, .brf_empty_bulletin())
    .brf_save_root_data(root, combined)
    if (!quiet) {
      message("Root ", root, ": rebuilt cache with ", nrow(combined), " rows.")
    }
    return(invisible(combined))
  }
  no_data_paths <- character()
  parsed <- lapply(html_files, function(path) {
    result <- tryCatch(.brf_parse_html_report(path, root), error = function(e) {
      warning("Failed to parse ", basename(path), ": ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (is.null(result) || !nrow(result) || isTRUE(attr(result, "brf_no_data"))) {
      no_data_paths <<- c(no_data_paths, path)
      return(NULL)
    }
    result
  })
  if (length(no_data_paths)) {
    .brf_handle_no_data_paths(no_data_paths, root, quiet = quiet)
  }
  parsed <- Filter(Negate(is.null), parsed)
  combined <- if (length(parsed)) {
    .brf_bind_rows(parsed)
  } else {
    .brf_empty_bulletin()
  }
  combined <- .brf_prepare_root_data(root, combined)
  .brf_save_root_data(root, combined)
  if (!quiet) {
    message("Root ", root, ": rebuilt cache with ", nrow(combined), " rows.")
  }
  invisible(combined)
}

#' Rebuild cached root and aggregate data
#'
#' @param root Optional character vector with roots to target. When omitted
#'   and `rebuild_roots` is `TRUE`, every cached root is rebuilt from the raw
#'   HTML files.
#' @param all When `TRUE`, refreshes the aggregate cache after the optional root
#'   rebuilds. Defaults to `TRUE`.
#' @param rebuild_roots Controls whether root caches are rebuilt from the raw
#'   HTML files. When `NULL` (default) the caches are rebuilt only when specific
#'   `root` values are supplied. Set to `TRUE` or `FALSE` to override this
#'   behaviour.
#' @param quiet Set to `TRUE` to silence informational messages.
#'
#' @return Invisibly returns a list with rebuilt root data frames (if any) and
#'   the refreshed aggregate data frame when requested.
#' @export
update_brfut_agg <- function(root = NULL,
                             all = TRUE,
                             rebuild_roots = NULL,
                             quiet = FALSE) {
  .brf_cache_dir()
  selected_roots <- if (is.null(root)) {
    character()
  } else {
    unique(.brf_normalize_root_vector(root))
  }
  rebuild_roots <- if (is.null(rebuild_roots)) {
    length(selected_roots) > 0
  } else {
    isTRUE(rebuild_roots)
  }
  rebuilt <- list()
  if (isTRUE(rebuild_roots)) {
    roots_to_rebuild <- if (length(selected_roots)) {
      selected_roots
    } else {
      .brf_list_cached_roots()
    }
    if (length(roots_to_rebuild)) {
      for (item in roots_to_rebuild) {
        rebuilt[[item]] <- .brf_rebuild_root_cache(item, quiet = quiet)
      }
    }
  }
  aggregate_data <- NULL
  if (isTRUE(all) || !file.exists(.brf_aggregate_path(create = FALSE))) {
    if (length(selected_roots)) {
      root_data <- lapply(selected_roots, .brf_load_root_data)
      root_data <- root_data[lengths(root_data) > 0]
      if (length(root_data)) {
        aggregate_data <- .brf_bind_rows(root_data)
      } else {
        aggregate_data <- .brf_empty_bulletin()
      }
      existing <- .brf_load_aggregate()
      keep_roots <- setdiff(unique(existing$root), selected_roots)
      if (length(keep_roots)) {
        remaining <- existing[existing$root %in% keep_roots, , drop = FALSE]
        aggregate_data <- .brf_bind_rows(list(remaining, aggregate_data))
      }
    } else {
      aggregate_data <- .brf_update_aggregate_from_roots()
    }
    aggregate_data <- aggregate_data[order(aggregate_data$date, aggregate_data$root, aggregate_data$ticker), , drop = FALSE]
    .brf_save_aggregate(aggregate_data)
    if (!quiet) {
      message("Aggregate cache rebuilt with ", nrow(aggregate_data), " rows.")
    }
  }
  invisible(list(roots = rebuilt, aggregate = aggregate_data))
}

#' Retrieve cached B3 futures data
#'
#' @param ticker Character vector with specific contract tickers (e.g. `"WINZ24"`).
#' @param start,end Optional bounds restricting the returned dates.
#' @param treatment Either the name of a built-in treatment (e.g. `"raw"`,
#'   `"standard"`, `"ohlcv_xts"`) or a function that receives the raw data frame
#'   and returns the desired shape.
#' @param rebuild_agg Set to `TRUE` to rebuild aggregates before retrieving
#'   data. The relevant root caches are rebuilt from the raw HTML files when
#'   necessary.
#' @param ... Additional arguments forwarded to the treatment function.
#'
#' @return The result of applying `treatment` to the filtered bulletin rows.
#' @export
get_brfut <- function(ticker,
                      start = NULL,
                      end = NULL,
                      treatment = "ohlcv_xts",
                      rebuild_agg = FALSE,
                      ...) {
  if (missing(ticker)) {
    stop("Argument `ticker` is required.", call. = FALSE)
  }
  ticker_text <- toupper(trimws(as.character(ticker)))
  if (isTRUE(rebuild_agg) || !file.exists(.brf_aggregate_path(create = FALSE))) {
    update_brfut_agg(all = TRUE, rebuild_roots = FALSE, quiet = TRUE)
  }
  data <- .brf_load_aggregate()
  if (!nrow(data)) {
    stop("Aggregate cache is empty. Run update_brfut() first.", call. = FALSE)
  }
  data <- data[data$ticker %in% ticker_text, , drop = FALSE]
  if (!nrow(data)) {
    stop("Requested ticker(s) not found in cache: ", paste(ticker_text, collapse = ", "), call. = FALSE)
  }
  bounds <- .brf_normalize_date_bounds(start, end)
  data$date <- as.Date(data$date)
  data <- data[data$date >= (bounds$start %||% min(data$date)) &
                 data$date <= bounds$end, , drop = FALSE]
  data <- data[order(data$date, data$ticker), , drop = FALSE]
  treatment_fn <- .brf_resolve_treatment(treatment)
  treatment_fn(data, ...)
}

#' Load all cached bulletins within a date range
#'
#' @param start,end Date bounds. When omitted all cached rows are returned.
#' @param rebuild_agg When `TRUE`, rebuilds the cached aggregates from the
#'   latest root files before loading. Defaults to `FALSE`.
#'
#' @return A data frame with every cached contract observation within the range.
#' @export
get_brfut_agg <- function(start = NULL, end = NULL, rebuild_agg = FALSE) {
  if (isTRUE(rebuild_agg) || !file.exists(.brf_aggregate_path(create = FALSE))) {
    update_brfut_agg(all = TRUE, rebuild_roots = FALSE, quiet = TRUE)
  }
  data <- .brf_load_aggregate()
  if (!nrow(data)) {
    stop("Aggregate cache is empty. Run update_brfut() first.", call. = FALSE)
  }
  bounds <- .brf_normalize_date_bounds(start, end)
  data$date <- as.Date(data$date)
  from <- if (is.null(bounds$start)) min(data$date) else bounds$start
  to <- bounds$end
  data <- data[data$date >= from & data$date <= to, , drop = FALSE]
  data[order(data$date, data$root, data$ticker), , drop = FALSE]
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}
