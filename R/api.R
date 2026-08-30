#' Update cached B3 futures bulletins
#'
#' Downloads legacy HTML bulletins or complete PR/BVBG.086 XML price reports,
#' parses them, and stores tidy daily observations in the configured cache.
#' Legacy HTML files remain raw; for PR, only the final compressed XML snapshot,
#' a futures-only RDS and a hash-verified manifest are retained.
#'
#' @details
#' Dates before the XML cutover (defaults to 2025-12-15) use the legacy HTML
#' bulletin endpoint. Dates on/after the cutover download the complete
#' `PRyymmdd.zip`/BVBG.086 report. If an archive contains several complete
#' publications, the one with the newest embedded `AppHdr/CreDt` is selected;
#' snapshots are never added together. Override the cutover date with
#' `options(brfutures.xml_cutover_date = "YYYY-MM-DD")` (or
#' `brfutures.bdi_cutover_date` for backwards compatibility). If the ZIP
#' endpoint blocks non-browser clients, set a browser-like user-agent via
#' `options(brfutures.bvbg_user_agent = "...")`.
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

.brf_sync_parsed_report <- function(path,
                                    root,
                                    register_no_data,
                                    force = FALSE,
                                    quiet = FALSE,
                                    source = NULL) {
  if (missing(register_no_data) || !is.function(register_no_data)) {
    stop("A register_no_data handler is required.", call. = FALSE)
  }
  if (!file.exists(path)) {
    return(NULL)
  }
  if (is.null(source)) {
    source <- if (.brf_is_xml_path(path)) {
      "xml"
    } else {
      "html"
    }
  }
  report_date <- .brf_extract_report_date_from_name(path)
  parsed_path <- .brf_parsed_path(root, report_date, create = FALSE)
  existing <- NULL
  if (file.exists(parsed_path)) {
    existing <- .brf_read_parsed_rds(parsed_path)
    if (!is.null(existing) && .brf_parsed_is_current(existing)) {
      no_data <- isTRUE(attr(existing, "brf_no_data"))
      if (!no_data && identical(source, "xml")) {
        current_source <- NA_character_
        if ("source" %in% names(existing) && nrow(existing)) {
          current_source <- unique(stats::na.omit(as.character(existing$source)))[1]
        }
        if (!identical(current_source, "xml")) {
          existing <- NULL
        }
      }
    }
  }
  if (!is.null(existing) && .brf_parsed_is_current(existing)) {
    no_data <- isTRUE(attr(existing, "brf_no_data"))
    if (force && !no_data) {
      return(list(
        date = report_date,
        data = existing,
        updated = FALSE,
        no_data = FALSE
      ))
    }
    if (no_data) {
      return(list(
        date = report_date,
        data = NULL,
        updated = FALSE,
        no_data = TRUE
      ))
    }
    return(NULL)
  }
  parsed <- tryCatch(
    if (identical(source, "xml")) {
      .brf_parse_bvbg_xml_for_root(path, root)
    } else {
      .brf_parse_html_report_clean(path, root)
    },
    error = function(e) {
      warning("Failed to parse ", basename(path), ": ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(parsed)) {
    return(NULL)
  }
  if (isTRUE(attr(parsed, "brf_no_data"))) {
    register_no_data(path)
    .brf_remove_parsed_day(root, report_date)
    return(list(
      date = report_date,
      data = NULL,
      updated = TRUE,
      no_data = TRUE
    ))
  }
  if (is.null(attr(parsed, "brf_parser_version", exact = TRUE))) {
    attr(parsed, "brf_parser_version") <- .brf_parser_version()
  }
  if (is.null(attr(parsed, "brf_parsed_at", exact = TRUE))) {
    attr(parsed, "brf_parsed_at") <- Sys.time()
  }
  attr(parsed, "brf_report_date") <- attr(parsed, "brf_report_date", exact = TRUE) %||% report_date
  .brf_save_parsed_day(root, report_date, parsed)
  list(
    date = report_date,
    data = parsed,
    updated = TRUE,
    no_data = FALSE
  )
}

.brf_prepare_root_data <- function(root, combined, skip_dates = as.Date(character())) {
  root_norm <- .brf_normalize_root(root)
  if (!nrow(combined)) {
    combined <- .brf_empty_bulletin()
  }
  if (!"source" %in% names(combined)) {
    combined$source <- if (nrow(combined)) rep("html", nrow(combined)) else character()
  } else {
    missing_source <- is.na(combined$source) | !nzchar(combined$source)
    if (any(missing_source)) {
      combined$source[missing_source] <- "html"
    }
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
  }
  combined <- .brf_align_bulletin_schema(combined)
  combined
}

.brf_update_root <- function(root, start, end, quiet = FALSE) {
  root <- .brf_normalize_root(root)
  raw_dir <- .brf_raw_dir(root)
  .brf_parsed_dir(root)
  skip_entries_html <- .brf_no_data_entries(root, source = "html")
  skip_entries_xml <- .brf_no_data_entries(root, source = "xml")
  skip_entries_zip <- .brf_no_data_entries(root, source = "zip")
  skip_files_html <- unique(skip_entries_html$filename)
  skip_dates_html <- unique(skip_entries_html$date)
  if (length(skip_dates_html)) {
    cutover <- .brf_xml_cutover_date()
    skip_dates_html <- skip_dates_html[!is.na(skip_dates_html) & skip_dates_html < cutover]
  }
  skip_dates_xml <- unique(skip_entries_xml$date)
  if (length(skip_dates_xml)) {
    xml_present <- vapply(skip_dates_xml, function(date) {
      file.exists(.brf_bvbg_parsed_path(date, create = FALSE))
    }, logical(1))
    skip_dates_xml <- skip_dates_xml[!xml_present]
  }
  skip_dates_zip <- unique(skip_entries_zip$date)
  if (length(skip_dates_zip)) {
    xml_present <- vapply(skip_dates_zip, function(date) {
      file.exists(.brf_bvbg_parsed_path(date, create = FALSE)) ||
        file.exists(.brf_bvbg_raw_path(date, create = FALSE))
    }, logical(1))
    skip_dates_zip <- skip_dates_zip[!xml_present]
  }
  skip_dates <- unique(c(skip_dates_html, skip_dates_xml, skip_dates_zip))
  skip_dates <- skip_dates[!is.na(skip_dates)]
  register_no_data_html <- function(paths) {
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
    skip_files_html <<- unique(c(skip_files_html, info$filename))
    new_skip_dates <- info$date[!is.na(info$date)]
    if (length(new_skip_dates)) {
      skip_dates_html <<- unique(c(skip_dates_html, new_skip_dates))
      skip_dates <<- unique(c(skip_dates, new_skip_dates))
      lapply(new_skip_dates, function(date) .brf_remove_parsed_day(root, date))
    }
    unlink(paths[file.exists(paths)])
  }
  if (length(skip_files_html)) {
    skip_paths <- file.path(raw_dir, skip_files_html)
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
    skip_dates_to_drop <- skip_dates[!is.na(skip_dates)]
    if (length(skip_dates_to_drop)) {
      lapply(skip_dates_to_drop, function(date) .brf_remove_parsed_day(root, date))
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
    } else if (nrow(current)) {
      current$date <- as.Date(current$date)
      last_date <- suppressWarnings(max(current$date, na.rm = TRUE))
      if (is.finite(last_date) && !is.na(last_date)) {
        start_date <- last_date + 1
      } else {
        start_date <- end
      }
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
  cutover <- .brf_xml_cutover_date()
  html_days <- target_days[target_days < cutover]
  xml_days <- target_days[target_days >= cutover]
  newly_downloaded <- character()
  xml_downloaded <- character()
  preexisting_needed <- character()
  parsed_updates <- list()
  refresh_dates <- as.Date(character())
  ensure_path <- function(path, force = FALSE) {
    if (!file.exists(path)) {
      return()
    }
    if (isTRUE(getOption("brfutures.debug_no_data", FALSE))) {
      message("Parsing ", basename(path))
    }
    if (.brf_file_has_no_data_message(path)) {
      register_no_data_html(path)
      return()
    }
    doc_check <- tryCatch(
      xml2::read_html(path, encoding = "windows-1252"),
      error = function(e) NULL
    )
    if (!is.null(doc_check) && !.brf_root_available_in_doc(doc_check, root)) {
      register_no_data_html(path)
      return()
    }
    result <- .brf_sync_parsed_report(
      path,
      root,
      register_no_data_html,
      force = force,
      quiet = quiet,
      source = "html"
    )
    if (is.null(result)) {
      return()
    }
    if (isTRUE(result$no_data)) {
      refresh_dates <<- unique(c(refresh_dates, result$date))
      return()
    }
    if (!is.null(result$data) && inherits(result$data, "data.frame")) {
      parsed_updates[[length(parsed_updates) + 1L]] <<- .brf_parsed_strip_attrs(result$data)
      refresh_dates <<- unique(c(refresh_dates, result$date))
    }
  }
  for (raw_day in html_days) {
    day_date <- as.Date(raw_day, origin = "1970-01-01")
    raw_files <- .brf_raw_files_for_date(root, day_date)
    if (length(raw_files)) {
      already_cached <- nrow(current) && day_date %in% as.Date(current$date)
      already_skipped <- length(skip_dates) && day_date %in% skip_dates
      if (!already_cached && !already_skipped) {
        preexisting_needed <- c(preexisting_needed, raw_files)
      }
      next
    }
    downloaded <- .brf_download_html(day_date, root, quiet = quiet)
    newly_downloaded <- c(newly_downloaded, downloaded)
    ensure_path(downloaded, force = FALSE)
  }
  if (length(xml_days)) {
    processed_xml_dates <- as.Date(character())
    for (raw_day in xml_days) {
      day_date <- as.Date(raw_day, origin = "1970-01-01")
      raw_exists <- file.exists(.brf_bvbg_raw_path(day_date, create = FALSE))
      parsed <- .brf_bvbg_ensure_parsed_day(day_date, quiet = quiet)
      if (isTRUE(attr(parsed, "brf_no_data"))) {
      if (isTRUE(attr(parsed, "brf_download_failed"))) {
        if (!quiet) {
          message("BVBG: unable to download XML for ", format(day_date, "%Y-%m-%d"))
        }
        .brf_register_no_data_zip("ALL", day_date, quiet = quiet)
        skip_dates_zip <- unique(c(skip_dates_zip, day_date))
        skip_dates <- unique(c(skip_dates, day_date))
        refresh_dates <- unique(c(refresh_dates, day_date))
      } else {
        .brf_register_no_data_xml("ALL", day_date, quiet = quiet)
        skip_dates_xml <- unique(c(skip_dates_xml, day_date))
        skip_dates <- unique(c(skip_dates, day_date))
          refresh_dates <- unique(c(refresh_dates, day_date))
        }
        next
      }
      if (!raw_exists && file.exists(.brf_bvbg_raw_path(day_date, create = FALSE))) {
        xml_downloaded <- c(xml_downloaded, as.character(day_date))
      }
      processed_xml_dates <- unique(c(processed_xml_dates, day_date))
      root_subset <- .brf_bvbg_filter_root_shared(parsed, root)
      if (!isTRUE(attr(root_subset, "brf_no_data")) && nrow(root_subset)) {
        parsed_updates[[length(parsed_updates) + 1L]] <- .brf_parsed_strip_attrs(root_subset)
      }
    }
    if (length(processed_xml_dates)) {
      refresh_dates <- unique(c(refresh_dates, processed_xml_dates))
    }
  }
  if (!quiet && length(c(newly_downloaded, xml_downloaded))) {
    message("Root ", root, ": downloaded ", length(c(newly_downloaded, xml_downloaded)), " report(s).")
  }
  parsed_needed <- newly_downloaded
  data_path <- .brf_root_data_path(root, create = FALSE)
  existing_parsed_files <- .brf_list_parsed_files(root)
  if (!file.exists(data_path) || !length(existing_parsed_files)) {
    parsed_needed <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE, ignore.case = TRUE)
  }
  parsed_needed <- unique(c(parsed_needed, preexisting_needed))
  parsed_needed <- parsed_needed[file.exists(parsed_needed)]
  if (length(skip_files_html)) {
    parsed_needed <- parsed_needed[!(basename(parsed_needed) %in% skip_files_html)]
  }
  if (length(parsed_needed)) {
    for (path in parsed_needed) {
      ensure_path(path, force = path %in% preexisting_needed)
    }
  }
  refresh_dates <- unique(c(refresh_dates, skip_dates))
  refresh_dates <- refresh_dates[!is.na(refresh_dates)]
  if (nrow(current)) {
    current$date <- as.Date(current$date)
  }
  if (length(refresh_dates) && nrow(current)) {
    current <- current[!(current$date %in% refresh_dates), , drop = FALSE]
  }
  additions <- if (length(parsed_updates)) {
    out <- .brf_bind_rows(parsed_updates)
    if (nrow(out)) {
      out$date <- as.Date(out$date)
    }
    out
  } else {
    .brf_empty_bulletin()
  }
  combined_sources <- list()
  if (nrow(current)) combined_sources[[length(combined_sources) + 1L]] <- current
  if (nrow(additions)) combined_sources[[length(combined_sources) + 1L]] <- additions
  combined <- if (length(combined_sources)) {
    .brf_bind_rows(combined_sources)
  } else {
    .brf_empty_bulletin()
  }
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
  .brf_parsed_dir(root)
  if (!dir.exists(raw_dir)) {
    combined <- .brf_prepare_root_data(root, .brf_empty_bulletin())
    .brf_save_root_data(root, combined)
    if (!quiet) {
      message("Root ", root, ": rebuilt cache with ", nrow(combined), " rows.")
    }
    return(invisible(combined))
  }
  html_files <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE, ignore.case = TRUE)
  xml_years <- .brf_bvbg_list_years()
  if (!length(html_files) && !length(xml_years)) {
    combined <- .brf_prepare_root_data(root, .brf_empty_bulletin())
    .brf_save_root_data(root, combined)
    if (!quiet) {
      message("Root ", root, ": rebuilt cache with ", nrow(combined), " rows.")
    }
    return(invisible(combined))
  }
  removed <- if (length(html_files)) {
    .brf_handle_no_data_paths(html_files, root, quiet = quiet)
  } else {
    .brf_no_data_empty()
  }
  if (is.data.frame(removed) && nrow(removed)) {
    for (date_val in removed$date[!is.na(removed$date)]) {
      .brf_remove_parsed_day(root, date_val)
    }
  }
  html_files <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE, ignore.case = TRUE)
  xml_years <- .brf_bvbg_list_years()
  if (!length(html_files) && !length(xml_years)) {
    combined <- .brf_prepare_root_data(root, .brf_empty_bulletin())
    .brf_save_root_data(root, combined)
    if (!quiet) {
      message("Root ", root, ": rebuilt cache with ", nrow(combined), " rows.")
    }
    return(invisible(combined))
  }
  register_no_data_html <- function(paths) {
    paths <- unique(paths)
    if (!length(paths)) {
      return()
    }
    info <- .brf_handle_no_data_paths(paths, root, quiet = quiet)
    if (is.data.frame(info) && nrow(info)) {
      for (date_val in info$date[!is.na(info$date)]) {
        .brf_remove_parsed_day(root, date_val)
      }
    }
  }
  for (path in html_files) {
    .brf_sync_parsed_report(path, root, register_no_data_html, force = FALSE, quiet = quiet, source = "html")
  }
  skip_dates_html <- .brf_no_data_entries(root, source = "html")$date
  if (length(skip_dates_html)) {
    skip_dates_html <- skip_dates_html[!is.na(skip_dates_html) & skip_dates_html < .brf_xml_cutover_date()]
  }
  parsed_frames <- .brf_collect_parsed_data(root, skip_dates = skip_dates_html)
  if (length(parsed_frames)) {
    parsed_frames <- lapply(parsed_frames, function(df) {
      df$date <- as.Date(df$date)
      df[df$date < .brf_xml_cutover_date(), , drop = FALSE]
    })
    parsed_frames <- Filter(function(df) nrow(df) > 0, parsed_frames)
  }
  xml_frames <- list()
  if (length(xml_years)) {
    for (year in xml_years) {
      year_data <- .brf_bvbg_year_data(year, quiet = quiet)
      if (!inherits(year_data, "data.frame") || !nrow(year_data)) {
        next
      }
      year_data$date <- as.Date(year_data$date)
      year_data <- year_data[year_data$date >= .brf_xml_cutover_date(), , drop = FALSE]
      if (!nrow(year_data)) {
        next
      }
      root_subset <- .brf_bvbg_filter_root_shared(year_data, root)
      if (!isTRUE(attr(root_subset, "brf_no_data")) && nrow(root_subset)) {
        xml_frames[[length(xml_frames) + 1L]] <- root_subset
      }
    }
  }
  combined_sources <- list()
  if (length(parsed_frames)) combined_sources <- c(combined_sources, parsed_frames)
  if (length(xml_frames)) combined_sources <- c(combined_sources, xml_frames)
  combined <- if (length(combined_sources)) {
    .brf_bind_rows(combined_sources)
  } else {
    .brf_empty_bulletin()
  }
  combined <- .brf_prepare_root_data(root, combined, skip_dates = skip_dates_html)
  .brf_save_root_data(root, combined)
  if (!quiet) {
    message("Root ", root, ": rebuilt cache with ", nrow(combined), " rows.")
  }
  invisible(combined)
}

#' Rebuild cached root and aggregate data
#'
#' Rebuilds per-root caches from cached HTML bulletins and complete PR/BVBG.086
#' futures rows (including compact yearly BVBG caches) before refreshing the
#' aggregate store.
#'
#' @param root Optional character vector with roots to target. When omitted
#'   and `rebuild_roots` is `TRUE`, every cached root is rebuilt from the raw
#'   HTML files and retained PR snapshots/parsed rows.
#' @param all When `TRUE`, refreshes the aggregate cache after the optional root
#'   rebuilds. Defaults to `TRUE`.
#' @param rebuild_roots Controls whether root caches are rebuilt from the raw
#'   HTML files and retained PR snapshots/parsed rows. When `NULL` (default),
#'   caches are rebuilt only when specific `root` values are supplied. Set to
#'   `TRUE` or `FALSE` to override this behaviour.
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
#' Returns data combined from HTML and BVBG XML sources; the `source` column
#' indicates the origin (`html` or `xml`) before treatments are applied.
#'
#' @param ticker Character vector with specific contract tickers (e.g. `"WINZ24"`).
#' @param start,end Optional bounds restricting the returned dates.
#' @param treatment Either the name of a built-in treatment (e.g. `"raw"`,
#'   `"standard"`, `"ohlcv_xts"`) or a function that receives the raw data frame
#'   and returns the desired shape.
#' @param add_attrs When `TRUE` (default), attach futures metadata to the result.
#' @param rebuild_agg Set to `TRUE` to rebuild aggregates before retrieving
#'   data. The relevant root caches are rebuilt from the raw HTML/XML files when
#'   necessary.
#' @param tz Timezone used when returning `xts` objects. Defaults to
#'   `"America/Sao_Paulo"`.
#' @param keep_time When `TRUE` (default), keep the clock time when assigning the
#'   timezone (e.g. midnight stays midnight). When `FALSE`, shift timestamps to
#'   the target timezone.
#' @param ... Additional arguments forwarded to the treatment function.
#'
#' @return The result of applying `treatment` to the filtered bulletin rows.
#' @export
get_brfut <- function(ticker,
                      start = NULL,
                      end = NULL,
                      treatment = "ohlcv_drop0_xts",
                      add_attrs = TRUE,
                      rebuild_agg = FALSE,
                      tz = "America/Sao_Paulo",
                      keep_time = TRUE,
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
  finish <- treatment_fn(data, ...)
  estimated <- .brf_estimate_maturity(finish)
  if (add_attrs) estimated <- .brf_add_futures_attrs(estimated, ticker)

  # estimated <- if (xts::is.xts(estimated)) {
  #   xts::xts(zoo::coredata(estimated),
  #     order.by = lubridate::force_tz(zoo::index(estimated), tz)
  #   )
  # } else {
  #   estimated
  # }

  if (xts::is.xts(estimated)) {
    estimated <- .brf_xts_apply_timezone(estimated, tz = tz, keep_time = keep_time)
  }
  return(estimated)
}

#' Load all cached bulletins within a date range
#'
#' Returns cached rows merged from HTML and BVBG XML sources; the `source`
#' column indicates the origin (`html` or `xml`) before treatments are applied.
#'
#' @param start,end Date bounds. When omitted all cached rows are returned.
#' @param root Optional character vector restricting the returned roots. When
#'   `NULL`, rows from every cached root are included.
#' @param treatment Either the name of a built-in treatment (e.g. `"standard"`,
#'   `"regular"`, `"raw"`) or a function receiving the assembled aggregate data
#'   frame and returning the desired shape. Defaults to `"clean_data"`, which
#'   removes redundant bulletin columns, converts localized numeric strings to
#'   numbers, and renames selected columns to clearer aliases (e.g.
#'   `"preco_abert"` -> `"open"`). Use `"clean_data_drop0"` to additionally drop
#'   rows where any key OHLC/V fields are zero or `NA`.
#' @param rebuild_agg When `TRUE`, rebuilds the cached aggregates from the
#'   latest root files before loading. Defaults to `FALSE`.
#'
#' @return A data frame with every cached contract observation within the range.
#' @export
get_brfut_agg <- function(start = NULL,
                          end = NULL,
                          root = NULL,
                          treatment = "clean_data",
                          rebuild_agg = FALSE) {
  filter_roots <- .brf_normalize_root_vector(root)
  if (isTRUE(rebuild_agg) || !file.exists(.brf_aggregate_path(create = FALSE))) {
    update_brfut_agg(
      root = if (length(filter_roots)) filter_roots else NULL,
      all = TRUE,
      rebuild_roots = FALSE,
      quiet = TRUE
    )
  }
  data <- .brf_load_aggregate()
  if (!nrow(data)) {
    stop("Aggregate cache is empty. Run update_brfut() first.", call. = FALSE)
  }
  if (length(filter_roots)) {
    data <- data[data$root %in% filter_roots, , drop = FALSE]
  }
  bounds <- .brf_normalize_date_bounds(start, end)
  data$date <- as.Date(data$date)
  treatment_key <- if (is.character(treatment) && length(treatment)) {
    tolower(treatment[[1L]])
  } else {
    NA_character_
  }
  needs_di_prior_context <- !is.na(treatment_key) &&
    treatment_key %in% c("di_adjustments", "di_adjustments_tibble") &&
    !is.null(bounds$start)
  if (nrow(data)) {
    from <- if (is.null(bounds$start)) min(data$date) else bounds$start
    if (needs_di_prior_context) {
      from <- tryCatch(
        bizdays::add.bizdays(
          bounds$start,
          -1L,
          .brf_di_resolve_session_calendar()
        ),
        error = function(e) bounds$start - 7L
      )
    }
    to <- bounds$end
    data <- data[data$date >= from & data$date <= to, , drop = FALSE]
  }
  data <- .brf_normalize_old_tickers(data)
  if (nrow(data)) {
    data <- data[order(data$date, data$root, data$ticker), , drop = FALSE]
  }
  treatment_fn <- .brf_resolve_agg_treatment(treatment)
  result <- treatment_fn(data)
  result <- .brf_estimate_maturity(result)
  if (needs_di_prior_context && is.data.frame(result) && "date" %in% names(result)) {
    result_date <- suppressWarnings(as.Date(result$date))
    result <- result[result_date >= bounds$start, , drop = FALSE]
  }
  result
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}
