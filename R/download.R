#' Build the download URL for a B3 futures bulletin
#'
#' @param date Date of the trading session (Date or character).
#' @param ticker_root Commodity symbol root (e.g. `"IND"`, `"DI1"`).
#' @param format Requested format, either `"xls"` or `"html"`.
#'
#' @return A character scalar with the encoded URL.
#' @export
bmf_bulletin_url <- function(date,
                             ticker_root = "IND",
                             format = c("xls", "html")) {
  date <- .normalize_bmf_date(date)
  format <- match.arg(format)
  ticker_vec <- .bmf_normalize_ticker_root(ticker_root)
  ticker_vec <- ticker_vec[!is.na(ticker_vec)]
  if (!length(ticker_vec)) {
    stop("ticker_root cannot be missing or empty.", call. = FALSE)
  }
  ticker_root <- ticker_vec[1L]
  if (format == "xls") {
    base_url <- "https://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/SistemaPregao_excel1.asp"
    query <- paste0(
      "Data=", .bmf_format_date_for_query(date),
      "&Mercadoria=", utils::URLencode(ticker_root, reserved = TRUE),
      "&XLS=true"
    )
  } else {
    base_url <- "https://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/SistemaPregao1.asp"
    encoded_caminho <- "Resumo%20Estat%EDstico%20-%20Sistema%20Preg%E3o"
    query <- paste0(
      "pagetype=pop",
      "&caminho=", encoded_caminho,
      "&Data=", .bmf_format_date_for_query(date),
      "&Mercadoria=", utils::URLencode(ticker_root, reserved = TRUE)
    )
  }
  paste(base_url, query, sep = "?")
}

#' Download and save a single B3 daily report
#'
#' @param date Date of the trading session (Date or character).
#' @param ticker_root Commodity symbol root (e.g. `"IND"`, `"DI1"`).
#' @param dest_dir Directory where the report will be stored.
#' @param overwrite Whether to overwrite an existing file.
#' @param quiet If `TRUE`, suppress informative messages.
#' @param format Report format. `"xls"` downloads the Excel bulletin (default)
#'   while `"html"` retrieves the legacy web version.
#' @param which Storage location passed to `tools::R_user_dir` when `dest_dir`
#'   is `NULL`. Defaults to the package cache directory.
#'
#' @return Invisibly returns the path to the saved file.
#' @export
download_bmf_report <- function(date,
                                ticker_root = "IND",
                                dest_dir = NULL,
                                overwrite = FALSE,
                                quiet = FALSE,
                                format = c("xls", "html"),
                                which = c("cache", "data", "config")) {
  ticker_vec <- .bmf_normalize_ticker_root(ticker_root)
  ticker_vec <- ticker_vec[!is.na(ticker_vec)]
  if (!length(ticker_vec)) {
    stop("ticker_root cannot be missing or empty.", call. = FALSE)
  }
  ticker_root_norm <- ticker_vec[1L]
  date <- .normalize_bmf_date(date)
  format <- match.arg(format)
  which <- match.arg(which)
  if (is.null(dest_dir)) {
    dest_dir <- .bmf_storage_dir(ticker_root_norm, which = which)
  } else if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  base_name <- sprintf("%s_%s", ticker_root_norm, format(date, "%Y-%m-%d"))
  existing_paths <- file.path(dest_dir, paste0(base_name, c(".xls", ".html")))
  existing_paths <- existing_paths[file.exists(existing_paths)]
  if (!overwrite && length(existing_paths)) {
    existing_path <- normalizePath(existing_paths[1L], winslash = "/", mustWork = FALSE)
    if (!quiet) {
      message("Report already exists at ", existing_path)
    }
    return(invisible(existing_path))
  }
  expected_extension <- if (format == "xls") ".xls" else ".html"
  dest_file <- file.path(dest_dir, paste0(base_name, expected_extension))
  url <- bmf_bulletin_url(date, ticker_root = ticker_root_norm, format = format)
  resp <- httr::RETRY(
    "GET",
    url,
    httr::user_agent("brfutures package (https://github.com/)"),
    times = 3,
    pause_base = 1,
    terminate_on = c(400, 401, 403, 404)
  )
  httr::stop_for_status(resp, task = sprintf("download %s report for %s", ticker_root_norm, format(date, "%Y-%m-%d")))
  content <- httr::content(resp, as = "raw")
  writeBin(content, dest_file)
  actual_format <- .bmf_detect_report_format(dest_file)
  actual_extension <- if (identical(actual_format, "xls")) ".xls" else ".html"
  if (!identical(actual_extension, expected_extension)) {
    new_path <- file.path(dest_dir, paste0(base_name, actual_extension))
    if (file.exists(new_path)) {
      if (!overwrite) {
        unlink(dest_file)
        stop("Report already exists at ", new_path, "; use overwrite = TRUE to replace it.", call. = FALSE)
      }
      unlink(new_path)
    }
    if (!file.rename(dest_file, new_path)) {
      stop("Failed to move downloaded report to ", new_path, call. = FALSE)
    }
    dest_file <- new_path
    format <- actual_format
    expected_extension <- actual_extension
  }
  if (!quiet) {
    if (identical(format, "xls")) {
      message("Saved ", ticker_root_norm, " report to ", dest_file)
    } else {
      message("Saved ", ticker_root_norm, " report to ", dest_file, " (HTML fallback used)")
    }
  }
  dest_file <- normalizePath(dest_file, winslash = "/", mustWork = TRUE)
  invisible(dest_file)
}

#' Parse a B3 bulletin into a tidy data frame
#'
#' @param path Path to the saved report file (HTML or XLS).
#' @param ticker_root Commodity symbol root (optional, inferred from filename if
#'   `NULL`).
#' @param report_date Optional date override; if `NULL`, the function will try to
#'   infer the date from the file contents or file name.
#' @param format Report format, inferred automatically when `NULL`.
#'
#' @return A data frame with the parsed bulletin for all contract maturities.
#' @export
parse_bmf_report <- function(path,
                             ticker_root = NULL,
                             report_date = NULL,
                             format = NULL) {
  if (!file.exists(path)) {
    stop("File '", path, "' does not exist.", call. = FALSE)
  }
  file_info <- file.info(path)
  if (!is.na(file_info$size) && file_info$size == 0L) {
    stop("Report '", path, "' is empty.", call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  detected <- .bmf_detect_report_format(path)
  if (!detected %in% c("xls", "html")) {
    detected <- "xls"
  }
  if (is.null(ticker_root) || all(is.na(ticker_root)) || !any(nzchar(trimws(as.character(ticker_root))))) {
    ticker_root <- .bmf_infer_ticker_from_path(path)
  }
  normalized <- .bmf_normalize_ticker_root(ticker_root)
  normalized <- normalized[!is.na(normalized)]
  ticker_root <- if (length(normalized)) normalized[1L] else NA_character_
  if (is.null(format)) {
    format <- detected
  } else {
    format <- match.arg(format, choices = c("xls", "html"))
    if (!identical(format, detected)) {
      message("Detected report format '", detected, "' differs from requested '", format, "'. Using detected format.")
      format <- detected
    }
  }
  data <- switch(
    format,
    xls = .parse_bmf_report_xls(path, ticker_root = ticker_root, report_date = report_date),
    html = .parse_bmf_report_html(path, ticker_root = ticker_root, report_date = report_date)
  )
  if (!"ticker_root" %in% names(data)) {
    data$ticker_root <- ticker_root
  } else if (!is.na(ticker_root)) {
    data$ticker_root[] <- ticker_root
  }
  if (!"mercadoria" %in% names(data)) {
    data$mercadoria <- data$ticker_root
  } else {
    data$mercadoria[] <- data$ticker_root
  }
  priority <- c("date", "ticker_root", "mercadoria", "contract_code")
  other_cols <- setdiff(names(data), priority)
  data <- data[, c(priority, other_cols), drop = FALSE]
  rownames(data) <- NULL
  data
}

.parse_bmf_report_html <- function(path, ticker_root, report_date) {
  doc <- xml2::read_html(path, encoding = "windows-1252")
  resolved_date <- report_date
  if (is.null(resolved_date)) {
    resolved_date <- .bmf_guess_report_date(path, doc)
  }
  if (!is.null(resolved_date) && !is.na(resolved_date)) {
    resolved_date <- .normalize_bmf_date(resolved_date)
  } else {
    resolved_date <- NA
  }
  if (.bmf_report_has_no_data_message(doc)) {
    empty <- .bmf_empty_bulletin_dataframe()
    attr(empty, "report_date") <- resolved_date
    attr(empty, "ticker_root") <- ticker_root
    attr(empty, "mercadoria") <- ticker_root
    attr(empty, "source") <- path
    attr(empty, "reason") <- "no_data"
    message("Report ", basename(path), " contains no data. Returning empty result.")
    return(empty)
  }
  tables <- tryCatch(
    list(
      vencto = .bmf_extract_table(doc, "MercadoFut0"),
      volume = .bmf_extract_table(doc, "MercadoFut1"),
      prices = .bmf_extract_table(doc, "MercadoFut2")
    ),
    error = function(err) {
      msg <- conditionMessage(err)
      if (grepl("was not found in the report", msg, fixed = TRUE)) {
        fallback <- .bmf_extract_full_bulletin_table(doc)
        if (!is.null(fallback)) {
          return(list(fallback = fallback))
        }
      }
      list(error = err)
    }
  )
  if (!is.null(tables$error)) {
    empty <- .bmf_empty_bulletin_dataframe()
    attr(empty, "report_date") <- resolved_date
    attr(empty, "ticker_root") <- ticker_root
    attr(empty, "mercadoria") <- ticker_root
    attr(empty, "source") <- path
    attr(empty, "reason") <- "missing_tables"
    message("Report ", basename(path), " is missing the expected tables. Returning empty result.")
    return(empty)
  }
  if (!is.null(tables$fallback)) {
    data <- tables$fallback
  } else {
    if (is.null(tables$vencto) || is.null(tables$volume) || is.null(tables$prices)) {
      empty <- .bmf_empty_bulletin_dataframe()
      attr(empty, "report_date") <- resolved_date
      attr(empty, "ticker_root") <- ticker_root
      attr(empty, "mercadoria") <- ticker_root
      attr(empty, "source") <- path
      attr(empty, "reason") <- "incomplete_tables"
      message("Report ", basename(path), " returned incomplete tables. Returning empty result.")
      return(empty)
    }
    data <- data.frame(
      tables$vencto,
      tables$volume,
      tables$prices,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  colnames(data) <- .bmf_rename_bulletin_columns(colnames(data))
  if (!"contract_code" %in% names(data)) {
    stop("Parsed data is missing the contract_code column.", call. = FALSE)
  }
  numeric_cols <- setdiff(names(data), "contract_code")
  data[numeric_cols] <- lapply(data[numeric_cols], .bmf_parse_ptbr_number)
  data$contract_code <- trimws(as.character(data$contract_code))
  data <- data[nzchar(data$contract_code), , drop = FALSE]
  data$ticker_root <- ticker_root
  data$mercadoria <- ticker_root
  data$date <- resolved_date
  data <- data[, c("date", "ticker_root", "mercadoria", "contract_code", setdiff(names(data), c("date", "ticker_root", "mercadoria", "contract_code")))]
  rownames(data) <- NULL
  data
}

.parse_bmf_report_xls <- function(path, ticker_root, report_date) {
  sheets <- readxl::excel_sheets(path)
  if (!length(sheets)) {
    stop("Workbook in '", path, "' does not contain any sheets.", call. = FALSE)
  }
  extract_candidate <- function(sheet) {
    raw <- readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      .name_repair = "minimal"
    )
    if (!nrow(raw)) {
      return(NULL)
    }
    raw <- as.data.frame(raw, stringsAsFactors = FALSE)
    raw[] <- lapply(raw, function(col) {
      if (inherits(col, "POSIXct") || inherits(col, "Date")) {
        format(col, "%d/%m/%Y")
      } else if (inherits(col, "numeric")) {
        as.character(col)
      } else {
        as.character(col)
      }
    })
    header_idx <- which(apply(raw, 1, function(row) any(grepl("VENCTO", row, ignore.case = TRUE))))
    if (!length(header_idx)) {
      return(NULL)
    }
    header_row <- header_idx[1L]
    header <- raw[header_row, , drop = TRUE]
    header <- trimws(header)
    header <- iconv(header, from = "UTF-8", to = "ASCII//TRANSLIT")
    if (!any(nzchar(header))) {
      return(NULL)
    }
    data <- raw[-seq_len(header_row), , drop = FALSE]
    if (!nrow(data)) {
      return(NULL)
    }
    colnames(data) <- header
    data <- data[, nzchar(colnames(data)), drop = FALSE]
    if (!ncol(data)) {
      return(NULL)
    }
    data[] <- lapply(data, trimws)
    data
  }
  candidates <- lapply(sheets, extract_candidate)
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) {
    stop("Could not locate the bulletin table inside '", path, "'.", call. = FALSE)
  }
  data <- candidates[[1L]]
  all_cells <- unlist(candidates, use.names = FALSE)
  all_cells_chr <- vapply(all_cells, function(value) {
    if (is.na(value)) {
      ""
    } else {
      as.character(value)
    }
  }, character(1), USE.NAMES = FALSE)
  if (any(grepl("Nao ha dados", all_cells_chr, ignore.case = TRUE))) {
    resolved_date <- report_date
    if (is.null(resolved_date) || is.na(resolved_date)) {
      resolved_date <- .bmf_extract_date_from_filename(path)
    }
    if (!is.null(resolved_date) && !is.na(resolved_date)) {
      resolved_date <- .normalize_bmf_date(resolved_date)
    } else {
      resolved_date <- NA
    }
    empty <- .bmf_empty_bulletin_dataframe()
    attr(empty, "report_date") <- resolved_date
    attr(empty, "ticker_root") <- ticker_root
    attr(empty, "mercadoria") <- ticker_root
    attr(empty, "source") <- path
    message("Report ", basename(path), " contains no data. Returning empty result.")
    return(empty)
  }
  data <- data[nzchar(data[[1L]]), , drop = FALSE]
  colnames(data) <- .bmf_rename_bulletin_columns(colnames(data))
  if (!"contract_code" %in% names(data)) {
    stop("Parsed Excel data is missing the contract_code column.", call. = FALSE)
  }
  numeric_cols <- setdiff(names(data), "contract_code")
  data[numeric_cols] <- lapply(data[numeric_cols], .bmf_parse_ptbr_number)
  data$contract_code <- trimws(as.character(data$contract_code))
  data <- data[nzchar(data$contract_code), , drop = FALSE]
  inferred_date <- report_date
  if (is.null(inferred_date)) {
    dates <- vapply(all_cells_chr, function(text) {
      if (!nzchar(text)) {
        as.Date(NA)
      } else {
        .bmf_extract_date_from_text(text)
      }
    }, as.Date(NA))
    inferred_date <- dates[!is.na(dates)][1L]
  }
  if (is.null(inferred_date) || is.na(inferred_date)) {
    inferred_date <- .bmf_extract_date_from_filename(path)
  }
  if (!is.null(inferred_date) && !is.na(inferred_date)) {
    inferred_date <- .normalize_bmf_date(inferred_date)
  } else {
    inferred_date <- NA
  }
  data$date <- inferred_date
  data$ticker_root <- ticker_root
  data$mercadoria <- ticker_root
  data <- data[, c("date", "ticker_root", "mercadoria", "contract_code", setdiff(names(data), c("date", "ticker_root", "mercadoria", "contract_code")))]
  rownames(data) <- NULL
  data
}

#' Collect parsed bulletin data for a ticker root
#'
#' @param ticker_root Commodity symbol root.
#' @param data_dir Directory containing saved bulletin files for the ticker.
#' @param drop_empty If `TRUE`, drop reports with zero rows.
#' @param which Storage location passed to `tools::R_user_dir` when `data_dir`
#'   is `NULL`.
#'
#' @return A data frame combining all parsed bulletins.
#' @export
bmf_collect_contracts <- function(ticker_root,
                                  data_dir = NULL,
                                  drop_empty = TRUE,
                                  which = c("cache", "data", "config")) {
  normalized <- .bmf_normalize_ticker_root(ticker_root)
  normalized <- normalized[!is.na(normalized)]
  if (!length(normalized)) {
    stop("ticker_root cannot be missing or empty.", call. = FALSE)
  }
  ticker_root_norm <- normalized[1L]
  which <- match.arg(which)
  if (is.null(data_dir)) {
    data_dir <- .bmf_storage_dir(ticker_root_norm, which = which)
  }
  if (!dir.exists(data_dir)) {
    stop("Directory '", data_dir, "' does not exist.", call. = FALSE)
  }
  files <- list.files(
    data_dir,
    pattern = paste0("^", ticker_root_norm, "_.*\\.(html?|xls[x]?)$"),
    full.names = TRUE
  )
  files <- sort(files)
  if (!length(files)) {
    return(.bmf_empty_bulletin_dataframe())
  }
  reason_dates <- as.Date(character())
  parsed <- lapply(files, function(path) {
    inferred_date <- .bmf_extract_date_from_filename(path)
    df <- parse_bmf_report(path, ticker_root = ticker_root_norm, report_date = inferred_date)
    if (drop_empty && !nrow(df)) {
      reason <- attr(df, "reason")
      if (!is.null(reason) && reason %in% c("no_data", "missing_tables", "incomplete_tables")) {
        if (file.exists(path)) {
          unlink(path)
        }
        reason_dates <<- unique(c(reason_dates, attr(df, "report_date")))
        return(NULL)
      }
      return(NULL)
    }
    df
  })
  parsed <- Filter(Negate(is.null), parsed)
  if (length(reason_dates)) {
    existing_skip <- .bmf_read_no_data_dates(ticker_root_norm, which)
    .bmf_write_no_data_dates(ticker_root_norm, which, unique(c(existing_skip, reason_dates)))
  }
  if (!length(parsed)) {
    return(.bmf_empty_bulletin_dataframe())
  }
  combined <- do.call(rbind, parsed)
  combined <- combined[order(combined$contract_code, combined$date), , drop = FALSE]
  combined$ticker <- paste0(combined$ticker_root, combined$contract_code)
  combined <- combined[, c("date", "ticker_root", "mercadoria", "contract_code", "ticker", setdiff(names(combined), c("date", "ticker_root", "mercadoria", "contract_code", "ticker"))), drop = FALSE]
  rownames(combined) <- NULL
  attr(combined, "source_files") <- files
  combined
}

#' Build xts time series for a ticker root
#'
#' @param ticker_root Commodity symbol root.
#' @param data_dir Directory containing saved bulletin files.
#' @param out_dir Directory where the xts objects will be saved (RDS files).
#' @param save_series If `TRUE`, persist each xts object as an `.rds` file.
#' @param add_agg If `TRUE`, also persist an aggregated data file containing all
#'   contracts with a `ticker` column for downstream merges.
#' @param which Storage location passed to `tools::R_user_dir` when directories
#'   are inferred.
#' @param verbose If `TRUE`, emit progress messages while building the series.
#' @param return Controls what is returned. `"list"` (default) returns the
#'   per-contract xts objects; `"agg"` returns the aggregated data frame.
#'
#' @return A named list of xts objects; the list has an attribute `files` with
#'   the paths of the saved series (if `save_series = TRUE`).
#' @export
bmf_build_contract_series <- function(ticker_root,
                                      data_dir = NULL,
                                      out_dir = NULL,
                                      save_series = TRUE,
                                      add_agg = FALSE,
                                      which = c("cache", "data", "config"),
                                      verbose = TRUE,
                                      estimate_maturity = FALSE,
                                      return = c("list", "agg")) {
  normalized <- .bmf_normalize_ticker_root(ticker_root)
  normalized <- normalized[!is.na(normalized)]
  if (!length(normalized)) {
    stop("ticker_root cannot be missing or empty.", call. = FALSE)
  }
  ticker_root_norm <- normalized[1L]
  which <- match.arg(which)
  return <- match.arg(return)
  if (is.null(data_dir)) {
    data_dir <- .bmf_storage_dir(ticker_root_norm, which = which)
  }
  if (is.null(out_dir)) {
    out_dir <- file.path(data_dir, "xts")
  }
  combined <- bmf_collect_contracts(
    ticker_root = ticker_root_norm,
    data_dir = data_dir,
    drop_empty = TRUE,
    which = which
  )
  if (!nrow(combined)) {
    stop("Parsed data does not contain valid rows for ticker ", ticker_root_norm, ".", call. = FALSE)
  }
  if (!"ticker" %in% names(combined)) {
    combined$ticker <- paste0(combined$ticker_root, combined$contract_code)
    combined <- combined[, c("date", "ticker_root", "mercadoria", "contract_code", "ticker", setdiff(names(combined), c("date", "ticker_root", "mercadoria", "contract_code", "ticker"))), drop = FALSE]
  }
  if (isTRUE(estimate_maturity)) {
    calendar_name <- .bmf_get_calendar()
    combined$estimated_maturity <- vapply(
      combined$ticker,
      function(sym) .bmf_estimate_maturity(sym, calendar_name = calendar_name),
      FUN.VALUE = as.Date(NA)
    )
    if (!inherits(combined$estimated_maturity, "Date")) {
      if (is.numeric(combined$estimated_maturity)) {
        combined$estimated_maturity <- as.Date(combined$estimated_maturity, origin = "1970-01-01")
      } else {
        combined$estimated_maturity <- as.Date(combined$estimated_maturity)
      }
    }
  }
  if (verbose) {
    message("Building series for ", ticker_root_norm, " from ", nrow(combined), " rows")
  }
  split_data <- split(combined, combined$contract_code)
  if ((save_series || add_agg) && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  series_list <- list()
  saved_paths <- character()
  for (code in names(split_data)) {
    df <- split_data[[code]]
    df <- df[order(df$date), ]
    df <- df[!duplicated(df$date, fromLast = TRUE), , drop = FALSE]
    value_cols <- setdiff(names(df), c("date", "ticker_root", "mercadoria", "contract_code", "ticker", "estimated_maturity"))
    numeric_cols <- value_cols[sapply(df[value_cols], function(col) is.numeric(col) || is.integer(col))]
    if (!length(numeric_cols)) {
      next
    }
    xts_obj <- xts::xts(as.matrix(df[numeric_cols]), order.by = df$date)
    colnames(xts_obj) <- numeric_cols
    series_name <- paste0(ticker_root_norm, code)
    series_list[[series_name]] <- xts_obj
    if (save_series) {
      file_path <- file.path(out_dir, paste0(series_name, ".rds"))
      saveRDS(xts_obj, file_path)
      saved_paths <- c(saved_paths, file_path)
      if (verbose) {
        message("  saved ", series_name, " -> ", normalizePath(file_path, winslash = "/", mustWork = TRUE))
      }
    }
  }
  attr(series_list, "files") <- saved_paths
  data_attr <- combined
  if (identical(return, "list")) {
    data_attr <- combined[, setdiff(names(combined), c("ticker", "ticker_root")), drop = FALSE]
  }
  attr(series_list, "data") <- data_attr
  if (isTRUE(add_agg)) {
    aggregate_data <- combined
    aggregate_data <- aggregate_data[, c("ticker", setdiff(names(aggregate_data), "ticker")), drop = FALSE]
    aggregate_path <- file.path(out_dir, paste0(ticker_root_norm, "_aggregate.rds"))
    saveRDS(aggregate_data, aggregate_path)
    aggregate_path <- normalizePath(aggregate_path, winslash = "/", mustWork = TRUE)
    attr(series_list, "aggregate_file") <- aggregate_path
    attr(series_list, "aggregate_data") <- aggregate_data
    if (verbose) {
      message("Aggregate saved -> ", aggregate_path)
    }
    if (identical(return, "agg")) {
      return(aggregate_data)
    }
  }
  if (identical(return, "agg")) {
    warning("Aggregate requested via return = 'agg' but add_agg is FALSE; returning empty data frame.", call. = FALSE)
    return(.bmf_empty_bulletin_dataframe())
  }
  series_list
}

#' Download a range of bulletins for a ticker root
#'
#' @param ticker_root Commodity symbol root.
#' @param start_date First date to download (inclusive).
#' @param end_date Last date to download (inclusive), defaults to today.
#' @param dest_dir Directory where reports will be stored.
#' @param format Preferred format (`"xls"` or `"html"`).
#' @param overwrite If `TRUE`, redownloads even when files already exist.
#' @param quiet If `TRUE`, suppress download messages.
#' @param skip_existing If `TRUE`, skip dates that already have a saved report.
#' @param which Storage location passed to `tools::R_user_dir` when `dest_dir`
#'   is `NULL`.
#'
#' @return Invisibly returns a data frame summarising download results.
#' @export
bmf_download_history <- function(ticker_root,
                                 start_date,
                                 end_date = Sys.Date(),
                                 dest_dir = NULL,
                                 format = c("xls", "html"),
                                 overwrite = FALSE,
                                 quiet = TRUE,
                                 skip_existing = TRUE,
                                 which = c("cache", "data", "config"),
                                 verbose = TRUE) {
  normalized <- .bmf_normalize_ticker_root(ticker_root)
  normalized <- normalized[!is.na(normalized)]
  if (!length(normalized)) {
    stop("ticker_root cannot be missing or empty.", call. = FALSE)
  }
  ticker_root_norm <- normalized[1L]
  which <- match.arg(which)
  if (is.null(dest_dir)) {
    dest_dir <- .bmf_storage_dir(ticker_root_norm, which = which)
  }
  start_date <- .normalize_bmf_date(start_date)
  end_date <- .normalize_bmf_date(end_date)
  if (start_date > end_date) {
    stop("start_date must be before or equal to end_date.", call. = FALSE)
  }
  format <- match.arg(format)
  if (overwrite) {
    skip_existing <- FALSE
  }
  no_data_dates <- .bmf_read_no_data_dates(ticker_root_norm, which)
  # clean pre-existing files with no data message
  existing_files <- list.files(
    dest_dir,
    pattern = paste0("^", ticker_root_norm, "_.*\\.(html?|xls[x]?)$"),
    full.names = TRUE
  )
  if (length(existing_files)) {
    cleaned_dates <- as.Date(character())
    for (path in existing_files) {
      if (.bmf_file_has_no_data_message(path)) {
        file_date <- .bmf_extract_date_from_filename(path)
        unlink(path)
        if (!is.na(file_date)) {
          cleaned_dates <- c(cleaned_dates, file_date)
        }
      }
    }
    if (length(cleaned_dates)) {
      no_data_dates <- unique(c(no_data_dates, cleaned_dates))
    }
  }
  dates <- seq(from = start_date, to = end_date, by = "day")
  if (verbose) {
    message("Downloading ", length(dates), " day(s) for ", ticker_root_norm)
  }
  results <- lapply(dates, function(current_date) {
    base_name <- sprintf("%s_%s", ticker_root_norm, format(current_date, "%Y-%m-%d"))
    existing_paths <- file.path(dest_dir, paste0(base_name, c(".xls", ".html")))
    existing_paths <- existing_paths[file.exists(existing_paths)]
    if (skip_existing && length(existing_paths)) {
      existing_path <- normalizePath(existing_paths[1L], winslash = "/", mustWork = FALSE)
      if (verbose) {
        message("[", format(current_date), "] skipping existing file ", existing_path)
      }
      return(data.frame(
        date = current_date,
        status = "skipped",
        path = existing_path,
        message = NA_character_,
        stringsAsFactors = FALSE
      ))
    }
    if (skip_existing && current_date %in% no_data_dates) {
      if (verbose) {
        message("[", format(current_date), "] skipping cached no-data day")
      }
      return(data.frame(
        date = current_date,
        status = "no_data_cached",
        path = NA_character_,
        message = "cached no-data",
        stringsAsFactors = FALSE
      ))
    }
    if (verbose) {
      message("[", format(current_date), "] downloading...")
    }
    outcome <- tryCatch(
      {
        saved_path <- download_bmf_report(
          date = current_date,
          ticker_root = ticker_root_norm,
          dest_dir = dest_dir,
          overwrite = overwrite,
          quiet = quiet,
          format = format,
          which = which
        )
        reason_flag <- NULL
        parsed_meta <- try(
          suppressMessages(
            parse_bmf_report(
              saved_path,
              ticker_root = ticker_root_norm,
              report_date = current_date,
              format = NULL
            )
          ),
          silent = TRUE
        )
        if (!inherits(parsed_meta, "try-error") && !nrow(parsed_meta)) {
          reason_flag <- attr(parsed_meta, "reason")
        }
        if (is.null(reason_flag) && .bmf_file_has_no_data_message(saved_path)) {
          reason_flag <- "no_data"
        }
        if (!is.null(reason_flag) && reason_flag %in% c("no_data", "missing_tables", "incomplete_tables")) {
          if (file.exists(saved_path)) {
            unlink(saved_path)
          }
          no_data_dates <<- unique(c(no_data_dates, current_date))
          if (verbose) {
            message("[", format(current_date), "] ", reason_flag, " (removed file)")
          }
          return(data.frame(
            date = current_date,
            status = reason_flag,
            path = NA_character_,
            message = reason_flag,
            stringsAsFactors = FALSE
          ))
        }
        saved_path <- normalizePath(saved_path, winslash = "/", mustWork = TRUE)
        if (verbose) {
          message("[", format(current_date), "] saved ", saved_path)
        }
        data.frame(
          date = current_date,
          status = "downloaded",
          path = saved_path,
          message = NA_character_,
          stringsAsFactors = FALSE
        )
      },
      error = function(err) {
        if (verbose) {
          message("[", format(current_date), "] error: ", conditionMessage(err))
        }
        data.frame(
          date = current_date,
          status = "error",
          path = NA_character_,
          message = conditionMessage(err),
          stringsAsFactors = FALSE
        )
      }
    )
    outcome
  })
  summary <- do.call(rbind, results)
  rownames(summary) <- NULL
  .bmf_write_no_data_dates(ticker_root_norm, which, no_data_dates)
  invisible(summary)
}

#' Retrieve available ticker roots from the B3 bulletin
#'
#' @return A data frame with `ticker_root` and `description` columns.
#' @export
bmf_list_ticker_roots <- function() {
  url <- "https://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/SistemaPregao1.asp?pagetype=pop"
  resp <- httr::RETRY(
    "GET",
    url,
    httr::user_agent("brfutures package (https://github.com/)"),
    times = 3,
    pause_base = 1,
    terminate_on = c(400, 401, 403, 404)
  )
  httr::stop_for_status(resp, task = "retrieve ticker root list")
  content <- httr::content(resp, as = "text", encoding = "ISO-8859-1")
  doc <- xml2::read_html(content, encoding = "ISO-8859-1")
  options <- xml2::xml_find_all(doc, "//select[@name='cboMercadoria']/option")
  if (!length(options)) {
    stop("Could not locate the ticker list in the bulletin page.", call. = FALSE)
  }
  texts <- trimws(xml2::xml_text(options))
  texts <- iconv(texts, from = "UTF-8", to = "ASCII//TRANSLIT")
  pattern <- "^([A-Z0-9]{1,6})\\s*:\\s*(.+)$"
  matches <- regexec(pattern, texts)
  extracted <- regmatches(texts, matches)
  roots <- vapply(
    extracted,
    function(x) if (length(x) == 3L) x[2] else NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
  descriptions <- vapply(
    extracted,
    function(x) if (length(x) == 3L) trimws(x[3]) else NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
  keep <- !is.na(roots) & nzchar(roots) & !is.na(descriptions) & nzchar(descriptions)
  if (!any(keep)) {
    return(data.frame(ticker_root = character(), description = character(), stringsAsFactors = FALSE))
  }
  roots <- .bmf_normalize_ticker_root(roots[keep])
  descriptions <- descriptions[keep]
  df <- data.frame(
    ticker_root = roots,
    description = descriptions,
    stringsAsFactors = FALSE
  )
  df <- df[!duplicated(df$ticker_root), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Retrieve the aggregate data file for a ticker root
#'
#' @param ticker_root Commodity symbol root.
#' @param data_dir Directory containing saved bulletin files. Defaults to the
#'   cache directory for the ticker when `NULL`.
#' @param which Storage location passed to `tools::R_user_dir` when directories
#'   are inferred.
#'
#' @return A data frame with the aggregated data or an empty data frame if the
#'   aggregate is missing. Prints the path when found.
#' @export
bmf_get_aggregate <- function(ticker_root,
                              data_dir = NULL,
                              which = c("cache", "data", "config"),
                              ohlc_locf = TRUE,
                              return = c("agg", "list")) {
  normalized <- .bmf_normalize_ticker_root(ticker_root)
  normalized <- normalized[!is.na(normalized)]
  if (!length(normalized)) {
    stop("ticker_root cannot be missing or empty.", call. = FALSE)
  }
  ticker_root_norm <- normalized[1L]
  which <- match.arg(which)
  return <- match.arg(return)
  if (is.null(data_dir)) {
    data_dir <- .bmf_storage_dir(ticker_root_norm, which = which, create = FALSE)
  }
  if (is.null(data_dir) || !dir.exists(data_dir)) {
    message("Aggregate directory does not exist for ", ticker_root_norm)
    return(.bmf_empty_bulletin_dataframe())
  }
  aggregate_path <- file.path(data_dir, "xts", paste0(ticker_root_norm, "_aggregate.rds"))
  if (!file.exists(aggregate_path)) {
    message("Aggregate file not found for ", ticker_root_norm, " at ", aggregate_path)
    return(.bmf_empty_bulletin_dataframe())
  }
  aggregate_path <- normalizePath(aggregate_path, winslash = "/", mustWork = TRUE)
  message("Aggregate file: ", aggregate_path)
  data <- readRDS(aggregate_path)
  if ("estimated_maturity" %in% names(data) && !inherits(data$estimated_maturity, "Date")) {
    if (is.numeric(data$estimated_maturity)) {
      data$estimated_maturity <- as.Date(data$estimated_maturity, origin = "1970-01-01")
    } else {
      data$estimated_maturity <- as.Date(data$estimated_maturity)
    }
  }
  if (isTRUE(ohlc_locf)) {
    data <- .bmf_locf_ohlc(data)
  }
  if (identical(return, "list")) {
    ordered_idx <- order(data$ticker, data$date, seq_len(nrow(data)))
    data <- data[ordered_idx, , drop = FALSE]
    split_list <- split(data, data$ticker)
    attr(split_list, "path") <- aggregate_path
    return(split_list)
  }
  data
}

#' Retrieve a processed series for a specific contract ticker
#'
#' @param ticker Contract symbol (e.g. `"WDOZ24"`).
#' @param data_dir Directory containing saved bulletin files. Defaults to the
#'   cache directory for the parent ticker root.
#' @param which Storage location passed to `tools::R_user_dir` when directories
#'   are inferred.
#' @param type One of `"full"`, `"ohlc"`, or `"ohlcv_locf"`.
#' @param verbose If `TRUE`, emit informative messages.
#' @param return Controls what is returned. `"agg"` (default) returns the
#'   aggregate data frame; `"list"` returns a list containing the data and
#'   underlying file path.
#'
#' @return A data frame containing the requested series; an empty data frame is
#'   returned when the ticker is not found.
#' @export
bmf_get_series <- function(ticker,
                           data_dir = NULL,
                           which = c("cache", "data", "config"),
                           type = c("full", "ohlc", "ohlcv_locf"),
                           verbose = FALSE, tz = "America/Sao_Paulo") {
  if (missing(ticker) || !nzchar(trimws(ticker[1L]))) {
    stop("ticker must be provided.", call. = FALSE)
  }
  ticker <- toupper(trimws(ticker[1L]))
  ticker_root <- .bmf_normalize_ticker_root(substr(ticker, 1L, 3L))
  which <- match.arg(which)
  type <- match.arg(type)
  if (is.null(data_dir)) {
    data_dir <- .bmf_storage_dir(ticker_root, which = which, create = FALSE)
  }
  if (is.null(data_dir) || !dir.exists(data_dir)) {
    if (verbose) {
      message("Data directory does not exist for ", ticker_root)
    }
    return(.bmf_empty_bulletin_dataframe())
  }
  aggregate_path <- file.path(data_dir, "xts", paste0(ticker_root, "_aggregate.rds"))
  if (!file.exists(aggregate_path)) {
    if (verbose) {
      message("Aggregate file not found for ", ticker_root, " at ", aggregate_path)
    }
    return(.bmf_empty_bulletin_dataframe())
  }
  aggregate_data <- readRDS(aggregate_path)
  subset <- aggregate_data[aggregate_data$ticker == ticker, , drop = FALSE]
  if (!nrow(subset)) {
    if (verbose) {
      message("Ticker ", ticker, " not found in aggregate data.")
    }
    return(.bmf_empty_bulletin_dataframe())
  }
  if ("estimated_maturity" %in% names(subset) && !inherits(subset$estimated_maturity, "Date")) {
    if (is.numeric(subset$estimated_maturity)) {
      subset$estimated_maturity <- as.Date(subset$estimated_maturity, origin = "1970-01-01")
    } else {
      subset$estimated_maturity <- as.Date(subset$estimated_maturity)
    }
  }
  if (type == "ohlc") {
    cols <- intersect(c("date", "ticker", "open", "high", "low", "close", "volume"), names(subset))
    subset <- subset[, cols, drop = FALSE]
  } else if (type == "ohlcv_locf") {
    subset <- .bmf_locf_ohlc(subset)
    cols <- intersect(c("date", "ticker", "open", "high", "low", "close", "volume"), names(subset))
    subset <- subset[, cols, drop = FALSE]
  }


  subset$open <- as.numeric(subset$open)
  subset$high <- as.numeric(subset$high)
  subset$low <- as.numeric(subset$low)
  subset$close <- as.numeric(subset$close)
  subset$volume <- as.numeric(subset$volume)
  mask <- !(subset$open == 0 & subset$high == 0 &
              subset$low == 0 & subset$close == 0)
  subset <- subset[mask, ]
  dates <- as.POSIXct(as.Date(subset$date), tz = "UTC")
  datas <- lubridate::force_tz(subset$date, tzone = "America/Sao_Paulo")

  subset_xts <- xts(subset[, c("open", "high", "low", "close", "volume")],
                    order.by = datas)
  colnames(subset_xts) <- c("Open","High","Low","Close","Volume")
#   subset$date <- as.POSIXct(subset$date, format = "%Y-%m-%d", tz = "UTC")

  #subset_xts <- xts(subset[, -which(names(subset) %in% c("date", "DATE"))],
   #                 order.by = subset$date)
  #subset_xts
subset_xts
  }

.bmf_locf_ohlc <- function(df) {
  required_cols <- intersect(c("open", "high", "low", "close"), names(df))
  if (!length(required_cols) || !"ticker" %in% names(df) || !"date" %in% names(df)) {
    return(df)
  }
  order_idx <- order(df$ticker, df$date, seq_len(nrow(df)))
  split_idx <- split(order_idx, df$ticker[order_idx])
  for (indices in split_idx) {
    for (col in required_cols) {
      values <- df[[col]][indices]
      if (!is.numeric(values)) {
        values <- suppressWarnings(as.numeric(values))
      }
      prev <- NA_real_
      for (i in seq_along(values)) {
        v <- values[i]
        if (is.na(v) || (!is.na(v) && v == 0)) {
          if (!is.na(prev)) {
            values[i] <- prev
          }
        } else {
          prev <- v
        }
      }
      df[[col]][indices] <- values
    }
  }
  df
}
