.brf_b3_reference_cache_dir <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- file.path(
      .brf_cache_dir(create = TRUE),
      "BDI",
      "reference"
    )
  }
  cache_dir <- path.expand(as.character(cache_dir)[1L])
  if (is.na(cache_dir) || !nzchar(cache_dir)) {
    stop("cache_dir must be one non-empty path.", call. = FALSE)
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(cache_dir)) {
    stop("Unable to create B3 reference cache at ", cache_dir, ".", call. = FALSE)
  }
  normalizePath(cache_dir, mustWork = TRUE)
}

.brf_b3_daily_file_name <- function(date,
                                    kind = c(
                                      "instrument",
                                      "indicator",
                                      "settlement"
                                    )) {
  kind <- match.arg(kind)
  date <- .brf_normalize_date(date)
  prefix <- switch(
    kind,
    instrument = "IN",
    indicator = "ID",
    settlement = "SPRD"
  )
  extension <- if (kind == "indicator") ".ex_" else ".zip"
  paste0(prefix, format(date, "%y%m%d"), extension)
}

.brf_b3_daily_file_url <- function(date,
                                   kind = c(
                                     "instrument",
                                     "indicator",
                                     "settlement"
                                   )) {
  kind <- match.arg(kind)
  paste0(
    "https://www.b3.com.br/pesquisapregao/download?filelist=",
    .brf_b3_daily_file_name(date, kind)
  )
}

.brf_b3_download_daily_archive <- function(date,
                                            kind,
                                            temp_dir,
                                            quiet = FALSE) {
  source_file <- .brf_b3_daily_file_name(date, kind)
  url <- .brf_b3_daily_file_url(date, kind)
  destination <- tempfile(
    pattern = paste0(source_file, "-"),
    tmpdir = temp_dir,
    fileext = ".download"
  )
  hook <- getOption("brfutures.b3_reference_download_hook", NULL)
  if (is.function(hook)) {
    result <- hook(url = url, destination = destination)
    if (identical(result, FALSE) || !file.exists(destination)) {
      stop("B3 reference download hook did not create an archive.", call. = FALSE)
    }
  } else {
    response <- tryCatch(
      httr::RETRY(
        verb = "GET",
        url = url,
        httr::user_agent(.brf_bvbg_user_agent()),
        httr::accept("application/zip,application/octet-stream,*/*"),
        times = 3,
        pause_base = 1,
        httr::write_disk(destination, overwrite = TRUE),
        quiet = quiet
      ),
      error = function(e) e
    )
    if (inherits(response, "error")) {
      if (file.exists(destination)) {
        unlink(destination)
      }
      stop(
        "Unable to download ",
        source_file,
        ": ",
        conditionMessage(response),
        call. = FALSE
      )
    }
    status <- httr::status_code(response)
    if (is.null(status) || status >= 400L) {
      if (file.exists(destination)) {
        unlink(destination)
      }
      stop(
        "B3 returned HTTP ",
        status,
        " for ",
        source_file,
        ".",
        call. = FALSE
      )
    }
  }
  listing <- suppressWarnings(tryCatch(
    utils::unzip(destination, list = TRUE),
    error = function(e) NULL
  ))
  if (is.null(listing) || !nrow(listing)) {
    if (file.exists(destination)) {
      unlink(destination)
    }
    stop(
      "Downloaded B3 reference payload is not a readable archive: ",
      source_file,
      ".",
      call. = FALSE
    )
  }
  list(
    path = destination,
    source_file = source_file,
    source_url = url,
    source_archive_sha256 = .brf_b3_source_file_sha256(destination)
  )
}

.brf_b3_zip_entry_header <- function(archive, entry, n = 100L) {
  connection <- unz(archive, entry, open = "rb")
  on.exit(close(connection), add = TRUE)
  readLines(connection, n = n, warn = FALSE, encoding = "UTF-8")
}

.brf_b3_xml_snapshot_identity <- function(archive, entry) {
  header <- paste(
    .brf_b3_zip_entry_header(archive, entry),
    collapse = "\n"
  )
  extract <- function(pattern) {
    matched <- regexec(pattern, header, perl = TRUE)
    pieces <- regmatches(header, matched)[[1L]]
    if (length(pieces) < 2L) NA_character_ else pieces[[2L]]
  }
  list(
    report_type = extract("<BizGrpTp>\\s*([^<]+?)\\s*</BizGrpTp>"),
    created_at = .brf_b3_parse_timestamp(extract(
      "<CreDt>\\s*([^<]+?)\\s*</CreDt>"
    ))
  )
}

.brf_b3_extract_archive_entry <- function(archive, entry, temp_dir) {
  extract_dir <- tempfile("entry-", tmpdir = temp_dir)
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- suppressWarnings(tryCatch(
    utils::unzip(
      archive,
      files = entry,
      exdir = extract_dir,
      junkpaths = TRUE
    ),
    error = function(e) character()
  ))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    fallback <- file.path(extract_dir, basename(entry))
    if (file.exists(fallback)) {
      paths <- fallback
    }
  }
  if (length(paths) != 1L) {
    stop(
      "Unable to extract archive entry '",
      entry,
      "'.",
      call. = FALSE
    )
  }
  list(path = paths[[1L]], dir = extract_dir)
}

.brf_b3_find_nested_payload <- function(archive,
                                        kind,
                                        temp_dir) {
  xml_kind <- kind %in% c("instrument", "settlement")
  expected_report <- if (identical(kind, "instrument")) {
    "^BVBG\\.028(?:\\.|$)"
  } else if (identical(kind, "settlement")) {
    "^BVBG\\.187(?:\\.|$)"
  } else {
    NA_character_
  }
  queue <- normalizePath(archive, mustWork = TRUE)
  owned_dirs <- character()
  on.exit({
    for (path in owned_dirs) {
      if (dir.exists(path) && startsWith(
        normalizePath(path, mustWork = FALSE),
        normalizePath(temp_dir, mustWork = TRUE)
      )) {
        unlink(path, recursive = TRUE, force = TRUE)
      }
    }
  }, add = TRUE)
  instrument_candidates <- list()
  indicator_candidates <- list()
  visited <- character()

  while (length(queue)) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    current_key <- normalizePath(current, mustWork = TRUE)
    if (current_key %in% visited) {
      next
    }
    visited <- c(visited, current_key)
    listing <- suppressWarnings(tryCatch(
      utils::unzip(current, list = TRUE),
      error = function(e) NULL
    ))
    if (is.null(listing) || !nrow(listing)) {
      next
    }
    entries <- as.character(listing$Name)
    if (xml_kind) {
      xml_entries <- entries[grepl("\\.xml$", entries, ignore.case = TRUE)]
      if (length(xml_entries)) {
        for (entry in xml_entries) {
          identity <- tryCatch(
            .brf_b3_xml_snapshot_identity(current, entry),
            error = function(e) NULL
          )
          if (!is.null(identity) &&
              !is.na(identity$report_type) &&
              grepl(expected_report, identity$report_type) &&
              length(identity$created_at) == 1L &&
              !is.na(identity$created_at)) {
            instrument_candidates[[length(instrument_candidates) + 1L]] <-
              list(
                archive = current,
                entry = entry,
                report_type = identity$report_type,
                created_at = identity$created_at
              )
          }
        }
      }
    } else {
      hits <- entries[tolower(basename(entries)) == "indic.txt"]
      if (length(hits)) {
        for (entry in hits) {
          indicator_candidates[[length(indicator_candidates) + 1L]] <-
            list(archive = current, entry = entry)
        }
      }
    }

    nested_entries <- entries[
      grepl("\\.(zip|ex_)$", entries, ignore.case = TRUE)
    ]
    if (length(nested_entries)) {
      for (entry in nested_entries) {
        extracted <- .brf_b3_extract_archive_entry(
          current,
          entry,
          temp_dir
        )
        owned_dirs <- c(owned_dirs, extracted$dir)
        queue <- c(queue, extracted$path)
      }
    }
  }

  candidates <- if (xml_kind) {
    instrument_candidates
  } else {
    indicator_candidates
  }
  if (!length(candidates)) {
    stop(
      "No ",
      if (xml_kind) {
        if (identical(kind, "instrument")) "BVBG.028 XML" else "BVBG.187 XML"
      } else {
        "Indic.txt"
      },
      " payload found in the B3 archive.",
      call. = FALSE
    )
  }
  if (xml_kind) {
    created <- as.POSIXct(
      vapply(candidates, function(x) as.numeric(x$created_at), numeric(1L)),
      origin = "1970-01-01",
      tz = "UTC"
    )
    names_order <- vapply(candidates, `[[`, character(1L), "entry")
    selected <- order(created, names_order, na.last = NA)[length(created)]
  } else {
    names_order <- vapply(candidates, `[[`, character(1L), "entry")
    selected <- order(names_order)[length(candidates)]
  }
  chosen <- candidates[[selected]]
  extracted <- .brf_b3_extract_archive_entry(
    chosen$archive,
    chosen$entry,
    temp_dir
  )
  # The selected payload must outlive the nested working directories cleaned
  # by this function.
  durable <- tempfile(
    pattern = "selected-",
    tmpdir = temp_dir,
    fileext = paste0(".", tools::file_ext(chosen$entry))
  )
  if (!file.copy(extracted$path, durable, overwrite = TRUE)) {
    stop("Unable to stage selected B3 reference payload.", call. = FALSE)
  }
  if (dir.exists(extracted$dir)) {
    unlink(extracted$dir, recursive = TRUE, force = TRUE)
  }
  list(
    path = durable,
    source_file = basename(chosen$entry),
    report_type = if (xml_kind) {
      chosen$report_type
    } else {
      NA_character_
    },
    created_at = if (xml_kind) {
      chosen$created_at
    } else {
      as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
    }
  )
}

.brf_b3_visit_nested_xml_payloads <- function(archive,
                                               kind = "instrument",
                                               temp_dir,
                                               latest = TRUE,
                                               FUN) {
  if (!identical(kind, "instrument")) {
    stop(
      "Sequential nested XML traversal is currently defined for IN/BVBG.028.",
      call. = FALSE
    )
  }
  if (!is.function(FUN)) {
    stop("FUN must be a function.", call. = FALSE)
  }
  expected_report <- "^BVBG\\.028(?:\\.|$)"
  queue <- normalizePath(archive, mustWork = TRUE)
  owned_dirs <- character()
  on.exit({
    for (path in owned_dirs) {
      if (dir.exists(path) && startsWith(
        normalizePath(path, mustWork = FALSE),
        normalizePath(temp_dir, mustWork = TRUE)
      )) {
        unlink(path, recursive = TRUE, force = TRUE)
      }
    }
  }, add = TRUE)
  candidates <- list()
  visited <- character()

  while (length(queue)) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    current_key <- normalizePath(current, mustWork = TRUE)
    if (current_key %in% visited) {
      next
    }
    visited <- c(visited, current_key)
    listing <- suppressWarnings(tryCatch(
      utils::unzip(current, list = TRUE),
      error = function(e) NULL
    ))
    if (is.null(listing) || !nrow(listing)) {
      next
    }
    entries <- as.character(listing$Name)
    xml_entries <- entries[grepl("\\.xml$", entries, ignore.case = TRUE)]
    if (length(xml_entries)) {
      for (entry in xml_entries) {
        identity <- tryCatch(
          .brf_b3_xml_snapshot_identity(current, entry),
          error = function(e) NULL
        )
        if (!is.null(identity) &&
            !is.na(identity$report_type) &&
            grepl(expected_report, identity$report_type) &&
            length(identity$created_at) == 1L &&
            !is.na(identity$created_at)) {
          candidates[[length(candidates) + 1L]] <- list(
            archive = current,
            entry = entry,
            report_type = identity$report_type,
            created_at = identity$created_at
          )
        }
      }
    }
    nested_entries <- entries[
      grepl("\\.(zip|ex_)$", entries, ignore.case = TRUE)
    ]
    if (length(nested_entries)) {
      for (entry in nested_entries) {
        extracted <- .brf_b3_extract_archive_entry(
          current,
          entry,
          temp_dir
        )
        owned_dirs <- c(owned_dirs, extracted$dir)
        queue <- c(queue, extracted$path)
      }
    }
  }
  if (!length(candidates)) {
    stop(
      "No BVBG.028 XML payload found in the B3 archive.",
      call. = FALSE
    )
  }
  created <- as.POSIXct(
    vapply(candidates, function(x) as.numeric(x$created_at), numeric(1L)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  names_order <- vapply(candidates, `[[`, character(1L), "entry")
  candidates <- candidates[order(created, names_order, na.last = NA)]
  if (isTRUE(latest)) {
    candidates <- tail(candidates, 1L)
  }

  results <- vector("list", length(candidates))
  for (index in seq_along(candidates)) {
    candidate <- candidates[[index]]
    extracted <- .brf_b3_extract_archive_entry(
      candidate$archive,
      candidate$entry,
      temp_dir
    )
    result <- NULL
    tryCatch(
      {
        result <- FUN(extracted$path, candidate)
      },
      finally = {
        if (dir.exists(extracted$dir)) {
          unlink(extracted$dir, recursive = TRUE, force = TRUE)
        }
      }
    )
    results[[index]] <- result
  }
  results
}

.brf_b3_content_store <- function(source,
                                  base_dir,
                                  source_file) {
  sha256 <- .brf_b3_source_file_sha256(source)
  target_dir <- file.path(base_dir, "sha256", sha256)
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }
  target <- file.path(target_dir, basename(source_file))
  if (!file.exists(target)) {
    staged <- tempfile(".partial-", tmpdir = target_dir)
    if (!file.copy(source, staged, overwrite = TRUE)) {
      stop("Unable to stage content-addressed B3 source.", call. = FALSE)
    }
    if (!file.rename(staged, target)) {
      if (file.exists(staged)) {
        unlink(staged)
      }
      stop("Unable to atomically publish B3 source cache.", call. = FALSE)
    }
  }
  list(path = target, sha256 = sha256, dir = target_dir)
}

.brf_b3_atomic_save_rds <- function(object, path, immutable = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(immutable) && file.exists(path)) {
    existing <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!identical(existing, object)) {
      stop(
        "Immutable B3 reference manifest fingerprint collision.",
        call. = FALSE
      )
    }
    return(invisible(path))
  }
  staged <- tempfile(".partial-", tmpdir = dirname(path))
  saveRDS(object, staged, compress = "xz")
  if (isTRUE(immutable) && file.exists(path)) {
    existing <- tryCatch(readRDS(path), error = function(e) NULL)
    unlink(staged)
    if (!identical(existing, object)) {
      stop(
        "Immutable B3 reference manifest fingerprint collision.",
        call. = FALSE
      )
    }
    return(invisible(path))
  }
  if (!file.rename(staged, path)) {
    if (file.exists(staged)) {
      unlink(staged)
    }
    stop("Unable to atomically publish B3 reference manifest.", call. = FALSE)
  }
  invisible(path)
}

#' Fetch official B3 futures lifecycle source data
#'
#' Downloads `INyymmdd.zip` and retains the compressed archive in a
#' SHA-256-addressed cache. Nested `BVBG.028` candidates are ordered by their
#' official `AppHdr/CreDt`. They are extracted and parsed one at a time, then
#' immediately removed, so requesting all revisions does not retain multiple
#' hundreds-of-megabytes XML files on disk.
#'
#' @param date B3 report date.
#' @param cache_dir Optional reference cache root. Defaults below
#'   `options(brfutures.cache_dir)`.
#' @param refresh Whether to download the archive even when cached snapshots
#'   exist.
#' @param quiet Suppress download progress messages.
#' @param latest Whether to return only the newest official revision per
#'   contract and report date. Set to `FALSE` to retain corrections in causal
#'   publication order.
#' @return A causal BIT lifecycle data frame for the report date.
#' @export
brf_b3_contract_lifecycle_fetch <- function(date,
                                            cache_dir = NULL,
                                            refresh = FALSE,
                                            quiet = FALSE,
                                            latest = TRUE) {
  date <- .brf_normalize_date(date)
  cache_dir <- .brf_b3_reference_cache_dir(cache_dir)
  day_dir <- file.path(
    cache_dir,
    "instruments",
    format(date, "%Y-%m-%d")
  )
  dir.create(day_dir, recursive = TRUE, showWarnings = FALSE)
  archive_dir <- file.path(day_dir, "archives")
  cached_archives <- if (dir.exists(archive_dir)) {
    list.files(
      archive_dir,
      pattern = "\\.(zip|ex_)$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character()
  }
  legacy_cached_xml <- list.files(
    day_dir,
    pattern = "\\.xml$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  work_dir <- tempfile("fetch-", tmpdir = cache_dir)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    if (dir.exists(work_dir) && startsWith(
      normalizePath(work_dir, mustWork = FALSE),
      normalizePath(cache_dir, mustWork = TRUE)
    )) {
      unlink(work_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  if (isTRUE(refresh) ||
      (!length(cached_archives) && !length(legacy_cached_xml))) {
    archive <- .brf_b3_download_daily_archive(
      date,
      "instrument",
      work_dir,
      quiet = quiet
    )
    stored_archive <- .brf_b3_content_store(
      archive$path,
      archive_dir,
      archive$source_file
    )
    cached_archives <- unique(c(cached_archives, stored_archive$path))
  }
  if (length(cached_archives)) {
    frames <- list()
    for (archive_path in cached_archives) {
      archive_sha256 <- .brf_b3_source_file_sha256(archive_path)
      parsed <- .brf_b3_visit_nested_xml_payloads(
        archive_path,
        kind = "instrument",
        temp_dir = work_dir,
        latest = latest,
        FUN = function(path, candidate) {
          out <- .brf_b3_contract_lifecycle_parse_one(path, root = "BIT")
          out$source_archive_file <- rep(basename(archive_path), nrow(out))
          out$source_archive_sha256 <- rep(archive_sha256, nrow(out))
          out$source_archive_entry <- rep(candidate$entry, nrow(out))
          out
        }
      )
      frames <- c(frames, parsed)
    }
    return(.brf_b3_contract_lifecycle_finalize(
      frames,
      latest = latest
    ))
  }
  if (!length(legacy_cached_xml)) {
    stop("No cached BVBG.028 snapshot is available for ", date, ".", call. = FALSE)
  }
  brf_b3_contract_lifecycle_read(
    legacy_cached_xml,
    root = "BIT",
    latest = latest
  )
}

#' Fetch official historical B3 settlement rows
#'
#' Downloads the daily `SPRDyymmdd.zip` archive and parses its BVBG.187 price
#' report directly, regardless of the package's global HTML/XML cutover. This
#' gives BIT and other explicitly requested roots a complete historical source
#' path without changing how `update_brfut()` selects its normal source.
#'
#' The selected BVBG.187 XML is retained in a SHA-256-addressed cache. Official
#' `AdjstdQtStin` and `PrvsAdjstdQtStin`, application-header availability,
#' message/instrument identifiers and source fingerprints are preserved.
#' `available_at` is the `AppHdr/CreDt` of the complete `PricRpt` group.
#' `settlement_available_at` is the same observed timestamp when that group
#' contains `AdjstdQt`; it is never synthesized from the report date. This
#' settlement-specific clock is mandatory in
#' `brfutures_b3_bit_sources_v2`; persisted version-1 rows are incompatible
#' with exact execution and must be rebuilt from the retained official XML.
#'
#' @param date B3 report date.
#' @param root B3 futures root to retain. Defaults to `"BIT"`.
#' @param cache_dir Optional reference cache root.
#' @param refresh Whether to download even when a cached BVBG.187 source exists.
#' @param quiet Suppress download progress messages.
#' @return Canonical B3 settlement rows for the requested root and date,
#'   including UTC `available_at` and `settlement_available_at` evidence.
#' @export
brf_b3_settlements_fetch <- function(date,
                                     root = "BIT",
                                     cache_dir = NULL,
                                     refresh = FALSE,
                                     quiet = FALSE) {
  date <- .brf_normalize_date(date)
  root <- .brf_normalize_root(root)
  cache_dir <- .brf_b3_reference_cache_dir(cache_dir)
  day_dir <- file.path(
    cache_dir,
    "settlements",
    root,
    format(date, "%Y-%m-%d")
  )
  dir.create(day_dir, recursive = TRUE, showWarnings = FALSE)
  cached <- list.files(
    day_dir,
    pattern = "\\.xml$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (isTRUE(refresh) || !length(cached)) {
    work_dir <- tempfile("fetch-", tmpdir = cache_dir)
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit({
      if (dir.exists(work_dir) && startsWith(
        normalizePath(work_dir, mustWork = FALSE),
        normalizePath(cache_dir, mustWork = TRUE)
      )) {
        unlink(work_dir, recursive = TRUE, force = TRUE)
      }
    }, add = TRUE)
    archive <- .brf_b3_download_daily_archive(
      date,
      "settlement",
      work_dir,
      quiet = quiet
    )
    payload <- .brf_b3_find_nested_payload(
      archive$path,
      "settlement",
      work_dir
    )
    stored <- .brf_b3_content_store(
      payload$path,
      day_dir,
      payload$source_file
    )
    cached <- unique(c(cached, stored$path))
  }
  if (!length(cached)) {
    stop("No cached BVBG.187 source is available for ", date, ".", call. = FALSE)
  }
  frames <- lapply(cached, function(path) {
    parsed <- .brf_parse_bvbg_xml_for_root(path, root)
    if (!inherits(parsed, "data.frame") ||
        !nrow(parsed) ||
        isTRUE(attr(parsed, "brf_no_data"))) {
      return(NULL)
    }
    parsed
  })
  frames <- Filter(Negate(is.null), frames)
  if (!length(frames)) {
    out <- .brf_empty_bulletin()
    out$source_schema_id <- character()
    out$source_schema_version <- integer()
    return(out)
  }
  out <- .brf_bind_rows(frames)
  out$date <- as.Date(out$date)
  out$available_at <- .brf_b3_parse_timestamp(out$available_at)
  out$settlement_available_at <- .brf_b3_parse_timestamp(
    out$settlement_available_at
  )
  out$source_schema_id <- .brf_b3_bit_source_schema_id()
  out$source_schema_version <- .brf_b3_bit_source_schema_version()
  required <- c(
    "contract_code", "date", "available_at", "settlement_available_at",
    "settlement_price",
    "settlement_status", "previous_settlement",
    "previous_settlement_status", "source_report_type", "source_file",
    "source_sha256"
  )
  if (!all(required %in% names(out))) {
    stop("BVBG.187 parser omitted required settlement provenance.", call. = FALSE)
  }
  invalid <- is.na(out$contract_code) | !nzchar(out$contract_code) |
    is.na(out$date) | is.na(out$available_at) |
    is.na(out$settlement_available_at) |
    out$settlement_available_at > out$available_at |
    is.na(out$source_file) | !nzchar(out$source_file) |
    is.na(out$source_sha256) |
    !grepl("^[0-9a-f]{64}$", out$source_sha256) |
    is.na(out$source_report_type) |
    !grepl("^BVBG\\.187(?:\\.|$)", out$source_report_type)
  if (any(invalid)) {
    stop(
      "BVBG.187 settlement rows require causal timestamps and complete ",
      "source provenance.",
      call. = FALSE
    )
  }
  out <- out[out$date == date, , drop = FALSE]
  out <- out[order(
    out$contract_code,
    out$date,
    out$available_at,
    out$source_sha256
  ), , drop = FALSE]
  key <- paste(out$contract_code, out$date, sep = "|")
  out <- out[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.brf_b3_indicator_cached_manifests <- function(day_dir) {
  paths <- list.files(
    day_dir,
    pattern = "^manifest(?:-[0-9a-f]{64})?\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  manifests <- lapply(paths, function(path) {
    value <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.list(value)) value else NULL
  })
  Filter(Negate(is.null), manifests)
}

.brf_b3_indicator_manifest_fingerprint <- function(manifest) {
  required <- c(
    "schema_version", "report_date", "available_at", "payload_sha256",
    "source_archive_file", "source_archive_sha256", "source_url"
  )
  missing <- setdiff(required, names(manifest))
  if (length(missing)) {
    stop(
      "Indicator manifest is missing fingerprint fields: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  payload <- manifest[required]
  payload$report_date <- format(as.Date(payload$report_date), "%Y-%m-%d")
  payload$available_at <- format(
    .brf_b3_parse_timestamp(payload$available_at),
    "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  )
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

#' Fetch official B3 indicator source data used by BIT
#'
#' Downloads `IDyymmdd.ex_`, traverses its nested archive and stores the
#' selected `Indic.txt` content by SHA-256. Because the legacy payload has no
#' authoritative publication timestamp, a new download requires explicit
#' `available_at` evidence. Each distinct evidence tuple is written to its own
#' immutable, content-addressed observation manifest. Re-fetching identical
#' bytes with a later evidenced availability therefore preserves both
#' observations, while the returned view selects the latest causal one.
#'
#' @param date B3 report date.
#' @param available_at Causal publication/availability timestamp. May be
#'   omitted only when a cached manifest already supplies it.
#' @param cache_dir Optional reference cache root.
#' @param refresh Whether to download even when a cached manifest exists.
#' @param quiet Suppress download progress messages.
#' @return Parsed BIT settlement indicator observations with payload and
#'   archive provenance.
#' @export
brf_b3_indicators_fetch <- function(date,
                                    available_at = NULL,
                                    cache_dir = NULL,
                                    refresh = FALSE,
                                    quiet = FALSE) {
  date <- .brf_normalize_date(date)
  cache_dir <- .brf_b3_reference_cache_dir(cache_dir)
  day_dir <- file.path(
    cache_dir,
    "indicators",
    format(date, "%Y-%m-%d")
  )
  dir.create(day_dir, recursive = TRUE, showWarnings = FALSE)
  manifests <- .brf_b3_indicator_cached_manifests(day_dir)
  if (isTRUE(refresh) || !length(manifests)) {
    parsed_available_at <- .brf_b3_parse_timestamp(available_at)
    if (length(parsed_available_at) != 1L || is.na(parsed_available_at)) {
      stop(
        "A new Indic.txt download requires explicit causal available_at ",
        "evidence; the payload does not embed it.",
        call. = FALSE
      )
    }
    work_dir <- tempfile("fetch-", tmpdir = cache_dir)
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit({
      if (dir.exists(work_dir) && startsWith(
        normalizePath(work_dir, mustWork = FALSE),
        normalizePath(cache_dir, mustWork = TRUE)
      )) {
        unlink(work_dir, recursive = TRUE, force = TRUE)
      }
    }, add = TRUE)
    archive <- .brf_b3_download_daily_archive(
      date,
      "indicator",
      work_dir,
      quiet = quiet
    )
    payload <- .brf_b3_find_nested_payload(
      archive$path,
      "indicator",
      work_dir
    )
    stored <- .brf_b3_content_store(
      payload$path,
      day_dir,
      "Indic.txt"
    )
    manifest <- list(
      schema_version = 1L,
      report_date = date,
      available_at = parsed_available_at,
      payload_path = stored$path,
      payload_sha256 = stored$sha256,
      source_archive_file = archive$source_file,
      source_archive_sha256 = archive$source_archive_sha256,
      source_url = archive$source_url
    )
    manifest$observation_fingerprint <-
      .brf_b3_indicator_manifest_fingerprint(manifest)
    .brf_b3_atomic_save_rds(
      manifest,
      file.path(
        stored$dir,
        paste0(
          "manifest-",
          manifest$observation_fingerprint,
          ".rds"
        )
      ),
      immutable = TRUE
    )
    manifests <- .brf_b3_indicator_cached_manifests(day_dir)
  }
  if (!length(manifests)) {
    stop("No cached Indic.txt manifest is available for ", date, ".", call. = FALSE)
  }
  frames <- lapply(manifests, function(manifest) {
    required <- c(
      "report_date", "available_at", "payload_path", "payload_sha256",
      "source_archive_file", "source_archive_sha256"
    )
    if (!all(required %in% names(manifest)) ||
        !file.exists(manifest$payload_path) ||
        !identical(
          .brf_b3_source_file_sha256(manifest$payload_path),
          manifest$payload_sha256
        )) {
      stop("Cached Indic.txt manifest failed integrity validation.", call. = FALSE)
    }
    fingerprint <- .brf_b3_indicator_manifest_fingerprint(manifest)
    if (!is.null(manifest$observation_fingerprint) &&
        !identical(manifest$observation_fingerprint, fingerprint)) {
      stop(
        "Cached Indic.txt observation fingerprint does not reconcile.",
        call. = FALSE
      )
    }
    out <- brf_b3_indicators_read(
      manifest$payload_path,
      report_date = manifest$report_date,
      available_at = manifest$available_at,
      source_file = manifest$source_archive_file
    )
    out$source_archive_file <- manifest$source_archive_file
    out$source_archive_sha256 <- manifest$source_archive_sha256
    out$source_url <- manifest$source_url %||% NA_character_
    out$source_observation_fingerprint <- fingerprint
    out
  })
  out <- do.call(rbind, frames)
  out <- out[order(
    out$reference_date,
    out$canonical_indicator,
    out$available_at,
    out$source_sha256
  ), , drop = FALSE]
  key <- paste(out$reference_date, out$canonical_indicator, sep = "|")
  out <- out[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}
