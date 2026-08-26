.brf_contract_registry_cache <- new.env(parent = emptyenv())

.brf_contract_registry_cutover_year <- function() {
  2018L
}

.brf_contract_root_specs <- function() {
  data.frame(
    root = c(
      "WIN", "IND", "WDO", "DOL", "CCM", "BGI", "ICF", "DI1",
      "BIT", "XFI", "SOL", "ETR", "GLD"
    ),
    multiplier = c(
      0.2, 1, 10, 50, 450, 330, 100, 1, 0.01, 10, 5, 0.25, 1
    ),
    tick_size = c(
      5, 5, 0.5, 0.5, 0.01, 0.05, 0.05, NA_real_,
      20, 0.10, 0.01, 0.10, 0.25
    ),
    tick_value = c(
      1, 5, 5, 25, 4.5, 16.5, 5, NA_real_,
      0.2, 1, 0.05, 0.025, 0.25
    ),
    contract_size = c(
      NA_real_, NA_real_, 10000, 50000, 450, 330, 100, 100000,
      0.01, rep(NA_real_, 4L)
    ),
    unit_of_measure = c(
      "index points", "index points", "USD", "USD", "60kg corn bags",
      "arrobas", "60kg coffee bags", "PU points", "BTC",
      rep(NA_character_, 4L)
    ),
    stringsAsFactors = FALSE
  )
}

#' Read canonical B3 futures root specifications
#'
#' Returns the stable product-level fields used when historical contracts do
#' not carry an official per-contract value. Per-contract `BVBG.028`
#' multipliers returned by [brf_contract_resolve()] always take precedence.
#' DI1 intentionally has no scalar tick size or tick value because those are
#' maturity- and price-dependent execution facts.
#'
#' @return A data frame with one row per supported B3 futures root.
#' @export
brf_contract_specs <- function() {
  out <- .brf_contract_root_specs()
  out$origin <- "brfutures canonical B3 root specifications"
  out
}

.brf_contract_registry_columns <- function() {
  c(
    "ticker", "contract_symbol", "root", "contract_year",
    "contract_month", "maturity_date", "last_trade_date", "multiplier",
    "contract_size", "unit_of_measure", "currency", "official",
    "date_quality", "maturity_rule", "last_trade_rule", "source_type",
    "source_report_date", "source_available_at", "source_report_type",
    "source_message_id", "source_archive_file", "source_archive_sha256",
    "source_snapshot_sha256", "instrument_id", "first_seen_date",
    "last_seen_date", "origin", "status"
  )
}

.brf_contract_registry_empty <- function(n = 0L) {
  n <- as.integer(n)
  data.frame(
    ticker = rep(NA_character_, n),
    contract_symbol = rep(NA_character_, n),
    root = rep(NA_character_, n),
    contract_year = rep(NA_integer_, n),
    contract_month = rep(NA_integer_, n),
    maturity_date = rep(as.Date(NA), n),
    last_trade_date = rep(as.Date(NA), n),
    multiplier = rep(NA_real_, n),
    contract_size = rep(NA_real_, n),
    unit_of_measure = rep(NA_character_, n),
    currency = rep(NA_character_, n),
    official = rep(NA, n),
    date_quality = rep(NA_character_, n),
    maturity_rule = rep(NA_character_, n),
    last_trade_rule = rep(NA_character_, n),
    source_type = rep(NA_character_, n),
    source_report_date = rep(as.Date(NA), n),
    source_available_at = rep(
      as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"), n
    ),
    source_report_type = rep(NA_character_, n),
    source_message_id = rep(NA_character_, n),
    source_archive_file = rep(NA_character_, n),
    source_archive_sha256 = rep(NA_character_, n),
    source_snapshot_sha256 = rep(NA_character_, n),
    instrument_id = rep(NA_character_, n),
    first_seen_date = rep(as.Date(NA), n),
    last_seen_date = rep(as.Date(NA), n),
    origin = rep(NA_character_, n),
    status = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
}

.brf_contract_registry_builtin_path <- function() {
  path <- system.file(
    "extdata", "b3-futures-contracts.rds", package = "brfutures"
  )
  if (!nzchar(path) || !file.exists(path)) {
    stop(
      "The built-in B3 futures contract registry is unavailable; reinstall ",
      "brfutures from a complete package build.",
      call. = FALSE
    )
  }
  path
}

.brf_contract_registry_cache_dir <- function(cache_dir = NULL,
                                             create = FALSE) {
  if (is.null(cache_dir)) {
    base <- getOption("brfutures.cache_dir", NULL)
    if (is.null(base) || !length(base) || is.na(base[[1L]]) ||
        !nzchar(trimws(as.character(base[[1L]])))) {
      return(NULL)
    }
    cache_dir <- file.path(
      path.expand(as.character(base[[1L]])), "BDI", "reference"
    )
  } else {
    cache_dir <- path.expand(as.character(cache_dir)[1L])
  }
  if (is.na(cache_dir) || !nzchar(cache_dir)) {
    stop("cache_dir must be one non-empty path.", call. = FALSE)
  }
  if (isTRUE(create) && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (isTRUE(create) && !dir.exists(cache_dir)) {
    stop("Unable to create contract registry cache at ", cache_dir, ".",
      call. = FALSE
    )
  }
  normalizePath(cache_dir, mustWork = FALSE)
}

.brf_contract_registry_cache_path <- function(cache_dir = NULL,
                                              create = FALSE) {
  directory <- .brf_contract_registry_cache_dir(cache_dir, create = create)
  if (is.null(directory)) return(NULL)
  file.path(directory, "b3-futures-contracts.rds")
}

.brf_contract_registry_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1970-01-01"))
  suppressWarnings(as.Date(as.character(x)))
}

.brf_contract_registry_timestamp <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }
  suppressWarnings(as.POSIXct(as.character(x), tz = "UTC"))
}

.brf_contract_registry_normalize <- function(x, label = "contract registry") {
  if (!is.data.frame(x)) {
    stop(label, " must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(.brf_contract_registry_columns(), names(x))
  if (length(missing)) {
    stop(
      label, " is missing required fields: ", paste(missing, collapse = ", "),
      ".", call. = FALSE
    )
  }
  x <- x[.brf_contract_registry_columns()]
  character_columns <- c(
    "ticker", "contract_symbol", "root", "unit_of_measure", "currency",
    "date_quality", "maturity_rule", "last_trade_rule", "source_type",
    "source_report_type", "source_message_id", "source_archive_file",
    "source_archive_sha256", "source_snapshot_sha256", "instrument_id",
    "origin", "status"
  )
  for (name in character_columns) {
    x[[name]] <- as.character(x[[name]])
  }
  x$ticker <- toupper(trimws(x$ticker))
  x$contract_symbol <- toupper(trimws(x$contract_symbol))
  x$root <- toupper(trimws(x$root))
  x$contract_year <- suppressWarnings(as.integer(x$contract_year))
  x$contract_month <- suppressWarnings(as.integer(x$contract_month))
  x$maturity_date <- .brf_contract_registry_date(x$maturity_date)
  x$last_trade_date <- .brf_contract_registry_date(x$last_trade_date)
  x$source_report_date <- .brf_contract_registry_date(x$source_report_date)
  x$first_seen_date <- .brf_contract_registry_date(x$first_seen_date)
  x$last_seen_date <- .brf_contract_registry_date(x$last_seen_date)
  x$source_available_at <- .brf_contract_registry_timestamp(
    x$source_available_at
  )
  x$multiplier <- suppressWarnings(as.numeric(x$multiplier))
  x$contract_size <- suppressWarnings(as.numeric(x$contract_size))
  x$official <- as.logical(x$official)

  standard <- grepl(
    "^[A-Z0-9]{2,8}[FGHJKMNQUVXZ][0-9]{2}$", x$ticker, perl = TRUE
  )
  invalid <- !standard |
    is.na(x$ticker) | !nzchar(x$ticker) |
    is.na(x$contract_symbol) | x$contract_symbol != x$ticker |
    is.na(x$root) | !nzchar(x$root) |
    is.na(x$maturity_date) | is.na(x$last_trade_date) |
    !is.finite(x$multiplier) | x$multiplier <= 0 |
    !isTRUE(all(x$official %in% TRUE))
  if (any(invalid)) {
    stop(label, " contains invalid canonical official rows.", call. = FALSE)
  }

  duplicate_tickers <- unique(x$ticker[
    duplicated(x$ticker) | duplicated(x$ticker, fromLast = TRUE)
  ])
  if (length(duplicate_tickers)) {
    fact_columns <- c(
      "root", "maturity_date", "last_trade_date", "multiplier",
      "contract_size", "currency"
    )
    for (ticker in duplicate_tickers) {
      rows <- x[x$ticker == ticker, fact_columns, drop = FALSE]
      encoded <- do.call(paste, c(lapply(rows, as.character), sep = "\r"))
      if (length(unique(encoded)) != 1L) {
        stop(
          label, " contains conflicting official definitions for `", ticker,
          "`.", call. = FALSE
        )
      }
    }
    x <- x[order(
      x$ticker, x$source_available_at, x$source_report_date,
      na.last = TRUE
    ), , drop = FALSE]
    first_seen <- tapply(x$first_seen_date, x$ticker, min, na.rm = TRUE)
    last_seen <- tapply(x$last_seen_date, x$ticker, max, na.rm = TRUE)
    x <- x[!duplicated(x$ticker, fromLast = TRUE), , drop = FALSE]
    x$first_seen_date <- as.Date(
      first_seen[x$ticker], origin = "1970-01-01"
    )
    x$last_seen_date <- as.Date(
      last_seen[x$ticker], origin = "1970-01-01"
    )
  }
  x <- x[order(x$ticker), , drop = FALSE]
  rownames(x) <- NULL
  x
}

.brf_contract_registry_signature <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths) & file.exists(paths)]
  if (!length(paths)) return("")
  info <- file.info(paths)
  paste(
    paths, as.numeric(info$mtime), info$size,
    collapse = "|"
  )
}

#' Read the canonical B3 futures contract registry
#'
#' Reads the compact registry bundled with the package and, when present, an
#' explicitly refreshed local registry. Contract definitions from 2018 onward
#' come only from official B3 `BVBG.028` instrument snapshots. Ordinary reads
#' are local, cached in memory, and never access the network.
#'
#' @param cache_dir Optional B3 reference-cache directory containing a registry
#'   created by [brf_b3_contract_registry_update()]. When omitted, a configured
#'   `options(brfutures.cache_dir)` is consulted without creating directories.
#' @return A data frame with one canonical official definition per ticker.
#' @export
brf_contract_registry <- function(cache_dir = NULL) {
  builtin_path <- .brf_contract_registry_builtin_path()
  cache_path <- .brf_contract_registry_cache_path(cache_dir, create = FALSE)
  paths <- c(builtin_path, cache_path)
  signature <- .brf_contract_registry_signature(paths)
  cache_key <- paste0(
    normalizePath(builtin_path, mustWork = TRUE), "|",
    if (is.null(cache_path)) "" else cache_path
  )
  cached <- .brf_contract_registry_cache[[cache_key]]
  if (is.list(cached) && identical(cached$signature, signature)) {
    return(cached$data)
  }

  frames <- list(readRDS(builtin_path))
  if (!is.null(cache_path) && file.exists(cache_path)) {
    frames[[length(frames) + 1L]] <- readRDS(cache_path)
  }
  registry <- .brf_contract_registry_normalize(
    do.call(rbind, frames), "B3 futures contract registry"
  )
  .brf_contract_registry_cache[[cache_key]] <- list(
    signature = signature,
    data = registry
  )
  registry
}

.brf_contract_registry_from_lifecycle <- function(lifecycle) {
  required <- c(
    "contract", "root", "report_date", "available_at", "expiry_date",
    "last_trade_date", "contract_multiplier", "quote_currency",
    "instrument_id", "source_report_type", "source_message_id",
    "source_archive_file", "source_archive_sha256", "source_sha256"
  )
  if (!is.data.frame(lifecycle) || !nrow(lifecycle) ||
      length(setdiff(required, names(lifecycle)))) {
    stop(
      "A non-empty canonical BVBG.028 lifecycle snapshot is required.",
      call. = FALSE
    )
  }
  ticker <- toupper(trimws(as.character(lifecycle$contract)))
  keep <- grepl(
    "^[A-Z0-9]{2,8}[FGHJKMNQUVXZ][0-9]{2}$", ticker, perl = TRUE
  ) & !is.na(lifecycle$expiry_date) &
    as.integer(format(as.Date(lifecycle$expiry_date), "%Y")) >=
      .brf_contract_registry_cutover_year()
  lifecycle <- lifecycle[keep, , drop = FALSE]
  ticker <- ticker[keep]
  if (!nrow(lifecycle)) {
    stop("BVBG.028 contains no canonical dated futures rows.", call. = FALSE)
  }
  root <- toupper(trimws(as.character(lifecycle$root)))
  specs <- .brf_contract_root_specs()
  spec_index <- match(root, specs$root)
  maturity <- as.Date(lifecycle$expiry_date)
  out <- .brf_contract_registry_empty(nrow(lifecycle))
  out$ticker <- ticker
  out$contract_symbol <- ticker
  out$root <- root
  out$contract_year <- as.integer(format(maturity, "%Y"))
  out$contract_month <- as.integer(format(maturity, "%m"))
  out$maturity_date <- maturity
  out$last_trade_date <- as.Date(lifecycle$last_trade_date)
  out$multiplier <- suppressWarnings(as.numeric(
    lifecycle$contract_multiplier
  ))
  out$contract_size <- specs$contract_size[spec_index]
  out$unit_of_measure <- specs$unit_of_measure[spec_index]
  out$currency <- as.character(lifecycle$quote_currency)
  out$official <- TRUE
  out$date_quality <- "official"
  out$maturity_rule <- "official_bvbg028_expiration_date"
  out$last_trade_rule <- "official_bvbg028_trading_end_date"
  out$source_type <- "B3_IN_BVBG.028"
  out$source_report_date <- as.Date(lifecycle$report_date)
  out$source_available_at <- .brf_contract_registry_timestamp(
    lifecycle$available_at
  )
  out$source_report_type <- as.character(lifecycle$source_report_type)
  out$source_message_id <- as.character(lifecycle$source_message_id)
  out$source_archive_file <- as.character(lifecycle$source_archive_file)
  out$source_archive_sha256 <- as.character(
    lifecycle$source_archive_sha256
  )
  out$source_snapshot_sha256 <- as.character(lifecycle$source_sha256)
  out$instrument_id <- as.character(lifecycle$instrument_id)
  out$first_seen_date <- as.Date(lifecycle$report_date)
  out$last_seen_date <- as.Date(lifecycle$report_date)
  out$origin <- "B3 BVBG.028 official instrument definition"
  out$status <- "resolved"
  .brf_contract_registry_normalize(out, "BVBG.028 lifecycle registry")
}

#' Refresh the canonical B3 futures contract registry
#'
#' Downloads one official B3 `BVBG.028` instrument snapshot, extracts every
#' dated futures definition, merges it with the bundled/local registry, and
#' atomically publishes the local result. This is an explicit maintenance
#' operation; [brf_contract_resolve()] never invokes it during a trading cycle.
#'
#' @param date B3 report date to ingest.
#' @param cache_dir Optional B3 reference-cache directory.
#' @param refresh Whether to redownload an already cached archive.
#' @param quiet Suppress download progress messages.
#' @return Invisibly, the merged canonical registry.
#' @export
brf_b3_contract_registry_update <- function(date,
                                            cache_dir = NULL,
                                            refresh = FALSE,
                                            quiet = FALSE) {
  reference_dir <- if (is.null(cache_dir)) {
    .brf_b3_reference_cache_dir(NULL)
  } else {
    .brf_contract_registry_cache_dir(cache_dir, create = TRUE)
  }
  lifecycle <- brf_b3_contract_lifecycle_fetch(
    date = date,
    cache_dir = reference_dir,
    refresh = refresh,
    quiet = quiet,
    latest = TRUE,
    root = NULL
  )
  observed <- .brf_contract_registry_from_lifecycle(lifecycle)
  existing <- brf_contract_registry(cache_dir = reference_dir)
  merged <- .brf_contract_registry_normalize(
    rbind(existing, observed), "Merged B3 futures contract registry"
  )
  target <- .brf_contract_registry_cache_path(
    reference_dir, create = TRUE
  )
  .brf_b3_atomic_save_rds(merged, target)
  rm(list = ls(.brf_contract_registry_cache, all.names = TRUE),
    envir = .brf_contract_registry_cache
  )
  invisible(merged)
}

.brf_contract_reference_dates <- function(reference_date, n) {
  if (is.null(reference_date)) {
    return(rep(Sys.Date(), n))
  }
  dates <- .brf_contract_registry_date(reference_date)
  if (length(dates) == 1L) dates <- rep(dates, n)
  if (length(dates) != n || anyNA(dates)) {
    stop(
      "reference_date must be one valid date or one date per ticker.",
      call. = FALSE
    )
  }
  dates
}

#' Resolve canonical B3 futures contract metadata
#'
#' Resolution is exact and local. Tickers with maturity in 2018 or later must
#' exist in the official `BVBG.028` registry; absence is an error and never
#' falls back to a formula. Supported pre-2018 contracts use the historical B3
#' rules and the package's B3 trading-session calendar.
#'
#' @param tickers Character vector of explicit dated B3 futures tickers.
#' @param reference_date Optional date used only to disambiguate two-digit years
#'   for tickers absent from the official registry. Defaults to `Sys.Date()`.
#' @param registry Optional explicit registry, mainly for deterministic tests.
#' @param strict Whether unresolved rows should raise an error. With `FALSE`,
#'   unresolved rows are returned with a diagnostic `status`.
#' @param cal Optional `bizdays` calendar used only by pre-2018 estimators.
#' @return One canonical contract row per input ticker, in input order.
#' @export
brf_contract_resolve <- function(tickers,
                                 reference_date = NULL,
                                 registry = NULL,
                                 strict = TRUE,
                                 cal = NULL) {
  if (!is.character(tickers) || !length(tickers) || anyNA(tickers)) {
    stop("tickers must be a non-empty character vector without NA.",
      call. = FALSE
    )
  }
  tickers <- toupper(trimws(tickers))
  if (any(!nzchar(tickers))) {
    stop("tickers must not contain empty values.", call. = FALSE)
  }
  if (is.null(registry)) {
    registry <- brf_contract_registry()
  } else {
    registry <- .brf_contract_registry_normalize(
      registry, "registry"
    )
  }
  reference_dates <- .brf_contract_reference_dates(
    reference_date, length(tickers)
  )
  out <- .brf_contract_registry_empty(length(tickers))
  out$ticker <- tickers
  out$contract_symbol <- tickers
  official_index <- match(tickers, registry$ticker)
  official <- !is.na(official_index)
  if (any(official)) {
    out[official, ] <- registry[official_index[official], , drop = FALSE]
  }

  unresolved <- which(!official)
  if (length(unresolved)) {
    pattern <- "^([A-Z0-9]{2,8})([FGHJKMNQUVXZ])([0-9]{2})$"
    specs <- .brf_contract_root_specs()
    for (index in unresolved) {
      pieces <- regmatches(
        tickers[[index]], regexec(pattern, tickers[[index]], perl = TRUE)
      )[[1L]]
      if (length(pieces) != 4L) {
        out$status[[index]] <- "invalid_ticker"
        next
      }
      root <- pieces[[2L]]
      month <- unname(.brf_futures_month_map[[pieces[[3L]]]])
      year <- .brf_infer_contract_year(
        pieces[[4L]], reference_dates[[index]]
      )
      out$root[[index]] <- root
      out$contract_year[[index]] <- year
      out$contract_month[[index]] <- month
      if (is.na(year) || is.null(month) || is.na(month)) {
        out$status[[index]] <- "invalid_ticker"
        next
      }
      if (year >= .brf_contract_registry_cutover_year()) {
        out$status[[index]] <- "official_contract_not_found"
        next
      }
      spec_index <- match(root, specs$root)
      maturity <- .brf_maturity_date(root, year, month, cal = cal)
      last_trade <- .brf_last_trade_date(root, maturity, cal = cal)
      multiplier <- if (is.na(spec_index)) {
        NA_real_
      } else {
        specs$multiplier[[spec_index]]
      }
      if (is.na(spec_index) || is.na(maturity) || is.na(last_trade) ||
          !is.finite(multiplier) || multiplier <= 0) {
        out$status[[index]] <- "unsupported_historical_contract"
        next
      }
      out$maturity_date[[index]] <- maturity
      out$last_trade_date[[index]] <- last_trade
      out$multiplier[[index]] <- multiplier
      out$contract_size[[index]] <- specs$contract_size[[spec_index]]
      out$unit_of_measure[[index]] <- specs$unit_of_measure[[spec_index]]
      out$official[[index]] <- FALSE
      out$date_quality[[index]] <- "estimated_historical"
      out$maturity_rule[[index]] <- .brf_maturity_rule(root)
      out$last_trade_rule[[index]] <- .brf_last_trade_rule(root)
      out$source_type[[index]] <- "brfutures_historical_b3_rule"
      out$origin[[index]] <- "brfutures historical B3 rule estimator"
      out$status[[index]] <- "resolved"
    }
  }

  failures <- if (identical(strict, FALSE)) {
    integer()
  } else {
    which(is.na(out$status) | out$status != "resolved")
  }
  if (length(failures)) {
    details <- paste0(
      "`", out$ticker[failures], "` (", out$status[failures], ")"
    )
    if (length(details) > 5L) {
      details <- c(details[seq_len(5L)], paste0("... +", length(details) - 5L))
    }
    stop(
      "Canonical B3 contract metadata unavailable: ",
      paste(details, collapse = ", "), ".",
      call. = FALSE
    )
  }
  rownames(out) <- NULL
  out
}

#' Resolve B3 futures maturity dates from tickers
#'
#' @inheritParams brf_contract_resolve
#' @return A `Date` vector in ticker order.
#' @export
brf_maturity_from_ticker <- function(tickers,
                                     reference_date = NULL,
                                     registry = NULL,
                                     strict = TRUE,
                                     cal = NULL) {
  brf_contract_resolve(
    tickers = tickers,
    reference_date = reference_date,
    registry = registry,
    strict = strict,
    cal = cal
  )$maturity_date
}
