.brf_ps_normalize_scalar <- function(x, label, allow_null = FALSE) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    if (allow_null) {
      return(NULL)
    }
    stop(label, " is required.", call. = FALSE)
  }
  out <- trimws(as.character(x))
  if (!nzchar(out)) {
    if (allow_null) {
      return(NULL)
    }
    stop(label, " is required.", call. = FALSE)
  }
  out
}

.brf_ps_normalize_timeframe <- function(timeframe) {
  if (is.null(timeframe) || length(timeframe) != 1L || is.na(timeframe)) {
    return(NULL)
  }
  tf <- trimws(as.character(timeframe))
  tf <- gsub("^_+", "", tf)
  if (!nzchar(tf)) {
    return(NULL)
  }
  tf
}

.brf_ps_normalize_config <- function(host, dbname, user, password, port) {
  host <- .brf_ps_normalize_scalar(host, "host", allow_null = TRUE)
  if (is.null(host)) {
    host <- "localhost"
  }
  dbname <- .brf_ps_normalize_scalar(dbname, "dbname", allow_null = TRUE)
  user <- .brf_ps_normalize_scalar(user, "user", allow_null = TRUE)
  password <- .brf_ps_normalize_scalar(password, "password", allow_null = TRUE)
  if (is.null(port) || length(port) != 1L || is.na(port)) {
    port <- 5432
  }
  port <- suppressWarnings(as.integer(port))
  if (is.na(port)) {
    stop("Port must be a single integer.", call. = FALSE)
  }
  list(
    host = host,
    dbname = dbname,
    user = user,
    password = password,
    port = port
  )
}

.brf_ps_require_packages <- function() {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required for PostgreSQL access.", call. = FALSE)
  }
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Package 'RPostgres' is required for PostgreSQL access.", call. = FALSE)
  }
  invisible(TRUE)
}

.brf_ps_connect <- function(host, dbname, user, password, port) {
  .brf_ps_require_packages()
  config <- .brf_ps_normalize_config(host, dbname, user, password, port)
  args <- list(drv = RPostgres::Postgres(), host = config$host, port = config$port)
  if (!is.null(config$dbname)) {
    args$dbname <- config$dbname
  }
  if (!is.null(config$user)) {
    args$user <- config$user
  }
  if (!is.null(config$password)) {
    args$password <- config$password
  }
  do.call(DBI::dbConnect, args)
}

.brf_ps_split_table <- function(table) {
  table <- .brf_ps_normalize_scalar(table, "table")
  parts <- strsplit(table, ".", fixed = TRUE)[[1L]]
  if (length(parts) > 2L) {
    stop("Table name must be 'table' or 'schema.table'.", call. = FALSE)
  }
  if (length(parts) == 2L) {
    if (!nzchar(parts[1L]) || !nzchar(parts[2L])) {
      stop("Table name must be 'table' or 'schema.table'.", call. = FALSE)
    }
    return(list(schema = parts[1L], table = parts[2L]))
  }
  list(schema = NULL, table = table)
}

.brf_ps_build_table_name <- function(base, suffix = NULL) {
  parts <- .brf_ps_split_table(base)
  table <- parts$table
  if (!is.null(suffix) && nzchar(suffix)) {
    table <- paste0(table, "_", suffix)
  }
  if (is.null(parts$schema)) {
    table
  } else {
    paste(parts$schema, table, sep = ".")
  }
}

.brf_ps_table_id <- function(table) {
  parts <- .brf_ps_split_table(table)
  if (is.null(parts$schema)) {
    return(parts$table)
  }
  DBI::Id(schema = parts$schema, table = parts$table)
}

.brf_ps_resolve_table <- function(con, table, timeframe = NULL) {
  tf <- .brf_ps_normalize_timeframe(timeframe)
  candidates <- if (is.null(tf)) {
    c(
      .brf_ps_build_table_name(table),
      .brf_ps_build_table_name(table, "daily"),
      .brf_ps_build_table_name(table, "1d"),
      .brf_ps_build_table_name(table, "1day")
    )
  } else {
    .brf_ps_build_table_name(table, tf)
  }
  candidates <- unique(candidates)
  for (candidate in candidates) {
    if (DBI::dbExistsTable(con, .brf_ps_table_id(candidate))) {
      return(candidate)
    }
  }
  stop("Table not found. Tried: ", paste(candidates, collapse = ", "), call. = FALSE)
}

.brf_ps_rename_column <- function(df, from, to) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }
  if (from %in% names(df) && from != to) {
    names(df)[names(df) == from] <- to
  }
  df
}

.brf_ps_parse_date <- function(x, label, allow_all_na = FALSE) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  x_chr <- as.character(x)
  out <- suppressWarnings(as.Date(x_chr))
  if (all(is.na(out)) && length(x_chr) && any(nzchar(x_chr))) {
    parsed <- suppressWarnings(as.POSIXct(x_chr, tz = "UTC"))
    if (any(!is.na(parsed))) {
      out <- as.Date(parsed)
    }
  }
  if (!allow_all_na && all(is.na(out)) && length(x_chr) && any(nzchar(x_chr))) {
    stop("Column '", label, "' could not be parsed as Date.", call. = FALSE)
  }
  out
}

.brf_ps_coerce_datetime <- function(x, label) {
  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(x)
  }
  x_chr <- as.character(x)
  has_time <- grepl("\\d{2}:\\d{2}", x_chr)
  if (any(has_time, na.rm = TRUE)) {
    parsed <- suppressWarnings(as.POSIXct(x_chr, tz = "UTC"))
    if (all(is.na(parsed)) && length(x_chr) && any(nzchar(x_chr))) {
      stop("Column '", label, "' could not be parsed as datetime.", call. = FALSE)
    }
    return(parsed)
  }
  .brf_ps_parse_date(x_chr, label, allow_all_na = FALSE)
}

.brf_ps_filter_bounds <- function(df, date_col, start, end) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }
  if (!date_col %in% names(df)) {
    stop("Date column '", date_col, "' not found in result set.", call. = FALSE)
  }
  bounds <- .brf_normalize_date_bounds(start, end)
  date_values <- .brf_ps_coerce_datetime(df[[date_col]], date_col)
  date_index <- as.Date(date_values)
  from <- if (is.null(bounds$start)) min(date_index, na.rm = TRUE) else bounds$start
  to <- bounds$end
  keep <- !is.na(date_index) & date_index >= from & date_index <= to
  out <- df[keep, , drop = FALSE]
  out[[date_col]] <- date_values[keep]
  out
}

.brf_ps_apply_maturity <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }
  if (!"maturity" %in% names(df)) {
    return(.brf_estimate_maturity(df))
  }
  parsed <- .brf_ps_parse_date(df$maturity, "maturity", allow_all_na = TRUE)
  df$maturity <- parsed
  if (any(is.na(parsed)) && "ticker" %in% names(df)) {
    estimated <- .brf_estimate_maturity(df)
    if ("maturity" %in% names(estimated)) {
      missing <- is.na(df$maturity)
      df$maturity[missing] <- estimated$maturity[missing]
    }
  }
  df
}

#' Retrieve futures data from PostgreSQL
#'
#' Queries a PostgreSQL table for specific tickers, applies the same treatments
#' as `get_brfut()`, and returns the filtered result.
#'
#' Connection defaults can be provided via:
#' `options(brfutures.postgres.host = "localhost",
#'          brfutures.postgres.dbname = NULL,
#'          brfutures.postgres.user = NULL,
#'          brfutures.postgres.password = NULL,
#'          brfutures.postgres.port = 5432)`
#'
#' @param ticker Character vector with contract tickers (e.g. `"WINZ24"`).
#' @param table Character scalar with the base table name (optionally schema-qualified).
#' @param ticker_col Column name that stores the contract ticker.
#' @param date_col Column name that stores the observation date/time.
#' @param root_col Optional column name that stores the contract root (renamed to `root`).
#' @param start,end Optional bounds restricting the returned dates.
#' @param treatment Either the name of a built-in treatment (e.g. `"raw"`,
#'   `"standard"`, `"ohlcv_xts"`) or a function that receives the raw data frame
#'   and returns the desired shape.
#' @param add_attrs When `TRUE` (default), enrich output with futures metadata.
#' @param tz Timezone used when returning `xts` objects. Defaults to
#'   `"America/Sao_Paulo"`.
#' @param keep_time When `TRUE` (default), keep the clock time when assigning the
#'   timezone (e.g. midnight stays midnight). When `FALSE`, shift timestamps to
#'   the target timezone.
#' @param timeframe Optional suffix used to locate a timeframe-specific table
#'   (e.g. `"1d"` -> `table_1d`).
#' @param host,dbname,user,password,port PostgreSQL connection parameters.
#' @param ... Additional arguments forwarded to the treatment function.
#'
#' @return The result of applying `treatment` to the filtered rows.
#' @export
get_psfut <- function(ticker,
                      table = "usa_futures_int_databento",
                      ticker_col = "ticker",
                      date_col = "date",
                      root_col = "root",
                      start = NULL,
                      end = NULL,
                      treatment = "ohlcv_drop0_xts",
                      add_attrs = TRUE,
                      tz = "America/Sao_Paulo",
                      keep_time = TRUE,
                      timeframe = "1d",
                      host = getOption("brfutures.postgres.host", "localhost"),
                      dbname = getOption("brfutures.postgres.dbname", NULL),
                      user = getOption("brfutures.postgres.user", NULL),
                      password = getOption("brfutures.postgres.password", NULL),
                      port = getOption("brfutures.postgres.port", 5432),
                      ...) {
  if (missing(ticker)) {
    stop("Argument `ticker` is required.", call. = FALSE)
  }
  ticker_text <- toupper(trimws(as.character(ticker)))
  ticker_text <- ticker_text[nzchar(ticker_text)]
  if (!length(ticker_text)) {
    stop("Argument `ticker` is required.", call. = FALSE)
  }
  if (missing(table)) {
    stop("Argument `table` is required.", call. = FALSE)
  }
  ticker_col <- .brf_ps_normalize_scalar(ticker_col, "ticker_col")
  date_col <- .brf_ps_normalize_scalar(date_col, "date_col")
  root_col <- .brf_ps_normalize_scalar(root_col, "root_col", allow_null = TRUE)

  con <- .brf_ps_connect(host, dbname, user, password, port)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  table_name <- .brf_ps_resolve_table(con, table, timeframe = timeframe)
  table_id <- DBI::dbQuoteIdentifier(con, .brf_ps_table_id(table_name))
  ticker_id <- DBI::dbQuoteIdentifier(con, ticker_col)
  ticker_vals <- DBI::dbQuoteString(con, ticker_text)
  where_clause <- paste0(
    "WHERE UPPER(",
    as.character(ticker_id),
    ") IN (",
    paste(ticker_vals, collapse = ", "),
    ")"
  )
  query <- paste("SELECT * FROM", as.character(table_id), where_clause)
  data <- DBI::dbGetQuery(con, query)

  if (!nrow(data)) {
    stop("Requested ticker(s) not found in table: ", table_name, call. = FALSE)
  }

  data <- .brf_ps_rename_column(data, ticker_col, "ticker")
  if (!is.null(root_col) && root_col %in% names(data)) {
    data <- .brf_ps_rename_column(data, root_col, "root")
  }
  data <- .brf_ps_rename_column(data, date_col, "date")
  required_cols <- c("ticker", "date")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  data$ticker <- toupper(trimws(as.character(data$ticker)))
  if ("root" %in% names(data)) {
    data$root <- toupper(trimws(as.character(data$root)))
  }

  data <- .brf_ps_filter_bounds(data, "date", start, end)
  data <- data[order(data$date, data$ticker), , drop = FALSE]

  treatment_fn <- .brf_resolve_treatment(treatment)
  finish <- treatment_fn(data, ...)
  estimated <- .brf_ps_apply_maturity(finish)
  if (add_attrs) {
    estimated <- .brf_add_futures_attrs(estimated, ticker)
  }
  if (xts::is.xts(estimated)) {
    estimated <- .brf_xts_apply_timezone(estimated, tz = tz, keep_time = keep_time)
  }
  estimated
}

#' Retrieve aggregated futures data from PostgreSQL
#'
#' Loads rows from a PostgreSQL table, optionally filtering by root, and applies
#' the same aggregate treatments as `get_brfut_agg()`.
#'
#' @param root Optional character vector restricting the returned roots.
#' @param table Character scalar with the base table name (optionally schema-qualified).
#' @param ticker_col Column name that stores the contract ticker.
#' @param root_col Column name that stores the contract root.
#' @param date_col Column name that stores the observation date/time.
#' @param start,end Date bounds. When omitted all matching rows are returned.
#' @param treatment Either the name of a built-in treatment (e.g. `"standard"`,
#'   `"regular"`, `"raw"`) or a function receiving the assembled data frame and
#'   returning the desired shape. Defaults to `"clean_data"`.
#' @param timeframe Optional suffix used to locate a timeframe-specific table
#'   (e.g. `"1d"` -> `table_1d`).
#' @param host,dbname,user,password,port PostgreSQL connection parameters.
#'
#' @return A data frame with every matching contract observation within the range.
#' @export
get_psfut_agg <- function(root = NULL,
                          table = "usa_futures_int_databento",
                          ticker_col = "ticker",
                          root_col = "root",
                          date_col = "date",
                          start = NULL,
                          end = NULL,
                          treatment = "clean_data",
                          timeframe = "1d",
                          host = getOption("brfutures.postgres.host", "localhost"),
                          dbname = getOption("brfutures.postgres.dbname", NULL),
                          user = getOption("brfutures.postgres.user", NULL),
                          password = getOption("brfutures.postgres.password", NULL),
                          port = getOption("brfutures.postgres.port", 5432)) {
  if (missing(table)) {
    stop("Argument `table` is required.", call. = FALSE)
  }
  ticker_col <- .brf_ps_normalize_scalar(ticker_col, "ticker_col")
  root_col <- .brf_ps_normalize_scalar(root_col, "root_col")
  date_col <- .brf_ps_normalize_scalar(date_col, "date_col")

  filter_roots <- .brf_normalize_root_vector(root)

  con <- .brf_ps_connect(host, dbname, user, password, port)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  table_name <- .brf_ps_resolve_table(con, table, timeframe = timeframe)
  table_id <- DBI::dbQuoteIdentifier(con, .brf_ps_table_id(table_name))

  if (length(filter_roots)) {
    root_id <- DBI::dbQuoteIdentifier(con, root_col)
    root_vals <- DBI::dbQuoteString(con, filter_roots)
    where_clause <- paste0(
      "WHERE UPPER(",
      as.character(root_id),
      ") IN (",
      paste(root_vals, collapse = ", "),
      ")"
    )
    query <- paste("SELECT * FROM", as.character(table_id), where_clause)
  } else {
    query <- paste("SELECT * FROM", as.character(table_id))
  }

  data <- DBI::dbGetQuery(con, query)
  if (!nrow(data)) {
    stop("No data available in table: ", table_name, call. = FALSE)
  }

  data <- .brf_ps_rename_column(data, ticker_col, "ticker")
  data <- .brf_ps_rename_column(data, root_col, "root")
  data <- .brf_ps_rename_column(data, date_col, "date")
  required_cols <- c("ticker", "root", "date")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  data$ticker <- toupper(trimws(as.character(data$ticker)))
  data$root <- toupper(trimws(as.character(data$root)))

  data <- .brf_ps_filter_bounds(data, "date", start, end)
  data <- .brf_normalize_old_tickers(data)
  data <- data[order(data$date, data$root, data$ticker), , drop = FALSE]

  treatment_fn <- .brf_resolve_agg_treatment(treatment)
  result <- treatment_fn(data)
  .brf_ps_apply_maturity(result)
}
