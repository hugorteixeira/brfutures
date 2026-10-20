.brf_bvbg_now <- function() Sys.time()

.brf_bvbg_attempt_path <- function(date) {
  file.path(.brf_bvbg_year_dir(format(as.Date(date), "%Y")),
            paste0(as.Date(date), "-attempt.rds"))
}

.brf_bvbg_attempt_read <- function(date) {
  path <- .brf_bvbg_attempt_path(date)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

.brf_bvbg_retry_due <- function(date, now = .brf_bvbg_now()) {
  attempt <- .brf_bvbg_attempt_read(date)
  if (identical(attempt$status, "available")) return(TRUE)
  age <- as.numeric(difftime(now, attempt$at %||% as.POSIXct(NA), units = "secs"))
  is.na(age) || age >= 6 * 3600 || age < 0
}

.brf_bvbg_refresh_due <- function(date, cached, now = .brf_bvbg_now()) {
  if (!inherits(cached, "data.frame")) return(FALSE)
  today <- as.Date(now, tz = "America/Sao_Paulo")
  if (as.Date(date) > today || as.Date(date) < today - 7) return(FALSE)
  attempt <- .brf_bvbg_attempt_read(date)
  checked <- attempt$at %||% attr(cached, "brf_parsed_at", exact = TRUE)
  age <- as.numeric(difftime(now, checked %||% as.POSIXct(NA), units = "secs"))
  if (!is.null(attempt) && !identical(attempt$status, "available")) return(is.na(age) || age >= 6 * 3600 || age < 0)
  is.na(age) || as.Date(checked, tz = "America/Sao_Paulo") < today || age < 0
}

.brf_no_data_write <- function(entries, path) {
  pending <- tempfile(".no-data-", tmpdir = dirname(path))
  on.exit(unlink(pending), add = TRUE)
  utils::write.csv(entries, pending, row.names = FALSE, quote = TRUE)
  if (!file.rename(pending, path)) stop("Cannot atomically publish no-data registry.", call. = FALSE)
  invisible(path)
}

.brf_bvbg_clear_unavailable <- function(date) {
  for (source in c("xml", "zip")) {
    path <- .brf_no_data_user_path(create = FALSE, source = source)
    if (!file.exists(path)) next
    entries <- .brf_user_no_data_entries(source)
    keep <- entries$date != as.Date(date)
    if (any(!keep)) .brf_no_data_write(entries[keep, c("root", "date"), drop = FALSE], path)
  }
  invisible(NULL)
}
