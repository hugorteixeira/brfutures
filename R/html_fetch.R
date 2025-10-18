.brf_bulletin_url <- function(date, root) {
  root <- .brf_normalize_root(root)
  date <- .brf_normalize_date(date)
  base <- "https://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/SistemaPregao1.asp"
  query <- paste0(
    "pagetype=pop",
    "&caminho=Resumo%20Estat%EDstico%20-%20Sistema%20Preg%E3o",
    "&Data=", utils::URLencode(format(date, "%d/%m/%Y"), reserved = TRUE),
    "&Mercadoria=", utils::URLencode(root, reserved = TRUE)
  )
  paste(base, query, sep = "?")
}

.brf_download_html <- function(date, root, quiet = FALSE) {
  root <- .brf_normalize_root(root)
  date <- .brf_normalize_date(date)
  raw_dir <- .brf_raw_dir(root)
  file_name <- paste0(root, "_", format(date, "%Y-%m-%d"), ".html")
  dest <- file.path(raw_dir, file_name)
  if (file.exists(dest)) {
    if (!quiet) {
      message("Already cached: ", dest)
    }
    return(dest)
  }
  url <- .brf_bulletin_url(date, root)
  resp <- httr::RETRY(
    verb = "GET",
    url = url,
    httr::user_agent("brfutures (https://github.com/hugorteixeira/brfutures)"),
    times = 3,
    pause_base = 1,
    terminate_on = c(400, 404, 500)
  )
  task_label <- paste("download", root, format(date, "%Y-%m-%d"))
  httr::stop_for_status(resp, task = task_label)
  content <- httr::content(resp, as = "raw")
  writeBin(content, dest)
  if (!quiet) {
    message("Saved ", root, " report for ", format(date, "%Y-%m-%d"))
  }
  dest
}
