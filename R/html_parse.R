.brf_file_has_no_data_message <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  doc <- tryCatch(
    xml2::read_html(path, encoding = "windows-1252"),
    error = function(e) NULL
  )
  if (!is.null(doc) && .brf_no_data_message(doc)) {
    return(TRUE)
  }
  raw_text <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  if (.brf_contains_no_data_phrase(raw_text)) {
    return(TRUE)
  }
  fallback <- tryCatch(
    readLines(path, warn = FALSE, encoding = "latin1"),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  .brf_contains_no_data_phrase(fallback)
}

.brf_extract_root_options <- function(doc) {
  if (is.null(doc)) {
    return(character())
  }
  nodes <- xml2::xml_find_all(doc, "//select[@name='cboMercadoria' or @id='comboMerc1']//option")
  if (!length(nodes)) {
    return(character())
  }
  attr_vals <- trimws(xml2::xml_attr(nodes, "value"))
  text_vals <- trimws(xml2::xml_text(nodes))
  text_roots <- vapply(strsplit(text_vals, ":", fixed = TRUE), function(parts) {
    if (!length(parts)) {
      return("")
    }
    trimws(parts[1])
  }, character(1))
  values <- unique(c(attr_vals, text_roots))
  values <- trimws(values)
  values <- values[nzchar(values)]
  values <- gsub("\\s+", "", values, perl = TRUE)
  unique(toupper(values))
}

.brf_root_available_in_doc <- function(doc, root) {
  root_norm <- .brf_normalize_root(root)
  options <- .brf_extract_root_options(doc)
  if (!length(options)) {
    return(TRUE)
  }
  root_norm %in% options
}

.brf_extract_report_date <- function(path, doc) {
  nodes <- xml2::xml_find_all(doc, "//td[contains(translate(., 'atualizado', 'ATUALIZADO'), 'ATUALIZADO EM')]")
  for (node in nodes) {
    text <- xml2::xml_text(node)
    guess <- try(.brf_extract_iso_date(text), silent = TRUE)
    if (!inherits(guess, "try-error") && !is.na(guess)) {
      return(guess)
    }
  }
  file_name <- basename(path)
  match <- regexpr("(\\d{4}-\\d{2}-\\d{2})", file_name, perl = TRUE)
  if (match[1L] != -1L) {
    found <- regmatches(file_name, match)
    parsed <- as.Date(found)
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  NA
}

.brf_table_node_to_df <- function(node, element_id = NULL) {
  rows <- xml2::xml_find_all(node, ".//tr")
  if (!length(rows)) {
    return(NULL)
  }
  extract_row <- function(row) {
    cells <- xml2::xml_find_all(row, ".//th|.//td")
    if (!length(cells)) {
      return(character())
    }
    vals <- xml2::xml_text(cells)
    vals <- gsub("\u00A0", " ", vals, fixed = TRUE)
    trimws(vals)
  }
  row_vals <- lapply(rows, extract_row)
  max_cols <- max(lengths(row_vals))
  if (!max_cols) {
    return(NULL)
  }
  pad_row <- function(vals) {
    if (length(vals) < max_cols) {
      c(vals, rep("", max_cols - length(vals)))
    } else {
      vals
    }
  }
  row_vals <- lapply(row_vals, pad_row)
  mat <- do.call(rbind, row_vals)
  header <- mat[1, , drop = TRUE]
  header <- gsub("\u00A0", " ", header, fixed = TRUE)
  header <- trimws(header)
  header[!nzchar(header)] <- paste0("V", which(!nzchar(header)))
  mat <- mat[-1, , drop = FALSE]
  if (!nrow(mat)) {
    return(NULL)
  }
  df <- as.data.frame(mat, stringsAsFactors = FALSE, optional = TRUE)
  names(df) <- header
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      trimws(gsub("\u00A0", " ", col, fixed = TRUE))
    } else {
      col
    }
  })
  df
}

.brf_parse_table_node <- function(node) {
  .brf_table_node_to_df(node)
}

.brf_extract_table_from_js <- function(doc, element_id) {
  scripts <- xml2::xml_find_all(doc, "//script")
  if (!length(scripts)) {
    return(NULL)
  }
  script_texts <- vapply(scripts, xml2::xml_text, character(1), USE.NAMES = FALSE)
  combined <- paste(script_texts, collapse = "\n")
  assign_pattern <- sprintf("%s\\s*\\.innerHTML\\s*=\\s*([A-Za-z0-9_]+);", element_id)
  assignments <- regmatches(combined, gregexpr(assign_pattern, combined, perl = TRUE))[[1]]
  if (!length(assignments)) {
    return(NULL)
  }
  vars <- sub(assign_pattern, "\\1", assignments)
  vars <- vars[nzchar(vars)]
  vars <- unique(vars)
  if (!length(vars)) {
    return(NULL)
  }
  for (target_var in vars) {
    line_pattern <- sprintf("%1$s\\s*=\\s*%1$s\\s*\\+\\s*'([^']*)';", target_var)
    pieces <- regmatches(combined, gregexpr(line_pattern, combined, perl = TRUE))[[1]]
    if (!length(pieces)) {
      next
    }
    extract_piece <- function(x) sub(line_pattern, "\\1", x, perl = TRUE)
    html_fragments <- vapply(pieces, extract_piece, character(1), USE.NAMES = FALSE)
    if (!length(html_fragments)) {
      next
    }
    html_string <- paste(html_fragments, collapse = "")
    html_string <- gsub("\\\\'", "'", html_string, fixed = FALSE)
    html_string <- gsub("\\\\\"", "\"", html_string, fixed = FALSE)
    html_string <- gsub("(?i)</tr>\\s*<td", "<tr><td", html_string, perl = TRUE)
    recovered <- sprintf("<html><body>%s</body></html>", html_string)
    recovered_doc <- xml2::read_html(recovered)
    recovered_node <- xml2::xml_find_first(recovered_doc, "//table")
    if (inherits(recovered_node, "xml_missing")) {
      next
    }
    tbl <- .brf_table_node_to_df(recovered_node, element_id = element_id)
    if (!is.null(tbl) && any(vapply(tbl, function(col) any(nzchar(as.character(col))), logical(1)))) {
      return(tbl)
    }
  }
  NULL
}

.brf_table_from_doc <- function(doc, id) {
  node <- xml2::xml_find_first(doc, sprintf("//*[@id='%s']//table", id))
  if (!inherits(node, "xml_missing")) {
    tbl <- .brf_parse_table_node(node)
    if (!is.null(tbl)) {
      return(tbl)
    }
  }
  .brf_extract_table_from_js(doc, id)
}

.brf_parse_html_report <- function(path, root) {
  root <- .brf_normalize_root(root)
  if (!file.exists(path)) {
    stop("File '", path, "' not found.", call. = FALSE)
  }
  doc <- xml2::read_html(path, encoding = "windows-1252")
  if (.brf_no_data_message(doc)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  if (!.brf_root_available_in_doc(doc, root)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  tables <- list(
    vencto = .brf_table_from_doc(doc, "MercadoFut0"),
    volume = .brf_table_from_doc(doc, "MercadoFut1"),
    prices = .brf_table_from_doc(doc, "MercadoFut2")
  )
  tables <- tables[!vapply(tables, is.null, logical(1))]
  if (!length(tables)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  order_key <- NULL
  if (!is.null(tables$vencto)) {
    vencto_cols <- names(tables$vencto)
    vencto_idx <- which(vapply(vencto_cols, .brf_sanitize_colname, character(1), USE.NAMES = FALSE) %in% c("vencto", "contract_code"))
    if (length(vencto_idx)) {
      order_key <- trimws(toupper(tables$vencto[[vencto_idx[1]]]))
    }
  }
  merged <- tables[[1L]]
  for (tbl in tables[-1L]) {
    tbl_cols <- names(tbl)
    tbl_idx <- which(vapply(tbl_cols, .brf_sanitize_colname, character(1), USE.NAMES = FALSE) %in% c("vencto", "contract_code"))
    if (!is.null(order_key) && length(tbl_idx)) {
      tbl_key <- trimws(toupper(tbl[[tbl_idx[1]]]))
      tbl <- tbl[match(order_key, tbl_key), , drop = FALSE]
    }
    new_cols <- setdiff(names(tbl), names(merged))
    for (col in new_cols) {
      merged[[col]] <- tbl[[col]]
    }
  }
  sanitized_cols <- vapply(names(merged), .brf_sanitize_colname, character(1), USE.NAMES = FALSE)
  code_idx <- which(sanitized_cols %in% c("contract_code", "vencto"))
  if (!length(code_idx)) {
    stop("Unable to locate contract codes in report ", path, call. = FALSE)
  }
  code_source <- merged[[code_idx[1]]]
  code_source <- trimws(as.character(code_source))
  merged <- merged[nzchar(code_source), , drop = FALSE]
  if (!nrow(merged)) {
    out <- .brf_empty_bulletin()
    attr(out, "brf_no_data") <- TRUE
    return(out)
  }
  code_source <- gsub("\\s+", "", code_source, perl = TRUE)
  merged$contract_code <- toupper(code_source)
  merged$contract_code <- gsub("\\s+", "", merged$contract_code, perl = TRUE)
  if (!is.null(root) && !is.na(root) && nzchar(root)) {
    valid <- nzchar(merged$contract_code)
    ticker <- merged$contract_code
    add_prefix <- valid & !startsWith(ticker, root)
    ticker[add_prefix] <- paste0(root, ticker[add_prefix])
    ticker[!valid] <- NA_character_
    merged$ticker <- ticker
  } else {
    merged$ticker <- merged$contract_code
  }
  merged$root <- root
  merged$date <- .brf_extract_report_date(path, doc)
  merged <- merged[, unique(c("date", "root", "contract_code", "ticker", names(merged)))]
  merged <- merged[order(merged$date, merged$contract_code), , drop = FALSE]
  rownames(merged) <- NULL
  merged
}
