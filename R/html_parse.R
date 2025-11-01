.brf_parser_version <- function() {
  3L
}

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
  # Remove duplicate column names at the end (from two-row headers)
  if (any(duplicated(header))) {
    dup_indices <- which(duplicated(header))
    # Only remove if duplicates are at the end
    if (length(dup_indices) > 0 && min(dup_indices) > (length(header) / 2)) {
      keep <- !duplicated(header)
      header <- header[keep]
      if (nrow(mat) > 0 && ncol(mat) > length(header)) {
        mat <- mat[, seq_len(length(header)), drop = FALSE]
      }
    }
  }
  data_cols <- if (nrow(mat) > 0) ncol(mat) else length(header)
  expand_header_tokens <- function(x) {
    # First normalize whitespace: convert tabs and multiple spaces to single space
    fixed <- gsub("\\s+", " ", x, perl = TRUE)
    # Split before known column keywords ONLY when concatenated (no space before)
    # This handles "PREÇOAJUSTE" -> "PREÇO|AJUSTE" but keeps "ÚLT. PREÇO" as one token
    keywords <- c("AJUSTE", "VAR", "ÚLT", "ULT", "PREÇO", "PRECO", "OF", "COMPRA", "VENDA", "PTOS")
    for (kw in keywords) {
      # Match keyword when preceded directly by a letter (no space)
      pattern <- sprintf("(?<=[A-ZÁÉÍÓÚÃÕÇ])(?=%s)", kw)
      fixed <- gsub(pattern, "|", fixed, perl = TRUE)
    }
    # Split when lowercase followed by uppercase (word boundary)
    fixed <- gsub("(?<=\\p{Ll})(?=\\p{Lu})", "|", fixed, perl = TRUE)
    # Split after ) when immediately followed by letter (no space)
    fixed <- gsub("(?<=\\))(?!\\s)(?=\\p{L})", "|", fixed, perl = TRUE)
    # Split after . when followed by uppercase (not lowercase)
    fixed <- gsub("(?<=\\.)(?=\\p{Lu})(?![a-z])", "|", fixed, perl = TRUE)

    # Further expand tokens that look like multiple columns
    tokens <- strsplit(fixed, "\\|", fixed = FALSE)[[1]]
    tokens <- trimws(tokens)
    tokens <- tokens[nzchar(tokens)]

    expanded_tokens <- character()
    for (token in tokens) {
      word_count <- length(strsplit(token, " ")[[1]])

      # Known 2-word patterns that should stay together
      known_two_word <- c(
        "ÚLT. PREÇO", "VAR. PTOS.",
        "PREÇO ABERT.", "PREÇO MIN.", "PREÇO MAX.", "PREÇO MED.",
        "PREÇO MÍN.", "PREÇO MÁX.", "PREÇO MÉD.",
        "AJUSTE ANTER.", "AJUSTE CORRIG."
      )

      # If token has multiple words, try to group into known patterns
      if (word_count >= 2) {
        # Check if the entire token is a known pattern
        if (token %in% known_two_word || token %in% c("ÚLT. OF. COMPRA", "ÚLT. OF. VENDA")) {
          expanded_tokens <- c(expanded_tokens, token)
          next
        }

        # Otherwise split and try to match patterns
        parts <- strsplit(token, " ")[[1]]
        i <- 1
        while (i <= length(parts)) {
          # Try to match 3-word patterns first
          if (i + 2 <= length(parts)) {
            three_word <- paste(parts[i:(i+2)], collapse = " ")
            if (three_word %in% c("ÚLT. OF. COMPRA", "ÚLT. OF. VENDA")) {
              expanded_tokens <- c(expanded_tokens, three_word)
              i <- i + 3
              next
            }
          }
          # Try to match 2-word patterns
          if (i + 1 <= length(parts)) {
            two_word <- paste(parts[i:(i+1)], collapse = " ")
            if (two_word %in% known_two_word) {
              expanded_tokens <- c(expanded_tokens, two_word)
              i <- i + 2
              next
            }
          }
          # No pattern matched, add single word
          expanded_tokens <- c(expanded_tokens, parts[i])
          i <- i + 1
        }
      } else {
        # Single word token, keep as-is
        expanded_tokens <- c(expanded_tokens, token)
      }
    }
    expanded_tokens
  }
  # Don't expand yet - wait until after two-row header detection
  if (!nrow(mat)) {
    return(NULL)
  }
  df <- as.data.frame(mat, stringsAsFactors = FALSE, optional = TRUE)
  names(df) <- header
  if (nrow(df)) {
    first_row <- df[1, , drop = TRUE]
    non_empty <- nzchar(first_row)
    letter_hits <- if (any(non_empty)) {
      sum(grepl("[A-Za-z]", first_row[non_empty]))
    } else {
      0L
    }
    total_hits <- if (any(non_empty)) sum(non_empty) else 0L
    text_ratio <- if (total_hits > 0L) letter_hits / total_hits else 0
    if (letter_hits >= 2L && text_ratio >= 0.5) {
      old_header <- names(df)
      new_header <- trimws(first_row)
      new_header[!nzchar(new_header)] <- old_header[!nzchar(new_header)]

      # Check for duplicate columns at the end (suggests two-row header)
      dup_at_end <- duplicated(new_header) | duplicated(new_header, fromLast = TRUE)
      tail_dups <- tail(which(dup_at_end), n = sum(tail(dup_at_end, 5)))

      # If we have duplicates at the end that match columns from the beginning,
      # it's likely a two-row header - remove the duplicates
      if (length(tail_dups) > 0 && all(tail_dups > (length(new_header) - 5))) {
        # Keep only unique columns
        new_header <- new_header[!duplicated(new_header)]
        # Trim data to match
        if (length(new_header) < ncol(df)) {
          df <- df[, seq_len(length(new_header)), drop = FALSE]
        }
      }

      # Handle rowspan: if old header had meaningful first column (like VENCTO)
      # and new header doesn't start with it, shift new header right
      first_col_meaningful <- nzchar(old_header[1]) &&
        !grepl("^(V[0-9]+|Cotacoes)$", old_header[1])
      new_starts_with_price <- grepl("^PRECO|^ULT|^AJUSTE|^VAR", new_header[1], ignore.case = TRUE)

      if (first_col_meaningful && new_starts_with_price) {
        # Shift new header to the right and insert old first column
        new_header <- c(old_header[1], new_header[-length(new_header)])
      }

      names(df) <- new_header
      df <- df[-1, , drop = FALSE]
    }
  }

  # Clean up concatenated headers before expansion
  # If a header contains tabs or is suspiciously long, extract just the first token
  current_headers <- names(df)
  cleaned_headers <- sapply(current_headers, function(h) {
    # Check if header contains tabs or is very long (likely concatenated)
    if (grepl("\t", h) || nchar(h) > 50) {
      # Split on tabs first
      parts <- strsplit(h, "\t+")[[1]]
      # Get first non-empty part
      first_part <- trimws(parts[nzchar(trimws(parts))])[1]
      if (is.na(first_part) || !nchar(first_part)) {
        first_part <- h
      }

      # Now extract the FIRST token from this part
      # Use the expand function to properly split concatenated keywords
      tokens <- expand_header_tokens(first_part)
      if (length(tokens) > 0) {
        # Take first token, but if it's followed by recognized patterns, include them
        # E.g., "AJUSTE" + "ANTER." = "AJUSTE ANTER."
        if (length(tokens) >= 2) {
          # Check if first two tokens form a known pattern
          two_token <- paste(tokens[1:2], collapse = " ")
          known_patterns <- c("AJUSTE ANTER.", "AJUSTE CORRIG.", "PREÇO ABERT.", "PREÇO MIN.", "PREÇO MAX.", "PREÇO MED.", "PREÇO MÍN.", "PREÇO MÁX.", "PREÇO MÉD.")
          if (two_token %in% known_patterns) {
            return(two_token)
          }
        }
        return(tokens[1])
      }
    }
    h
  }, USE.NAMES = FALSE)

  # Now expand the cleaned headers
  expanded <- unlist(lapply(cleaned_headers, expand_header_tokens), use.names = FALSE)

  # Use expanded headers if they better match the number of data columns
  if (length(expanded) == ncol(df)) {
    names(df) <- expanded
  } else if (length(expanded) > length(current_headers) && length(expanded) >= ncol(df)) {
    names(df) <- head(expanded, ncol(df))
  }

  if (any(names(df) == "Cotacoes")) {
    cot_idx <- which(names(df) == "Cotacoes")[1]
    price_names <- c("PRECO ABERT.", "PRECO MIN.", "PRECO MAX.", "ULT. PRECO", "AJUSTE")
    span <- min(length(price_names), ncol(df) - cot_idx + 1L)
    if (span > 0) {
      target_idx <- cot_idx + seq_len(span) - 1L
      target_idx <- target_idx[target_idx <= ncol(df)]
      names(df)[target_idx] <- price_names[seq_len(length(target_idx))]
    }
  }
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
    # Try single quotes first
    line_pattern <- sprintf("%1$s\\s*=\\s*%1$s\\s*\\+\\s*'([^']*)';", target_var)
    pieces <- regmatches(combined, gregexpr(line_pattern, combined, perl = TRUE))[[1]]

    # If no match with single quotes, try double quotes
    if (!length(pieces)) {
      line_pattern <- sprintf('%1$s\\s*=\\s*%1$s\\s*\\+\\s*"([^"]*)";', target_var)
      pieces <- regmatches(combined, gregexpr(line_pattern, combined, perl = TRUE))[[1]]
    }

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

.brf_clean_parsed_dataframe <- function(df, root, source_path) {
  if (!inherits(df, "data.frame")) {
    stop("Parsed report must be a data frame.", call. = FALSE)
  }
  root_norm <- .brf_normalize_root(root)
  raw_original <- names(df)
  if (!length(raw_original)) {
    df$date <- as.Date(df$date)
    df$root <- root_norm
    attr(df, "brf_raw_header_map") <- stats::setNames(character(), character())
    attr(df, "brf_raw_headers") <- character()
    attr(df, "brf_report_date") <- .brf_extract_report_date_from_name(source_path)
    return(df)
  }
  sanitized <- vapply(raw_original, .brf_sanitize_colname, character(1), USE.NAMES = FALSE)
  numeric_idx <- sanitized %in% .brf_numeric_targets
  if (any(numeric_idx)) {
    df[numeric_idx] <- lapply(df[numeric_idx], .brf_pt_number)
  }
  token_targets <- c(
    preco_med = "average_price",
    ult_preco = "close",
    ajuste = "settlement_price",
    ajuste_anter = "previous_settlement",
    ajuste_corrig = "corrected_settlement",
    var_ptos = "change_points",
    ult_of_compra = "last_bid",
    ult_of_venda = "last_ask"
  )
  canonical <- .brf_standardize_names(raw_original)
  fallback <- sanitized
  identical_raw <- canonical == raw_original
  canonical[identical_raw] <- fallback[identical_raw]
  canonical[!nzchar(canonical)] <- fallback[!nzchar(canonical)]

  # Only apply token_hits to columns that weren't successfully standardized
  # If canonical != sanitized, then standardization succeeded, so skip token matching
  successfully_standardized <- canonical != sanitized
  token_hits <- lapply(seq_along(sanitized), function(i) {
    if (successfully_standardized[i]) {
      # Skip token matching for already-standardized columns
      character()
    } else {
      # Apply token matching for composite/unstandardized columns
      hits <- token_targets[vapply(names(token_targets), function(token) grepl(token, sanitized[i], fixed = TRUE), logical(1))]
      hits
    }
  })
  dup <- duplicated(canonical)
  keep_idx <- !dup
  if (any(dup)) {
    df <- df[, keep_idx, drop = FALSE]
    canonical <- canonical[keep_idx]
    raw_original <- raw_original[keep_idx]
    token_hits <- token_hits[keep_idx]
  }
  names(df) <- canonical
  header_map <- stats::setNames(raw_original, canonical)
  post_sanitized <- vapply(names(df), .brf_sanitize_colname, character(1), USE.NAMES = FALSE)
  convert_idx <- post_sanitized %in% .brf_numeric_targets
  if (any(convert_idx)) {
    df[convert_idx] <- lapply(df[convert_idx], .brf_pt_number)
  }
  if (length(token_hits)) {
    names(token_hits) <- canonical
    cols_to_remove <- integer()
    for (idx in seq_along(token_hits)) {
      hits <- token_hits[[idx]]
      if (!length(hits)) {
        next
      }
      col_pos <- idx
      base_target <- hits[1]
      old_name <- canonical[idx]
      names(df)[col_pos] <- base_target
      header_map[base_target] <- header_map[old_name]
      if (!identical(old_name, base_target)) {
        header_map <- header_map[setdiff(names(header_map), old_name)]
      }
      canonical[idx] <- base_target
      if (!is.numeric(df[[col_pos]])) {
        df[[col_pos]] <- .brf_pt_number(df[[col_pos]])
      }
      if (length(hits) == 1L) {
        next
      }
      base_header_raw <- unname(header_map[base_target])
      for (extra_idx in seq(2, length(hits))) {
        target <- hits[extra_idx]
        pos <- col_pos + extra_idx - 1L
        if (pos > ncol(df)) {
          break
        }
        values <- df[[pos]]
        if (!is.numeric(values)) {
          values <- .brf_pt_number(values)
        }
        df[[target]] <- values
        header_map[target] <- base_header_raw
        # Mark the source column for removal since we copied its value
        cols_to_remove <- c(cols_to_remove, pos)
      }
    }
    # Remove columns that were expanded from composite headers
    if (length(cols_to_remove)) {
      cols_to_remove <- unique(cols_to_remove)
      # Sort in descending order to remove from right to left
      cols_to_remove <- sort(cols_to_remove, decreasing = TRUE)
      for (pos in cols_to_remove) {
        if (pos <= ncol(df)) {
          old_col_name <- names(df)[pos]
          df <- df[, -pos, drop = FALSE]
          if (old_col_name %in% names(header_map)) {
            header_map <- header_map[names(header_map) != old_col_name]
          }
        }
      }
    }
  }
  if (!"date" %in% names(df)) {
    df$date <- .brf_extract_report_date_from_name(source_path)
  }
  df$date <- as.Date(df$date)
  inferred_date <- .brf_extract_report_date_from_name(source_path)
  if (is.na(inferred_date)) {
    date_candidates <- df$date[!is.na(df$date)]
    if (length(date_candidates)) {
      inferred_date <- date_candidates[1]
    }
  }
  missing_date <- is.na(df$date)
  if (any(missing_date)) {
    df$date[missing_date] <- inferred_date
  }
  df$root <- root_norm
  if (!"contract_code" %in% names(df)) {
    stop("Standardized report is missing 'contract_code' values for ", basename(source_path), ".", call. = FALSE)
  }
  df$contract_code <- toupper(trimws(as.character(df$contract_code)))
  df$contract_code <- gsub("\\s+", "", df$contract_code, perl = TRUE)
  df <- df[nzchar(df$contract_code), , drop = FALSE]
  if (!nrow(df)) {
    attr(df, "brf_raw_header_map") <- header_map
    attr(df, "brf_raw_headers") <- unname(header_map)
    attr(df, "brf_report_date") <- inferred_date
    df$root <- root_norm
    return(df)
  }
  valid_codes <- nzchar(df$contract_code)
  if (!"ticker" %in% names(df)) {
    df$ticker <- df$contract_code
  }
  df$ticker <- toupper(trimws(as.character(df$ticker)))
  df$ticker <- gsub("\\s+", "", df$ticker, perl = TRUE)
  add_prefix <- valid_codes & !startsWith(df$ticker, root_norm)
  df$ticker[add_prefix] <- paste0(root_norm, df$ticker[add_prefix])
  df$ticker[!valid_codes] <- NA_character_
  df <- df[order(df$date, df$contract_code, df$ticker), , drop = FALSE]
  df <- .brf_deduplicate_contract_rows(df)
  col_order <- unique(c("date", "root", "contract_code", "ticker", names(df)))
  df <- df[, col_order, drop = FALSE]
  rownames(df) <- NULL
  attr(df, "brf_raw_header_map") <- header_map
  attr(df, "brf_raw_headers") <- unname(header_map)
  if (!"date" %in% names(df)) {
    attr(df, "brf_report_date") <- inferred_date
  } else {
    date_candidates <- df$date[!is.na(df$date)]
    attr(df, "brf_report_date") <- if (length(date_candidates)) date_candidates[1] else inferred_date
  }
  df
}

.brf_parse_html_report_clean <- function(path, root) {
  result <- .brf_parse_html_report(path, root)
  if (is.null(result)) {
    return(NULL)
  }
  root_norm <- .brf_normalize_root(root)
  report_date <- .brf_extract_report_date_from_name(path)
  if (isTRUE(attr(result, "brf_no_data"))) {
    attr(result, "brf_parser_version") <- .brf_parser_version()
    attr(result, "brf_source_html") <- path
    attr(result, "brf_root") <- root_norm
    attr(result, "brf_report_date") <- report_date
    attr(result, "brf_parsed_at") <- Sys.time()
    attr(result, "brf_raw_header_map") <- stats::setNames(character(), character())
    attr(result, "brf_raw_headers") <- character()
    return(result)
  }
  cleaned <- .brf_clean_parsed_dataframe(result, root_norm, path)
  if (is.null(attr(cleaned, "brf_report_date", exact = TRUE))) {
    attr(cleaned, "brf_report_date") <- report_date
  }
  attr(cleaned, "brf_parser_version") <- .brf_parser_version()
  attr(cleaned, "brf_source_html") <- path
  attr(cleaned, "brf_root") <- root_norm
  attr(cleaned, "brf_parsed_at") <- Sys.time()
  cleaned
}
