.brf_numeric_targets <- c(
  "contr_abert_1",
  "contr_fech_2",
  "num_negoc",
  "contr_negoc",
  "vol",
  "preco_abert",
  "preco_abertu",
  "preco_min",
  "preco_max",
  "preco_med",
  "ult_preco",
  "ajuste",
  "ajuste_anter",
  "ajuste_anter_3",
  "ajuste_ant",
  "ajuste_ant_3",
  "ajuste_corrig",
  "ajuste_corrig_4",
  "oscil",
  "osc",
  "var_ptos",
  "ult_of_compra",
  "ult_of_venda",
  "open_interest",
  "close_interest",
  "trade_count",
  "contracts_traded",
  "volume",
  "open",
  "high",
  "low",
  "average_price",
  "close",
  "settlement_price",
  "previous_settlement",
  "corrected_settlement",
  "change_percent",
  "change_points",
  "last_bid",
  "last_ask"
)

.brf_regular_treatment <- function(df, ...) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }
  norm_names <- vapply(names(df), .brf_sanitize_colname, character(1), USE.NAMES = FALSE)
  convert_idx <- norm_names %in% .brf_numeric_targets
  if (any(convert_idx)) {
    df[convert_idx] <- lapply(df[convert_idx], .brf_pt_number)
  }
  df
}

.brf_standardize_names <- function(columns) {
  if (!length(columns)) {
    return(columns)
  }
  sanitized <- vapply(columns, .brf_sanitize_colname, character(1), USE.NAMES = FALSE)
  mapping <- c(
    vencto = "contract_code",
    contr_abert_1 = "open_interest",
    contr_fech_2 = "close_interest",
    num_negoc = "trade_count",
    contr_negoc = "contracts_traded",
    vol = "volume",
    preco_abert = "open",
    preco_abertu = "open",
    preco_min = "low",
    preco_max = "high",
    preco_med = "average_price",
    ult_preco = "close",
    ajuste = "settlement_price",
    ajuste_anter = "previous_settlement",
    ajuste_anter_3 = "previous_settlement",
    ajuste_ant = "previous_settlement",
    ajuste_ant_3 = "previous_settlement",
    ajuste_corrig = "corrected_settlement",
    ajuste_corrig_4 = "corrected_settlement",
    oscil = "change_percent",
    osc = "change_percent",
    var_ptos = "change_points",
    ult_of_compra = "last_bid",
    ult_of_venda = "last_ask"
  )
  updated <- columns
  matched <- sanitized %in% names(mapping)
  if (any(matched)) {
    updated[matched] <- mapping[sanitized[matched]]
  }
  updated
}

.brf_standard_treatment <- function(df, ...) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }
  out <- .brf_regular_treatment(df, ...)
  names(out) <- .brf_standardize_names(names(out))
  dup <- duplicated(names(out))
  if (any(dup)) {
    out <- out[, !dup, drop = FALSE]
  }
  out
}

.brf_treatment_raw <- function(df, ...) {
  df
}

.brf_treatment_raw_tibble <- function(df, ...) {
  tibble::as_tibble(.brf_treatment_raw(df, ...))
}

.brf_treatment_regular_tibble <- function(df, ...) {
  tibble::as_tibble(.brf_regular_treatment(df, ...))
}

.brf_treatment_standard <- function(df, ...) {
  .brf_standard_treatment(df, ...)
}

.brf_treatment_standard_tibble <- function(df, ...) {
  tibble::as_tibble(.brf_standard_treatment(df, ...))
}

.brf_treatment_standard_xts <- function(df, ...) {
  std <- .brf_standard_treatment(df, ...)
  if (!nrow(std) || !"date" %in% names(std)) {
    return(xts::xts())
  }
  order_dates <- as.Date(std$date)
  keep_cols <- intersect(c("open", "high", "low", "close", "volume", "contracts_traded"), names(std))
  if (!length(keep_cols)) {
    return(xts::xts(order.by = order_dates[integer(0)]))
  }
  numeric_data <- std[keep_cols]
  numeric_data[] <- lapply(numeric_data, function(col) {
    if (is.numeric(col)) col else suppressWarnings(as.numeric(col))
  })
  matrix_data <- as.matrix(numeric_data)
  col_map <- c(
    open = "Open",
    high = "High",
    low = "Low",
    close = "Close",
    volume = "Volume",
    contracts_traded = "Volume_Qty"
  )
  colnames(matrix_data) <- col_map[keep_cols]
  valid <- !is.na(order_dates)
  matrix_data <- matrix_data[valid, , drop = FALSE]
  order_dates <- order_dates[valid]
  xts::xts(matrix_data, order.by = order_dates)
}

.brf_treatment_ohlcv_xts <- function(df, ...) {
  .brf_treatment_standard_xts(df, ...)
}

.brf_builtin_treatments <- function() {
  list(
    raw = .brf_treatment_raw,
    raw_tibble = .brf_treatment_raw_tibble,
    regular = .brf_regular_treatment,
    regular_tibble = .brf_treatment_regular_tibble,
    standard = .brf_treatment_standard,
    standard_tibble = .brf_treatment_standard_tibble,
    standard_xts = .brf_treatment_standard_xts,
    ohlcv_xts = .brf_treatment_ohlcv_xts
  )
}

.brf_resolve_treatment <- function(treatment) {
  if (is.function(treatment)) {
    return(treatment)
  }
  if (is.character(treatment) && length(treatment)) {
    key <- tolower(treatment[1])
    options <- .brf_builtin_treatments()
    if (key %in% names(options)) {
      return(options[[key]])
    }
    stop(
      "Unknown treatment '", treatment[1], "'. Available: ",
      paste(names(options), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  stop("Treatment must be a function or the name of a built-in treatment.", call. = FALSE)
}
