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
# Core builder (unchanged): returns raw OHLCV xts
.brf_treatment_standard_xts <- function(df, ...) {
  std <- .brf_standard_treatment(df, ...)
  if (!nrow(std) || !"date" %in% names(std)) {
    return(xts::xts())
  }

  # Ensure daily Date index
  order_dates <- as.Date(std$date)

  # Keep only expected columns if present
  keep_cols <- intersect(
    c("open", "high", "low", "close", "volume", "contracts_traded"),
    names(std)
  )
  if (!length(keep_cols)) {
    return(xts::xts(order.by = order_dates[integer(0)]))
  }

  # Force numeric quietly
  numeric_data <- std[keep_cols]
  numeric_data[] <- lapply(numeric_data, function(col) {
    if (is.numeric(col)) col else suppressWarnings(as.numeric(col))
  })

  # Build matrix and canonical names
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

  # Drop invalid dates only (no semantics change here)
  valid <- !is.na(order_dates)
  xts::xts(matrix_data[valid, , drop = FALSE], order.by = order_dates[valid])
}

# Helper: drop rows where ALL selected columns are zero (within tolerance)
.brf_drop_all_zero_rows_xts <- function(x, cols = NULL, eps = 0) {
  if (NROW(x) == 0) {
    return(x)
  }
  if (is.null(cols)) cols <- colnames(x)
  cols <- intersect(cols, colnames(x))
  if (!length(cols)) {
    return(x)
  }

  m <- as.matrix(x[, cols, drop = FALSE])
  all_zero <- (rowSums(abs(m) <= eps, na.rm = TRUE) == ncol(m)) &
    (rowSums(is.na(m)) == 0)
  x[!all_zero, ]
}

# RAW: keep zeros and NAs as-is (reference behavior)
.brf_treatment_ohlcv_xts <- function(df, ...) {
  .brf_treatment_standard_xts(df, ...)
}

# DROP0: default to test only OHLC (do not consider Volume by default)
.brf_treatment_ohlcv_drop0_xts <- function(
  df, ...,
  cols = c("Open", "High", "Low", "Close"), # default scope
  eps = 0 # numeric tolerance
) {
  x <- .brf_treatment_standard_xts(df, ...)
  .brf_drop_all_zero_rows_xts(x, cols = cols, eps = eps)
}

# LOCF: by default treat all-zero OHLC rows as NA, then LOCF only OHLC
.brf_treatment_ohlcv_locf_xts <- function(
  df, ...,
  zero_to_na_first = TRUE, # key default: zeros -> NA
  cols = c("Open", "High", "Low", "Close"), # fill only OHLC by default
  fromLast = FALSE, # forward fill
  na_rm = TRUE, # remove leading NAs before fill
  eps = 0 # zero tolerance for detection
) {
  x <- .brf_treatment_standard_xts(df, ...)
  if (NROW(x) == 0) {
    return(x)
  }

  cols <- intersect(cols, colnames(x))

  # Convert "all-zero" rows to NA (for selected columns) before LOCF
  if (zero_to_na_first && length(cols)) {
    m <- as.matrix(x[, cols, drop = FALSE])
    all_zero <- (rowSums(abs(m) <= eps, na.rm = TRUE) == ncol(m)) &
      (rowSums(is.na(m)) == 0)
    x[all_zero, cols] <- NA_real_
  }

  # Apply LOCF only to selected columns (keeps Volume untouched by default)
  if (length(cols) && length(cols) < ncol(x)) {
    x_fill <- x[, cols, drop = FALSE]
    x_rest <- x[, setdiff(colnames(x), cols), drop = FALSE]
    x_fill <- zoo::na.locf(x_fill, fromLast = fromLast, na.rm = na_rm)
    x <- cbind(x_fill, x_rest)[, colnames(x), drop = FALSE]
  } else {
    x <- zoo::na.locf(x, fromLast = fromLast, na.rm = na_rm)
  }
  x
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
    ohlcv_xts = .brf_treatment_ohlcv_xts,
    ohlcv_locf_xts = .brf_treatment_ohlcv_locf_xts,
    ohlcv_drop0_xts = .brf_treatment_ohlcv_drop0_xts
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

.brf_agg_standard_treatment <- function(df, ...) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(df)
  }
  out <- df
  sanitized <- vapply(names(out), .brf_sanitize_colname, character(1), USE.NAMES = FALSE)
  if (length(sanitized)) {
    valid <- nzchar(sanitized)
    if (any(valid)) {
      names(out)[valid] <- sanitized[valid]
    }
  }
  dup <- duplicated(names(out))
  if (any(dup)) {
    out <- out[, !dup, drop = FALSE]
  }
  sanitized_after <- vapply(names(out), .brf_sanitize_colname, character(1), USE.NAMES = FALSE)
  numeric_idx <- sanitized_after %in% .brf_numeric_targets
  if (any(numeric_idx)) {
    numeric_cols <- names(out)[numeric_idx]
    out[numeric_cols] <- lapply(out[numeric_cols], .brf_pt_number)
  }
  out
}

.brf_agg_clean_data_treatment <- function(df, ...) {
  cleaned <- .brf_agg_standard_treatment(df, ...)
  if (!nrow(cleaned)) {
    return(cleaned)
  }
  drop_cols <- c(
    "contract_code",
    "vencto",
    "contr_abert_1",
    "contr_fech_2",
    "num_negoc",
    "preco_med",
    "ajuste",
    "var_ptos",
    "ult_of_compra",
    "ult_of_venda"
  )
  keep_cols <- setdiff(names(cleaned), drop_cols)
  cleaned <- cleaned[, keep_cols, drop = FALSE]
  rename_map <- c(
    contr_negoc = "volume_qty",
    contract_negoc = "volume_qty",
    vol = "volume",
    preco_abert = "open",
    preco_min = "low",
    preco_max = "high",
    ult_preco = "close"
  )
  for (src in names(rename_map)) {
    if (src %in% names(cleaned)) {
      names(cleaned)[names(cleaned) == src] <- rename_map[[src]]
    }
  }
  dup <- duplicated(names(cleaned))
  if (any(dup)) {
    cleaned <- cleaned[, !dup, drop = FALSE]
  }
  cleaned
}

.brf_agg_clean_data_drop0_treatment <- function(df, ..., cols = c("volume_qty", "volume", "open", "high", "low", "close")) {
  cleaned <- .brf_agg_clean_data_treatment(df, ...)
  if (!is.data.frame(cleaned) || !nrow(cleaned)) {
    return(cleaned)
  }
  target_cols <- intersect(cols, names(cleaned))
  if (!length(target_cols)) {
    return(cleaned)
  }
  keep <- rep(TRUE, nrow(cleaned))
  for (col in target_cols) {
    values <- cleaned[[col]]
    numeric_values <- suppressWarnings(as.numeric(values))
    keep <- keep & !is.na(numeric_values) & numeric_values != 0
  }
  cleaned[keep, , drop = FALSE]
}

.brf_resolve_agg_treatment <- function(treatment) {
  if (is.function(treatment)) {
    return(treatment)
  }
  if (is.character(treatment) && length(treatment)) {
    key <- tolower(treatment[1])
    if (key == "clean_data") {
      return(.brf_agg_clean_data_treatment)
    }
    if (key == "clean_data_tibble") {
      return(function(df, ...) tibble::as_tibble(.brf_agg_clean_data_treatment(df, ...)))
    }
    if (key == "clean_data_drop0") {
      return(.brf_agg_clean_data_drop0_treatment)
    }
    if (key == "clean_data_drop0_tibble") {
      return(function(df, ...) tibble::as_tibble(.brf_agg_clean_data_drop0_treatment(df, ...)))
    }
    if (key == "standard") {
      return(.brf_agg_standard_treatment)
    }
    if (key == "standard_tibble") {
      return(function(df, ...) tibble::as_tibble(.brf_agg_standard_treatment(df, ...)))
    }
    return(.brf_resolve_treatment(treatment))
  }
  stop("Treatment must be a function or the name of a built-in treatment.", call. = FALSE)
}

.brf_add_futures_attrs <- function(data, ticker) {
  if (startsWith(ticker, "CCM")) {
    data <- .brf_add_futures_ccm(data, ticker)
  }
  if (startsWith(ticker, "WDO")) print("dol")
  if (startsWith(ticker, "WIN")) print("win")
  if (startsWith(ticker, "BGI")) print("boizin")
  return(data)
}

.brf_add_futures_ccm <- function(data, ticker) {
  print("milho")

  attr(data, "fut_slippage") <- 0.07
  attr(data, "fut_fees") <- 10
  attr(data, "fut_tick_size") <- 0.01
  attr(data, "fut_multiplier") <- 450

  attr_slippage <- attr(data, "fut_slippage")
  attr_fees <- attr(data, "fut_fees")
  # print(str(data))
  # FinancialInstrument::currency("USD")
  # FinancialInstrument::future(ticker,
  #   currency = "USD",
  #   multiplier = 450, tick_size = 0.01,
  #   identifiers = list(slippage = attr_slippage, fees = attr_fees),
  #   overwrite = TRUE
  # )
  return(data)
}
