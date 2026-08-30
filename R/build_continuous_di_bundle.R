# Versioned execution-aware DI1 daily continuous bundle

.brf_di_bundle_schema_id <- function() {
  "brfutures_di_continuous_bundle_v1"
}

.brf_di_bundle_schema_version <- function() {
  1L
}

.brf_di_bundle_source_schema_id <- function() {
  "brfutures_di_official_daily_v1"
}

.brf_di_bundle_default_ticker <- function(target_tenor,
                                          tenor_unit,
                                          allowed_maturities,
                                          selection_mode) {
  unit_suffix <- switch(
    tenor_unit,
    years = "Y",
    months = "M",
    business_days = "BD"
  )
  tenor_text <- gsub("\\.0+$", "", format(target_tenor, trim = TRUE))
  allowed <- toupper(trimws(as.character(allowed_maturities)))
  allowed <- allowed[!is.na(allowed) & nzchar(allowed)]
  maturity_suffix <- if (
    length(allowed) == 1L && identical(allowed, "ALL")
  ) {
    ""
  } else {
    paste0("_", paste(sort(unique(allowed)), collapse = ""))
  }
  legacy_suffix <- if (
    identical(selection_mode, "strict_du_floor") &&
      identical(tenor_unit, "years") &&
      length(allowed) == 1L && identical(allowed, "F")
  ) {
    "_DUFLOOR"
  } else {
    ""
  }
  paste0(
    "DI1FUT_", tenor_text, unit_suffix, maturity_suffix, legacy_suffix
  )
}

.brf_di_bundle_scalar_ticker <- function(ticker) {
  ticker <- toupper(trimws(as.character(ticker)))
  if (length(ticker) != 1L || is.na(ticker) || !nzchar(ticker)) {
    stop("`synthetic_ticker` must be one non-empty ticker.", call. = FALSE)
  }
  if (grepl("_(1M|5M|15M|30M|1H|4H)(_|$)", ticker)) {
    stop(
      "DI continuous execution bundles are daily-only; intraday remains signal-only.",
      call. = FALSE
    )
  }
  ticker
}

.brf_di_bundle_observed_available_at <- function(data) {
  aliases <- c(
    "di_adjustment_available_at", "adjustment_available_at",
    "publication_timestamp", "published_at"
  )
  hit <- aliases[aliases %in% names(data)][1L]
  empty <- as.POSIXct(
    rep(NA_real_, nrow(data)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  if (!length(hit) || is.na(hit)) {
    return(list(value = empty, field = rep(NA_character_, nrow(data))))
  }
  raw <- data[[hit]]
  # A Date, or a string containing only a date, proves no publication clock.
  has_clock <- if (inherits(raw, "POSIXt")) {
    !is.na(raw)
  } else if (inherits(raw, "Date")) {
    rep(FALSE, length(raw))
  } else {
    grepl("[ T][0-9]{2}:[0-9]{2}", trimws(as.character(raw)))
  }
  has_clock[is.na(has_clock)] <- FALSE
  parsed <- .brf_b3_parse_timestamp(raw)
  parsed[!has_clock] <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  list(
    value = parsed,
    field = ifelse(!is.na(parsed), hit, NA_character_)
  )
}

.brf_di_bundle_xts_df <- function(series) {
  out <- data.frame(
    date = as.Date(zoo::index(series)),
    zoo::coredata(series),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out) <- tolower(names(out))
  out
}

.brf_di_bundle_number <- function(data, field) {
  if (!field %in% names(data)) {
    return(rep(NA_real_, nrow(data)))
  }
  suppressWarnings(as.numeric(data[[field]]))
}

.brf_di_bundle_text <- function(data, field) {
  if (!field %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }
  out <- trimws(as.character(data[[field]]))
  out[is.na(out) | !nzchar(out)] <- NA_character_
  out
}

.brf_di_bundle_flag <- function(data, field) {
  if (!field %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }
  value <- data[[field]]
  if (is.logical(value)) {
    out <- value
  } else if (is.numeric(value)) {
    out <- is.finite(value) & value != 0
  } else {
    out <- tolower(trimws(as.character(value))) %in%
      c("true", "t", "1", "yes")
  }
  out[is.na(out)] <- FALSE
  out
}

.brf_di_bundle_selected_rows <- function(data,
                                         active_contracts,
                                         root,
                                         allowed_maturities,
                                         cal) {
  prepared <- .brf_di_prepare_continuous_data(
    data = data,
    root = root,
    allowed_maturities = allowed_maturities,
    cal = cal
  )$data
  source_key <- paste(
    format(as.Date(prepared$date), "%Y-%m-%d"),
    toupper(trimws(as.character(prepared$ticker))),
    sep = "\r"
  )
  active_key <- paste(
    format(as.Date(active_contracts$date), "%Y-%m-%d"),
    toupper(trimws(as.character(active_contracts$contract_symbol))),
    sep = "\r"
  )
  if (anyDuplicated(source_key)) {
    stop(
      "Canonical DI source is ambiguous by session and real contract.",
      call. = FALSE
    )
  }
  positions <- match(active_key, source_key)
  if (length(positions) != nrow(active_contracts) || anyNA(positions)) {
    stop(
      "DI bundle cannot align every selected real contract to its official source row.",
      call. = FALSE
    )
  }
  selected <- prepared[positions, , drop = FALSE]
  rownames(selected) <- NULL
  selected
}

.brf_di_bundle_signal_series <- function(series,
                                         synthetic_ticker,
                                         root) {
  rows <- .brf_di_bundle_xts_df(series)
  aliases <- list(
    rate_open = c("open"),
    rate_high = c("high"),
    rate_low = c("low"),
    rate_close = c("close"),
    pu_open = c("pu_open"),
    pu_high = c("pu_high"),
    pu_low = c("pu_low"),
    pu_close = c("pu_close"),
    volume = c("volume"),
    volume_qty = c("volume_qty"),
    rate_open_raw = c("rateopenraw", "open_unadj"),
    rate_high_raw = c("ratehighraw", "high_unadj"),
    rate_low_raw = c("ratelowraw", "low_unadj"),
    rate_close_raw = c("ratecloseraw", "close_unadj"),
    pu_open_raw = c("puopenraw", "pu_open_unadj"),
    pu_high_raw = c("puhighraw", "pu_high_unadj"),
    pu_low_raw = c("pulowraw", "pu_low_unadj"),
    pu_close_raw = c("pucloseraw", "pu_close_unadj")
  )
  out <- data.frame(
    date = rows$date,
    synthetic_ticker = rep(synthetic_ticker, nrow(rows)),
    root = rep(root, nrow(rows)),
    stringsAsFactors = FALSE
  )
  for (field in names(aliases)) {
    hit <- aliases[[field]][aliases[[field]] %in% names(rows)][1L]
    out[[field]] <- if (length(hit) && !is.na(hit)) {
      suppressWarnings(as.numeric(rows[[hit]]))
    } else {
      rep(NA_real_, nrow(rows))
    }
  }
  out$rate_adjustment_offset <- out$rate_close - out$rate_close_raw
  out$pu_adjustment_factor <- out$pu_close / out$pu_close_raw
  out$pu_inverse_adjustment_factor <- 1 / out$pu_adjustment_factor
  out$signal_usage <- "signal_only"
  out
}

.brf_di_bundle_execution_series <- function(selected,
                                             active,
                                             synthetic_ticker,
                                             available) {
  official <- .brf_di_bundle_flag(selected, "di_adjustment_is_official")
  quality <- .brf_di_bundle_text(selected, "di_adjustment_quality")
  adjustment_source <- .brf_di_bundle_text(selected, "source")
  settlement <- .brf_di_bundle_number(selected, "settlement_price")
  adjustment_base <- .brf_di_bundle_number(selected, "di_adjustment_base")
  adjustment_points <- .brf_di_bundle_number(selected, "di_adjustment_points")
  final <- official &
    is.finite(settlement) & settlement > 0 &
    is.finite(adjustment_base) & adjustment_base > 0 &
    is.finite(adjustment_points)
  provenance_complete <-
    !is.na(quality) & nzchar(quality) &
    !is.na(adjustment_source) & nzchar(adjustment_source)
  out <- data.frame(
    date = unname(as.Date(selected$date)),
    synthetic_ticker = rep(synthetic_ticker, nrow(selected)),
    root = toupper(trimws(as.character(selected$root))),
    contract_symbol = toupper(trimws(as.character(selected$ticker))),
    actual_maturity = as.Date(selected$maturity),
    valid_days = suppressWarnings(as.numeric(selected$valid_days)),
    contract_ordinal = suppressWarnings(as.integer(selected$contract_ordinal)),
    rate_open = .brf_di_bundle_number(selected, "open"),
    rate_high = .brf_di_bundle_number(selected, "high"),
    rate_low = .brf_di_bundle_number(selected, "low"),
    rate_close = .brf_di_bundle_number(selected, "close"),
    pu_open = .brf_di_bundle_number(selected, "PU_open"),
    pu_high = .brf_di_bundle_number(selected, "PU_high"),
    pu_low = .brf_di_bundle_number(selected, "PU_low"),
    pu_close = .brf_di_bundle_number(selected, "PU_close"),
    settlement_pu = settlement,
    previous_settlement_pu =
      .brf_di_bundle_number(selected, "previous_settlement"),
    adjustment_base_pu = adjustment_base,
    official_adjustment_points = adjustment_points,
    adjustment_is_official = official,
    adjustment_final = final,
    adjustment_quality = quality,
    adjustment_source = adjustment_source,
    adjustment_provenance_complete = provenance_complete,
    adjustment_available_at = available$value,
    availability_observed = !is.na(available$value),
    availability_source_field = available$field,
    volume = .brf_di_bundle_number(selected, "volume"),
    volume_qty = .brf_di_bundle_number(selected, "volume_qty"),
    open_interest = .brf_di_bundle_number(selected, "open_interest"),
    close_interest = .brf_di_bundle_number(selected, "close_interest"),
    trade_count = .brf_di_bundle_number(selected, "trade_count"),
    tick_size = .brf_di_bundle_number(selected, "TickSize"),
    stringsAsFactors = FALSE
  )
  active_key <- paste(active$date, active$contract_symbol, sep = "\r")
  out_key <- paste(out$date, out$contract_symbol, sep = "\r")
  if (!identical(active_key, out_key)) {
    stop(
      "DI execution rows conflict with the selected-contract map.",
      call. = FALSE
    )
  }
  out
}

.brf_di_bundle_contracts <- function(execution,
                                     active,
                                     spec,
                                     synthetic_ticker) {
  raw_complete <-
    is.finite(execution$rate_open) & is.finite(execution$rate_high) &
    is.finite(execution$rate_low) & is.finite(execution$rate_close) &
    is.finite(execution$pu_open) & execution$pu_open > 0 &
    is.finite(execution$pu_high) & execution$pu_high > 0 &
    is.finite(execution$pu_low) & execution$pu_low > 0 &
    is.finite(execution$pu_close) & execution$pu_close > 0
  row_supported <- execution$adjustment_final &
    execution$adjustment_provenance_complete &
    execution$availability_observed & raw_complete
  data.frame(
    date = execution$date,
    synthetic_ticker = rep(synthetic_ticker, nrow(execution)),
    root = execution$root,
    active_real_contract = execution$contract_symbol,
    actual_maturity = execution$actual_maturity,
    valid_days = execution$valid_days,
    month_code = as.character(active$month_code),
    contract_ordinal = execution$contract_ordinal,
    target_tenor = rep(as.numeric(spec$target_tenor), nrow(execution)),
    target_days = rep(as.numeric(spec$target_days), nrow(execution)),
    tenor_unit = rep(as.character(spec$tenor_unit), nrow(execution)),
    selection_mode = rep(as.character(spec$selection_mode), nrow(execution)),
    selection_version =
      rep(as.character(spec$selection_version), nrow(execution)),
    adjustment_available_at = execution$adjustment_available_at,
    availability_observed = execution$availability_observed,
    official_adjustment_final = execution$adjustment_final,
    adjustment_provenance_complete =
      execution$adjustment_provenance_complete,
    raw_execution_prices_complete = raw_complete,
    session_execution_supported = row_supported,
    pnl_formula_id = rep("di1_official_pu", nrow(execution)),
    stringsAsFactors = FALSE
  )
}

.brf_di_bundle_empty_roll_events <- function() {
  data.frame(
    event_id = character(),
    sequence = integer(),
    event_version = integer(),
    synthetic_ticker = character(),
    root = character(),
    decision_session = as.Date(character()),
    from_contract_symbol = character(),
    to_contract_symbol = character(),
    from_settlement_session = as.Date(character()),
    execution_session = as.Date(character()),
    effective_session = as.Date(character()),
    effective_at = as.POSIXct(character(), tz = "UTC"),
    available_at = as.POSIXct(character(), tz = "UTC"),
    from_adjustment_available_at = as.POSIXct(character(), tz = "UTC"),
    to_row_available_at = as.POSIXct(character(), tz = "UTC"),
    from_settlement_pu = numeric(),
    from_official_adjustment_points = numeric(),
    to_open_pu = numeric(),
    informational_roll_gap_pu = numeric(),
    roll_gap_pnl = numeric(),
    close_from_quantity_per_unit = numeric(),
    open_to_quantity_per_unit = numeric(),
    event_phase = character(),
    timing_quality = character(),
    price_quality = character(),
    event_execution_supported = logical(),
    pnl_formula_id = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
}

.brf_di_bundle_roll_events <- function(execution,
                                       synthetic_ticker) {
  if (nrow(execution) <= 1L) {
    return(.brf_di_bundle_empty_roll_events())
  }
  changes <- which(
    execution$contract_symbol[-1L] !=
      utils::head(execution$contract_symbol, -1L)
  ) + 1L
  if (!length(changes)) {
    return(.brf_di_bundle_empty_roll_events())
  }
  previous <- changes - 1L
  from_available <- execution$adjustment_available_at[previous]
  to_available <- execution$adjustment_available_at[changes]
  both_available <- !is.na(from_available) & !is.na(to_available)
  available_at <- as.POSIXct(
    rep(NA_real_, length(changes)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  available_at[both_available] <- as.POSIXct(
    pmax(
      as.numeric(from_available[both_available]),
      as.numeric(to_available[both_available])
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )
  supported <- both_available &
    execution$adjustment_final[previous] &
    execution$adjustment_final[changes] &
    is.finite(execution$settlement_pu[previous]) &
    execution$settlement_pu[previous] > 0 &
    is.finite(execution$pu_open[changes]) &
    execution$pu_open[changes] > 0
  effective_at <- as.POSIXct(
    rep(NA_real_, length(changes)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  event_id <- paste(
    execution$contract_symbol[previous],
    execution$contract_symbol[changes],
    format(execution$date[changes], "%Y-%m-%d"),
    "di_roll_v2",
    sep = ":"
  )
  data.frame(
    event_id = event_id,
    sequence = seq_along(changes),
    event_version = 2L,
    synthetic_ticker = rep(synthetic_ticker, length(changes)),
    root = rep("DI1", length(changes)),
    decision_session = execution$date[previous],
    from_contract_symbol = execution$contract_symbol[previous],
    to_contract_symbol = execution$contract_symbol[changes],
    from_settlement_session = execution$date[previous],
    execution_session = execution$date[changes],
    effective_session = execution$date[changes],
    effective_at = effective_at,
    available_at = available_at,
    from_adjustment_available_at = from_available,
    to_row_available_at = to_available,
    from_settlement_pu = execution$settlement_pu[previous],
    from_official_adjustment_points =
      execution$official_adjustment_points[previous],
    to_open_pu = execution$pu_open[changes],
    informational_roll_gap_pu =
      execution$pu_open[changes] - execution$settlement_pu[previous],
    roll_gap_pnl = rep(0, length(changes)),
    close_from_quantity_per_unit = rep(-1, length(changes)),
    open_to_quantity_per_unit = rep(1, length(changes)),
    event_phase = rep("between_official_sessions_modelled", length(changes)),
    timing_quality = rep(
      "daily_session_order_no_observed_fill_timestamp",
      length(changes)
    ),
    price_quality = rep(
      "official_previous_settlement_to_raw_next_session_open",
      length(changes)
    ),
    event_execution_supported = supported,
    pnl_formula_id = rep("di1_official_pu", length(changes)),
    source = rep("brfutures_di_roll_event_v2", length(changes)),
    stringsAsFactors = FALSE
  )
}

.brf_di_bundle_official_sessions <- function(execution) {
  execution[, c(
    "date", "synthetic_ticker", "root", "contract_symbol",
    "settlement_pu", "previous_settlement_pu", "adjustment_base_pu",
    "official_adjustment_points", "adjustment_is_official",
    "adjustment_final", "adjustment_quality", "adjustment_source",
    "adjustment_provenance_complete", "adjustment_available_at",
    "availability_observed", "availability_source_field"
  ), drop = FALSE]
}

.brf_di_bundle_provenance <- function(selected,
                                      execution) {
  fields <- c(
    "source", "di_adjustment_quality", "source_path", "source_file",
    "source_file_sha256", "payload_sha256", "report_date",
    "ohlc_repair_method", "ohlc_repair_status",
    "ohlc_repair_source_contracts", "ohlc_repair_neighbor_mode"
  )
  out <- data.frame(
    date = execution$date,
    contract_symbol = execution$contract_symbol,
    source_schema_id =
      rep(.brf_di_bundle_source_schema_id(), nrow(execution)),
    source_schema_version = rep(1L, nrow(execution)),
    adjustment_available_at = execution$adjustment_available_at,
    availability_source_field = execution$availability_source_field,
    stringsAsFactors = FALSE
  )
  for (field in fields) {
    out[[field]] <- if (field %in% names(selected)) {
      if (inherits(selected[[field]], "Date")) {
        as.Date(selected[[field]])
      } else {
        as.character(selected[[field]])
      }
    } else {
      rep(NA_character_, nrow(selected))
    }
  }
  out$ohlc_repaired <- .brf_di_bundle_flag(selected, "ohlc_repaired")
  out$ohlc_repair_prior_session_date <- if (
    "ohlc_repair_prior_session_date" %in% names(selected)
  ) {
    suppressWarnings(as.Date(selected$ohlc_repair_prior_session_date))
  } else {
    as.Date(rep(NA_character_, nrow(selected)))
  }
  for (field in paste0("ohlc_original_", c("open", "high", "low", "close"))) {
    out[[field]] <- .brf_di_bundle_number(selected, field)
  }
  out
}

.brf_di_bundle_contract_specs <- function() {
  data.frame(
    root = c("DI1", "DI1"),
    effective_from = as.Date(c("1900-01-01", "2025-08-18")),
    effective_to = as.Date(c("2025-08-17", NA)),
    spec_version = c(
      "b3_di1_official_pu_legacy_ticks_v1",
      "b3_di1_official_pu_current_ticks_v1"
    ),
    pnl_formula_id = "di1_official_pu",
    pnl_formula_version = 1L,
    kernel_family = "di1_official_adjustment",
    contract_model = "di1",
    contract_size = 100000,
    multiplier = 1,
    quantity_unit = "contract",
    signal_price_domain = "annualized_rate",
    execution_price_domain = "pu",
    price_quote_convention = "annualized_rate_signal_pu_execution",
    quote_currency = "BRL",
    settlement_currency = "BRL",
    pnl_currency = "BRL",
    price_scale = 0.01,
    cash_scale = 0.01,
    tick_size = NA_real_,
    tick_value = NA_real_,
    tick_rule_id = c(
      "di1_maturity_bucket_before_20250818",
      "di1_maturity_bucket_from_20250818"
    ),
    settlement_function = "positionsizer::ps_di_session_settlement",
    rounding_rule = "official_b3_pu_and_adjustment_as_published",
    execution_supported = TRUE,
    source_reference = paste(
      "B3 DI1 contract specification and official daily settlement bulletin"
    ),
    stringsAsFactors = FALSE
  )
}

.brf_di_bundle_cost_models <- function() {
  data.frame(
    root = "DI1",
    effective_from = as.Date("1900-01-01"),
    effective_to = as.Date(NA),
    cost_model_id = "di1_configured_modelled_costs_v1",
    cost_model_version = 1L,
    broker_fee = 10,
    broker_fee_style = "brl_per_contract_per_fill",
    emoluments_fee = 0.73,
    emoluments_fee_style = "brl_per_contract_per_fill",
    iss_fee = 0.5,
    iss_fee_style = "percent_of_broker_fee",
    slippage_value = 0.1,
    slippage_style = "percent_of_executed_pu_notional_per_leg",
    roll_legs_charged = 2L,
    cost_model_supported = TRUE,
    costs_observed = FALSE,
    official_b3_tariff_reproduced = FALSE,
    source = "configured modelling assumptions; not observed exchange costs",
    stringsAsFactors = FALSE
  )
}

.brf_di_bundle_canonical_component <- function(value) {
  if (xts::is.xts(value)) {
    value <- .brf_di_bundle_xts_df(value)
  }
  if (!is.data.frame(value)) {
    return(value)
  }
  value <- as.data.frame(value, stringsAsFactors = FALSE)
  ordering <- intersect(
    c(
      "date", "effective_from", "contract_symbol", "event_id", "sequence",
      "spec_version", "cost_model_id"
    ),
    names(value)
  )
  if (length(ordering) && nrow(value)) {
    keys <- lapply(ordering, function(field) {
      item <- value[[field]]
      if (inherits(item, "Date")) {
        format(item, "%Y-%m-%d")
      } else if (inherits(item, "POSIXt")) {
        format(item, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
      } else {
        as.character(item)
      }
    })
    value <- value[do.call(order, c(keys, list(na.last = TRUE))), , drop = FALSE]
  }
  rownames(value) <- NULL
  value
}

.brf_di_bundle_component_fingerprint <- function(value) {
  digest::digest(
    .brf_di_bundle_canonical_component(value),
    algo = "sha256",
    serialize = TRUE
  )
}

.brf_di_bundle_fingerprints <- function(bundle) {
  components <- c(
    "signal_series", "execution_series", "di_continuous_contracts",
    "di_roll_events", "official_sessions", "contract_specs",
    "cost_models", "provenance"
  )
  fingerprints <- stats::setNames(lapply(components, function(component) {
    .brf_di_bundle_component_fingerprint(bundle[[component]])
  }), components)
  unlist(fingerprints, use.names = TRUE)
}

.brf_di_bundle_manifest_fingerprint <- function(manifest) {
  manifest$bundle_fingerprint <- NULL
  digest::digest(manifest, algo = "sha256", serialize = TRUE)
}

#' Build a versioned execution-aware daily DI1 continuous bundle
#'
#' Keeps the adjusted synthetic signal separate from row-aligned real-contract
#' rate/PU observations, official settlement evidence, full roll events,
#' versioned DI1 P&L specifications, and explicit modelled costs. Publication
#' timestamps are copied only when the source contains an observed timestamp;
#' no midnight or end-of-day availability is invented. Consequently a bundle
#' with incomplete availability evidence remains useful for signals and gross
#' arithmetic research but has `manifest$execution_supported = FALSE`.
#'
#' @inheritParams build_continuous_di
#' @param target_tenor One positive target tenor. Build separate bundles for
#'   separate tenors so each object has one stable identity and fingerprint.
#' @param synthetic_ticker Optional stable public synthetic ticker.
#'
#' @return A serializable `brf_di_continuous_bundle` list with
#'   `signal_series`, `execution_series`, `di_continuous_contracts`,
#'   `di_roll_events`, `official_sessions`, `contract_specs`, `cost_models`,
#'   `provenance`, and a fingerprinted `manifest`.
#' @export
build_continuous_di_bundle <- function(
    data,
    target_tenor = 1,
    tenor_unit = c("years", "months", "business_days"),
    root = "DI1",
    allowed_maturities = "all",
    cal = NULL,
    include_pnl = TRUE,
    strict_target = TRUE,
    selection_mode = c("auto", "calendar_horizon", "strict_du_floor"),
    coverage_mode = c("first_eligible", "restart_strict_suffix"),
    synthetic_ticker = NULL) {
  tenor_unit <- match.arg(tenor_unit)
  selection_mode <- match.arg(selection_mode)
  coverage_mode <- match.arg(coverage_mode)
  if (!is.numeric(target_tenor) || length(target_tenor) != 1L ||
      is.na(target_tenor) || !is.finite(target_tenor) ||
      target_tenor <= 0) {
    stop(
      "`build_continuous_di_bundle()` requires one positive target tenor.",
      call. = FALSE
    )
  }
  root <- .brf_normalize_root(root)
  if (!identical(root, "DI1")) {
    stop(
      "`build_continuous_di_bundle()` supports only root DI1.",
      call. = FALSE
    )
  }
  resolved_mode <- .brf_di_resolve_selection_mode(
    selection_mode,
    target_tenor,
    tenor_unit,
    allowed_maturities
  )
  if (is.null(synthetic_ticker)) {
    synthetic_ticker <- .brf_di_bundle_default_ticker(
      target_tenor,
      tenor_unit,
      allowed_maturities,
      resolved_mode
    )
  }
  synthetic_ticker <- .brf_di_bundle_scalar_ticker(synthetic_ticker)

  series <- build_continuous_di(
    data = data,
    target_tenor = target_tenor,
    tenor_unit = tenor_unit,
    root = root,
    allowed_maturities = allowed_maturities,
    cal = cal,
    include_pnl = include_pnl,
    add_attrs = FALSE,
    add_globalenv = FALSE,
    strict_target = strict_target,
    selection_mode = resolved_mode,
    coverage_mode = coverage_mode
  )
  active <- attr(series, "active_contracts", exact = TRUE)
  spec <- attr(series, "continuous_spec", exact = TRUE)
  if (!is.data.frame(active) || nrow(active) != NROW(series) ||
      !is.list(spec)) {
    stop(
      "DI continuous builder did not expose its selected-contract evidence.",
      call. = FALSE
    )
  }
  active$date <- as.Date(active$date)
  active$contract_symbol <-
    toupper(trimws(as.character(active$contract_symbol)))
  selected <- .brf_di_bundle_selected_rows(
    data,
    active_contracts = active,
    root = root,
    allowed_maturities = allowed_maturities,
    cal = cal
  )
  available <- .brf_di_bundle_observed_available_at(selected)
  signal <- .brf_di_bundle_signal_series(
    series,
    synthetic_ticker = synthetic_ticker,
    root = root
  )
  execution <- .brf_di_bundle_execution_series(
    selected,
    active = active,
    synthetic_ticker = synthetic_ticker,
    available = available
  )
  contracts <- .brf_di_bundle_contracts(
    execution,
    active = active,
    spec = spec,
    synthetic_ticker = synthetic_ticker
  )
  rolls <- .brf_di_bundle_roll_events(
    execution,
    synthetic_ticker = synthetic_ticker
  )
  official_sessions <- .brf_di_bundle_official_sessions(execution)
  contract_specs <- .brf_di_bundle_contract_specs()
  contract_specs <- contract_specs[
    contract_specs$effective_from <= max(execution$date) &
      (is.na(contract_specs$effective_to) |
         contract_specs$effective_to >= min(execution$date)),
    ,
    drop = FALSE
  ]
  cost_models <- .brf_di_bundle_cost_models()
  provenance <- .brf_di_bundle_provenance(selected, execution)

  missing_availability <- sum(!contracts$availability_observed)
  unsupported_rolls <- if (nrow(rolls)) {
    sum(!rolls$event_execution_supported)
  } else {
    0L
  }
  execution_supported <-
    all(contracts$session_execution_supported) &&
    unsupported_rolls == 0L
  blockers <- character()
  if (missing_availability) {
    blockers <- c(
      blockers,
      paste0(
        missing_availability,
        "_official_session_adjustment_availability_timestamp(s)_missing"
      )
    )
  }
  if (unsupported_rolls) {
    blockers <- c(
      blockers,
      paste0(unsupported_rolls, "_roll_event(s)_lack_causal_evidence")
    )
  }
  if (any(!execution$adjustment_final)) {
    blockers <- c(
      blockers,
      paste0(
        sum(!execution$adjustment_final),
        "_official_adjustment_row(s)_not_final"
      )
    )
  }
  if (any(!execution$adjustment_provenance_complete)) {
    blockers <- c(
      blockers,
      paste0(
        sum(!execution$adjustment_provenance_complete),
        "_official_adjustment_provenance_row(s)_incomplete"
      )
    )
  }

  bundle <- list(
    signal_series = signal,
    execution_series = execution,
    di_continuous_contracts = contracts,
    di_roll_events = rolls,
    official_sessions = official_sessions,
    contract_specs = contract_specs,
    cost_models = cost_models,
    provenance = provenance
  )
  row_counts <- vapply(bundle, NROW, integer(1L))
  component_fingerprints <- .brf_di_bundle_fingerprints(bundle)
  manifest <- list(
    schema_id = .brf_di_bundle_schema_id(),
    schema_version = .brf_di_bundle_schema_version(),
    source_schema_id = .brf_di_bundle_source_schema_id(),
    source_schema_version = 1L,
    bundle_type = "b3_di1_daily_continuous",
    source = "B3 official daily",
    synthetic_ticker = synthetic_ticker,
    root = root,
    data_start = min(execution$date),
    data_end = max(execution$date),
    timeframe = "1d",
    signal_usage = "signal_only",
    signal_rate_adjustment = if (isTRUE(include_pnl)) "additive" else "none",
    signal_pu_adjustment = if (isTRUE(include_pnl)) {
      "multiplicative_ratio_backward"
    } else {
      "none"
    },
    execution_price_domain = "raw_pu_and_rate",
    pnl_formula_id = "di1_official_pu",
    pnl_formula_version = 1L,
    quote_currency = "BRL",
    settlement_currency = "BRL",
    pnl_currency = "BRL",
    selection_method = spec$method,
    target_tenor = spec$target_tenor,
    target_days = spec$target_days,
    tenor_unit = spec$tenor_unit,
    allowed_maturities = spec$allowed_maturities,
    strict_target = spec$strict_target,
    selection_mode = spec$selection_mode,
    selection_version = spec$selection_version,
    coverage_mode = spec$coverage_mode,
    roll_event_version = 2L,
    roll_timing_policy =
      "session_order_only_no_fabricated_intraday_clock",
    availability_policy = "observed_source_timestamp_only",
    missing_availability_count = missing_availability,
    roll_gap_pnl_policy = "zero",
    cost_model_id = cost_models$cost_model_id[[1L]],
    cost_evidence = "modelled_not_observed",
    execution_supported = execution_supported,
    execution_usage = if (execution_supported) {
      "real_contract_official_pu"
    } else {
      "signal_only_until_causal_availability_is_complete"
    },
    execution_blockers = blockers,
    row_counts = as.list(row_counts),
    component_fingerprints = as.list(component_fingerprints)
  )
  manifest$bundle_fingerprint <- .brf_di_bundle_manifest_fingerprint(manifest)
  bundle$manifest <- manifest
  class(bundle) <- c("brf_di_continuous_bundle", "list")
  validate_continuous_di_bundle(bundle)
}

#' Validate a versioned daily DI1 continuous bundle
#'
#' @param bundle Object returned by [build_continuous_di_bundle()].
#' @return The validated bundle, invisibly compatible with ordinary assignment.
#' @export
validate_continuous_di_bundle <- function(bundle) {
  required <- c(
    "signal_series", "execution_series", "di_continuous_contracts",
    "di_roll_events", "official_sessions", "contract_specs",
    "cost_models", "provenance", "manifest"
  )
  if (!is.list(bundle) || !all(required %in% names(bundle))) {
    stop(
      "DI continuous bundle lacks required component(s): ",
      paste(setdiff(required, names(bundle)), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  manifest <- bundle$manifest
  if (!is.list(manifest) ||
      !identical(manifest$schema_id, .brf_di_bundle_schema_id()) ||
      !identical(
        suppressWarnings(as.integer(manifest$schema_version)),
        .brf_di_bundle_schema_version()
      ) ||
      !identical(manifest$bundle_type, "b3_di1_daily_continuous") ||
      !identical(manifest$pnl_formula_id, "di1_official_pu") ||
      !identical(manifest$availability_policy, "observed_source_timestamp_only") ||
      !identical(
        manifest$roll_timing_policy,
        "session_order_only_no_fabricated_intraday_clock"
      )) {
    stop("DI continuous bundle manifest is incompatible.", call. = FALSE)
  }
  components <- setdiff(required, "manifest")
  if (any(!vapply(bundle[components], is.data.frame, logical(1L)))) {
    stop("DI continuous bundle components must be data frames.", call. = FALSE)
  }
  execution <- bundle$execution_series
  contracts <- bundle$di_continuous_contracts
  official <- bundle$official_sessions
  signal <- bundle$signal_series
  provenance <- bundle$provenance
  n <- nrow(execution)
  if (!n || nrow(contracts) != n || nrow(official) != n ||
      nrow(signal) != n || nrow(provenance) != n) {
    stop(
      "DI continuous bundle row-aligned components have inconsistent sizes.",
      call. = FALSE
    )
  }
  dates <- unname(as.Date(execution$date))
  if (anyNA(dates) || anyDuplicated(dates) || is.unsorted(dates)) {
    stop(
      "DI execution series must contain one ordered row per session.",
      call. = FALSE
    )
  }
  key <- paste(dates, execution$contract_symbol, sep = "\r")
  for (candidate in list(
    paste(contracts$date, contracts$active_real_contract, sep = "\r"),
    paste(official$date, official$contract_symbol, sep = "\r"),
    paste(provenance$date, provenance$contract_symbol, sep = "\r")
  )) {
    if (!identical(key, candidate)) {
      stop(
        "DI bundle components disagree on session/real-contract identity.",
        call. = FALSE
      )
    }
  }
  if (!identical(
        unname(as.numeric(as.Date(signal$date))),
        unname(as.numeric(dates))
      ) ||
      any(signal$synthetic_ticker != manifest$synthetic_ticker) ||
      any(execution$synthetic_ticker != manifest$synthetic_ticker)) {
    stop("DI signal and execution identities do not align.", call. = FALSE)
  }
  raw_fields <- c(
    "rate_open", "rate_high", "rate_low", "rate_close",
    "pu_open", "pu_high", "pu_low", "pu_close",
    "settlement_pu", "adjustment_base_pu",
    "official_adjustment_points"
  )
  missing_raw <- setdiff(raw_fields, names(execution))
  if (length(missing_raw)) {
    stop(
      "DI execution series lacks: ",
      paste(missing_raw, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  finite <- vapply(raw_fields, function(field) {
    all(is.finite(suppressWarnings(as.numeric(execution[[field]]))))
  }, logical(1L))
  if (any(!finite) ||
      any(execution$pu_low > pmin(
        execution$pu_open, execution$pu_close, execution$pu_high
      )) ||
      any(execution$pu_high < pmax(
        execution$pu_open, execution$pu_close, execution$pu_low
      ))) {
    stop("DI execution rate/PU data are incomplete or invalid.", call. = FALSE)
  }
  expected_points <- execution$settlement_pu - execution$adjustment_base_pu
  tolerance <- pmax(1, abs(expected_points)) * 1e-10
  if (any(abs(
    execution$official_adjustment_points - expected_points
  ) > tolerance)) {
    stop(
      "DI official adjustment does not reconcile to Settlement minus AdjustmentBase.",
      call. = FALSE
    )
  }
  observed <- !is.na(execution$adjustment_available_at)
  if (!identical(observed, as.logical(execution$availability_observed)) ||
      any(observed & (
        is.na(execution$availability_source_field) |
          !nzchar(execution$availability_source_field)
      ))) {
    stop(
      "DI adjustment availability must be copied from an observed source field.",
      call. = FALSE
    )
  }
  rolls <- bundle$di_roll_events
  changes <- if (n > 1L) {
    which(
      execution$contract_symbol[-1L] !=
        utils::head(execution$contract_symbol, -1L)
    ) + 1L
  } else {
    integer()
  }
  if (nrow(rolls) != length(changes)) {
    stop(
      "DI real-contract transitions do not match di_roll_events.",
      call. = FALSE
    )
  }
  if (nrow(rolls)) {
    previous <- changes - 1L
    if (any(rolls$event_version != 2L) ||
        any(rolls$from_contract_symbol !=
              execution$contract_symbol[previous]) ||
        any(rolls$to_contract_symbol != execution$contract_symbol[changes]) ||
        any(as.Date(rolls$execution_session) != dates[changes]) ||
        any(!is.na(rolls$effective_at)) ||
        any(rolls$roll_gap_pnl != 0) ||
        any(rolls$timing_quality !=
              "daily_session_order_no_observed_fill_timestamp")) {
      stop("DI roll events are malformed or fabricate an execution clock.", call. = FALSE)
    }
  }
  if (!all(bundle$contract_specs$pnl_formula_id == "di1_official_pu") ||
      !all(bundle$contract_specs$quote_currency == "BRL") ||
      !all(bundle$contract_specs$settlement_currency == "BRL") ||
      !all(bundle$contract_specs$pnl_currency == "BRL") ||
      any(bundle$contract_specs$multiplier != 1)) {
    stop(
      "DI versioned contract specifications are incompatible with official PU P&L.",
      call. = FALSE
    )
  }
  row_counts <- vapply(bundle[components], NROW, integer(1L))
  expected_counts <- unlist(manifest$row_counts, use.names = TRUE)
  if (!identical(
    as.integer(row_counts[names(expected_counts)]),
    as.integer(expected_counts)
  )) {
    stop("DI bundle manifest row counts do not reconcile.", call. = FALSE)
  }
  fingerprints <- .brf_di_bundle_fingerprints(bundle)
  expected_fingerprints <- unlist(
    manifest$component_fingerprints,
    use.names = TRUE
  )
  if (!identical(
    fingerprints[names(expected_fingerprints)],
    expected_fingerprints
  )) {
    stop("DI bundle component fingerprint mismatch.", call. = FALSE)
  }
  expected_bundle_fingerprint <- .brf_di_bundle_manifest_fingerprint(manifest)
  if (!identical(
    manifest$bundle_fingerprint,
    expected_bundle_fingerprint
  )) {
    stop("DI bundle manifest fingerprint mismatch.", call. = FALSE)
  }
  expected_supported <-
    all(contracts$session_execution_supported) &&
    (!nrow(rolls) || all(rolls$event_execution_supported))
  if (!identical(
    isTRUE(manifest$execution_supported),
    isTRUE(expected_supported)
  )) {
    stop(
      "DI bundle execution capability conflicts with its causal evidence.",
      call. = FALSE
    )
  }
  class(bundle) <- c("brf_di_continuous_bundle", "list")
  bundle
}

#' @export
print.brf_di_continuous_bundle <- function(x, ...) {
  manifest <- x$manifest
  cat("<brf_di_continuous_bundle>", manifest$synthetic_ticker, "\n")
  cat("  schema:", manifest$schema_version, "\n")
  cat("  sessions:", NROW(x$execution_series), "\n")
  cat("  rolls:", NROW(x$di_roll_events), "\n")
  cat(
    "  execution:",
    if (isTRUE(manifest$execution_supported)) {
      "real-contract official PU"
    } else {
      "signal-only (causal availability incomplete)"
    },
    "\n"
  )
  invisible(x)
}
