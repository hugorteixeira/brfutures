#' Build an execution-aware B3 continuous futures bundle
#'
#' Builds a versioned daily continuous-futures bundle with two clocks. The
#' sparse `signal_series` advances only on usable observed OHLC plus official
#' settlement. During an unobserved nominal session, its contract/factor
#' coordinate carries causally from the last observed signal bar; unobserved
#' intermediate maturities are collapsed rather than inserted synthetically.
#' `contract_map` retains the full official settlement calendar used to carry
#' and mark the real contract. Nominal mapping is based exclusively on calendar
#' days to the official last trading date. Ratio adjustments are anchored
#' exclusively on official settlements.
#'
#' This function is deliberately stricter than [build_backward_adjusted()] and
#' [build_forward_adjusted()]. It never infers a last trading date from the last
#' observation, never falls back from official settlement to another price, and
#' never substitutes an invalid roll ratio with one. Execution may choose the
#' nearest later-expiry contract that has a causal same-day executable bridge
#' when the nominal target does not. That is an execution-successor fallback,
#' recorded in the roll schedule, not volume/OI signal mapping.
#'
#' Schema v3 serializes
#' `roll_timing_policy = "causal_prebuffer_first_executable"`,
#' `roll_grace_policy = "symmetric_prebuffer_then_causal_extension"`, and
#' `signal_coordinate_policy =
#' "last_observed_signal_contract_causal_carry"`.
#'
#' @param data Non-empty data frame containing daily quotes by real contract.
#'   Required fields are `date`, `root`, `ticker`, `maturity`,
#'   `last_trade_date`, `open`, `high`, `low`, `close`, and
#'   `settlement_price`. Common contract/price aliases are accepted, but the
#'   official last-trade field must be named `last_trade_date`.
#' @param root Character scalar identifying the B3 futures root.
#' @param days_before_roll Non-negative integer number of calendar days before
#'   `last_trade_date`. A contract remains active only while
#'   `days_to_last_trade > days_before_roll`.
#' @param roll_grace_sessions Non-negative symmetric roll-window/SLA width in
#'   B3 sessions. Eligibility opens this many sessions before the nominal
#'   calendar roll, and the first causal session with official settlement and
#'   executable OHLC for both legs is used. If none exists by the nominal date,
#'   the old contract remains marked and the search continues; a delayed roll
#'   is warned and exceeding the same post-nominal threshold is flagged. This is
#'   not a hard late cap. If the nominal target advances while a roll is
#'   pending, intent coalesces to the latest target and may skip an illiquid
#'   intermediate maturity. Carrying the outgoing contract past its official
#'   expiry still fails. The default is three sessions.
#' @param adjustment_direction Either `"backward"` (preserve the latest raw
#'   level) or `"forward"` (preserve the earliest raw level).
#' @param adjustment_anchor Adjustment anchor. Schema version 3 supports only
#'   `"official_settlement"`.
#' @param roll_date_col Roll-date column. Schema version 3 requires
#'   `"last_trade_date"`; the argument is explicit to make the convention
#'   serializable in the bundle manifest.
#' @param strict Logical scalar. In strict mode, missing metadata, an
#'   unmappable session, or a missing/non-positive active-contract official
#'   settlement stops the build. A settlement-only session may still be marked,
#'   but invalid OHLC makes it non-tradable and excludes that session from
#'   `signal_series`; the map keeps the prior observed signal coordinate. Roll
#'   bridges and signal-ratio anchors remain strict: no close,
#'   last-observation, forward-fill, or invented factor is substituted.
#' @param maturities Either `"all"` or a character vector of futures month
#'   codes to retain.
#' @param synthetic_ticker Optional stable ticker for the synthetic signal.
#'   Defaults to `<ROOT>FUT_B<offset>` for backward adjustment and
#'   `<ROOT>FUT_F<offset>` for forward adjustment.
#'
#' @return An object of class `brf_continuous_bundle`. It is a list containing
#'   sparse `signal_series`; full-calendar `contract_map`; `roll_schedule`;
#'   raw `execution_data`; and `manifest`. The map separates the synthetic
#'   `signal_adjustment_factor`, end-of-session
#'   `execution_adjustment_factor`, and causal `order_transform_factor` (plus
#'   inverse, source, and `order_transform_asof_date`). An order transform is
#'   available only when its as-of date precedes the fill session. The map also
#'   exposes `signal_available`, `marking_supported`, `execution_tradable`, and
#'   `order_transform_available`. The roll schedule records the requested
#'   nominal target, actual successor, selection reason, skipped nominal
#'   contracts, and signed `execution_offset_sessions` (negative early,
#'   positive late).
#' @export
build_continuous_bundle <- function(data,
                                    root,
                                    days_before_roll,
                                    adjustment_direction = c("backward", "forward"),
                                    adjustment_anchor = "official_settlement",
                                    roll_date_col = "last_trade_date",
                                    strict = TRUE,
                                    maturities = "all",
                                    synthetic_ticker = NULL,
                                    roll_grace_sessions = 3L) {
  adjustment_direction <- match.arg(adjustment_direction)
  days_before_roll <- .brf_normalize_days_before_roll(days_before_roll)
  roll_grace_sessions <- .brf_bundle_normalize_nonnegative_integer(
    roll_grace_sessions,
    "roll_grace_sessions"
  )
  main_root <- .brf_normalize_root(root)
  strict <- .brf_bundle_scalar_logical(strict, "strict")

  if (length(adjustment_anchor) != 1L || is.na(adjustment_anchor) ||
      !identical(as.character(adjustment_anchor), "official_settlement")) {
    stop(
      "Continuous bundle schema v3 supports only adjustment_anchor = ",
      "'official_settlement'.",
      call. = FALSE
    )
  }
  if (length(roll_date_col) != 1L || is.na(roll_date_col) ||
      !identical(as.character(roll_date_col), "last_trade_date")) {
    stop(
      "Continuous bundle schema v3 requires roll_date_col = ",
      "'last_trade_date'.",
      call. = FALSE
    )
  }
  if (!is.data.frame(data) || !nrow(data)) {
    stop("`data` must be a non-empty data frame with futures quotes.", call. = FALSE)
  }

  columns <- .brf_bundle_resolve_columns(data)
  canonical <- .brf_bundle_canonical_data(data, columns)
  canonical <- canonical[canonical$root == main_root, , drop = FALSE]
  if (!nrow(canonical)) {
    stop("No data available for root '", main_root, "'.", call. = FALSE)
  }

  maturity_filter <- .brf_bundle_maturity_filter(maturities)
  if (!identical(maturity_filter, "ALL")) {
    month_code <- .brf_extract_month_code(canonical$contract, main_root)
    canonical <- canonical[month_code %in% maturity_filter, , drop = FALSE]
    if (!nrow(canonical)) {
      stop("No rows remain after filtering the requested maturities.", call. = FALSE)
    }
  }

  invalid_identity <- is.na(canonical$date) | !nzchar(canonical$contract) |
    is.na(canonical$maturity) | is.na(canonical$last_trade_date)
  if (any(invalid_identity)) {
    message <- paste0(
      "Exact continuous mapping requires date, contract, maturity, and ",
      "last_trade_date on every retained row; found ",
      sum(invalid_identity), " invalid row(s)."
    )
    if (strict) {
      stop(message, call. = FALSE)
    }
    canonical <- canonical[!invalid_identity, , drop = FALSE]
  }
  if (!nrow(canonical)) {
    stop("No valid rows remain for exact continuous mapping.", call. = FALSE)
  }

  canonical <- canonical[
    order(canonical$date, canonical$last_trade_date, canonical$maturity, canonical$contract),
    ,
    drop = FALSE
  ]
  rownames(canonical) <- NULL
  .brf_bundle_validate_unique_rows(canonical)
  .brf_bundle_validate_contract_metadata(canonical)

  synthetic_ticker <- .brf_bundle_synthetic_ticker(
    synthetic_ticker = synthetic_ticker,
    root = main_root,
    days_before_roll = days_before_roll,
    adjustment_direction = adjustment_direction
  )

  nominal_selected <- .brf_bundle_select_contracts(
    canonical,
    days_before_roll = days_before_roll,
    strict = strict
  )
  mapping_complete <- isTRUE(attr(nominal_selected, "mapping_complete", exact = TRUE))
  unmapped_sessions <- attr(nominal_selected, "unmapped_sessions", exact = TRUE)
  unmapped_reasons <- attr(nominal_selected, "unmapped_reasons", exact = TRUE)
  grace_result <- .brf_bundle_apply_roll_grace(
    data = canonical,
    nominal_selected = nominal_selected,
    roll_grace_sessions = roll_grace_sessions
  )
  selected <- grace_result$selected
  roll_timing <- grace_result$roll_timing
  .brf_bundle_validate_selection(selected)

  selected_quality <- .brf_bundle_price_quality(selected)
  selected$has_official_settlement <- selected_quality$has_official_settlement
  selected$has_valid_ohlc <- selected_quality$has_valid_ohlc
  selected$execution_tradable <- selected_quality$execution_supported
  selected$marking_supported <- selected_quality$has_official_settlement
  selected$execution_supported <- selected$marking_supported
  selected$data_quality <- ifelse(
    selected$marking_supported & !selected$execution_tradable,
    "settlement_only_no_trade",
    selected_quality$data_quality
  )

  if (strict && any(!selected$marking_supported)) {
    bad <- which(!selected$marking_supported)[1L]
    stop(
      "Exact continuous position cannot be marked on ",
      format(selected$date[bad]), " for contract '", selected$contract[bad],
      "': ", selected$data_quality[bad], ".",
      call. = FALSE
    )
  }

  nominal_quality <- .brf_bundle_price_quality(nominal_selected)
  nominal_selected$signal_available <- nominal_quality$execution_supported
  signal_selected <- nominal_selected[
    nominal_selected$signal_available,
    ,
    drop = FALSE
  ]
  if (!nrow(signal_selected)) {
    stop("No observed nominal-contract OHLC remains for the signal series.", call. = FALSE)
  }

  signal_rolls <- .brf_bundle_signal_roll_schedule(
    selected = signal_selected,
    execution_data = canonical,
    synthetic_ticker = synthetic_ticker,
    root = main_root
  )

  rolls <- .brf_bundle_roll_schedule(
    selected = selected,
    execution_data = canonical,
    synthetic_ticker = synthetic_ticker,
    root = main_root,
    roll_timing = roll_timing
  )
  delayed_rolls <- if (nrow(rolls)) {
    which(rolls$roll_delayed)
  } else {
    integer()
  }
  early_rolls <- if (nrow(rolls)) {
    which(rolls$roll_early)
  } else {
    integer()
  }
  coalesced_rolls <- if (nrow(rolls)) {
    which(rolls$skipped_nominal_count > 0L)
  } else {
    integer()
  }
  buffered_rolls <- sort(unique(c(
    delayed_rolls,
    early_rolls,
    coalesced_rolls
  )))
  if (length(buffered_rolls)) {
    details <- vapply(buffered_rolls, function(i) {
      timing_label <- if (rolls$execution_offset_sessions[[i]] < 0L) {
        paste0(
          abs(rolls$execution_offset_sessions[[i]]),
          " session(s) early"
        )
      } else if (rolls$execution_offset_sessions[[i]] > 0L) {
        paste0(
          rolls$execution_offset_sessions[[i]],
          " session(s) late"
        )
      } else {
        "on nominal session"
      }
      skipped_label <- if (rolls$skipped_nominal_count[[i]] > 0L) {
        paste0(
          "; skipped nominal ",
          rolls$skipped_nominal_contracts[[i]]
        )
      } else {
        ""
      }
      paste0(
        rolls$from_contract[[i]], "->", rolls$to_contract[[i]], " ",
        format(rolls$nominal_execution_date[[i]]), " -> ",
        format(rolls$execution_date[[i]]), " (",
        timing_label, skipped_label, ")"
      )
    }, character(1L))
    displayed_details <- utils::head(details, 12L)
    if (length(details) > length(displayed_details)) {
      displayed_details <- c(
        displayed_details,
        paste0(
          "... and ",
          length(details) - length(displayed_details),
          " more event(s); inspect roll_schedule for the full audit"
        )
      )
    }
    warning(
      "Buffered administrative roll differs from the nominal schedule; ",
      "configured causal prebuffer and late-warning threshold are ",
      roll_grace_sessions, " official session(s): ",
      paste(displayed_details, collapse = "; "), ".",
      call. = FALSE
    )
  }
  observed_signal_factors <- .brf_bundle_adjustment_factors(
    selected = signal_selected,
    roll_schedule = signal_rolls,
    adjustment_direction = adjustment_direction
  )
  if (any(!is.finite(observed_signal_factors) |
          observed_signal_factors <= 0)) {
    stop("Continuous adjustment produced a non-finite or non-positive factor.", call. = FALSE)
  }

  available_positions <- which(nominal_selected$signal_available)
  signal_observation_index <- findInterval(
    seq_len(nrow(nominal_selected)),
    available_positions
  )
  has_signal_history <- signal_observation_index > 0L
  signal_source_position <- seq_len(nrow(nominal_selected))
  signal_source_position[has_signal_history] <-
    available_positions[signal_observation_index[has_signal_history]]
  signal_coordinate_contract <-
    nominal_selected$contract[signal_source_position]
  signal_coordinate_factor <- rep(1, nrow(nominal_selected))
  signal_coordinate_factor[has_signal_history] <-
    observed_signal_factors[signal_observation_index[has_signal_history]]
  signal_selected$factor <- observed_signal_factors
  signal_selected$inverse_factor <- 1 / observed_signal_factors
  signal_coordinate_selected <-
    nominal_selected[signal_source_position, , drop = FALSE]
  signal_coordinate_selected$date <- nominal_selected$date
  signal_coordinate_selected$signal_available <-
    nominal_selected$signal_available
  signal_coordinate_selected$factor <- signal_coordinate_factor
  signal_coordinate_selected$inverse_factor <- 1 / signal_coordinate_factor

  same_contract <- selected$contract == signal_coordinate_contract
  execution_factor <- rep(NA_real_, nrow(selected))
  same_contract_with_history <- same_contract & has_signal_history
  execution_factor[same_contract_with_history] <-
    signal_coordinate_factor[same_contract_with_history]
  canonical_key <- paste(canonical$date, canonical$contract, sep = "\r")
  signal_coordinate_position <- match(
    paste(nominal_selected$date, signal_coordinate_contract, sep = "\r"),
    canonical_key
  )
  signal_coordinate_settlement <- rep(NA_real_, nrow(selected))
  has_signal_coordinate_row <- !is.na(signal_coordinate_position)
  signal_coordinate_settlement[has_signal_coordinate_row] <-
    canonical$settlement_price[
      signal_coordinate_position[has_signal_coordinate_row]
    ]
  bridgeable <- has_signal_history &
    !same_contract &
    is.finite(signal_coordinate_settlement) &
    signal_coordinate_settlement > 0 &
    selected$has_official_settlement
  execution_factor[bridgeable] <- signal_coordinate_factor[bridgeable] *
    signal_coordinate_settlement[bridgeable] /
    selected$settlement_price[bridgeable]
  execution_bridge_valid <- is.finite(execution_factor) & execution_factor > 0

  observed_signal_asof <- as.Date(
    rep(NA_character_, nrow(signal_selected))
  )
  if (nrow(signal_rolls)) {
    signal_roll_positions <- match(
      signal_rolls$effective_date,
      signal_selected$date
    )
    if (anyNA(signal_roll_positions)) {
      stop(
        "Signal adjustment events do not align with the official contract map.",
        call. = FALSE
      )
    }
    for (i in seq_len(nrow(signal_rolls))) {
      segment_start <- signal_roll_positions[[i]]
      segment_end <- if (i < nrow(signal_rolls)) {
        signal_roll_positions[[i + 1L]] - 1L
      } else {
        nrow(signal_selected)
      }
      observed_signal_asof[seq.int(segment_start, segment_end)] <-
        signal_rolls$anchor_date[[i]]
    }
  }
  signal_coordinate_asof <- as.Date(
    rep(NA_character_, nrow(nominal_selected))
  )
  signal_coordinate_asof[has_signal_history] <-
    observed_signal_asof[signal_observation_index[has_signal_history]]
  signal_coordinate_known_before_session <-
    is.na(signal_coordinate_asof) |
    signal_coordinate_asof < selected$date
  previous_date <- c(as.Date(NA), utils::head(selected$date, -1L))
  previous_signal_position <- match(
    paste(previous_date, signal_coordinate_contract, sep = "\r"),
    canonical_key
  )
  previous_active_position <- match(
    paste(previous_date, selected$contract, sep = "\r"),
    canonical_key
  )
  previous_signal_settlement <- rep(NA_real_, nrow(selected))
  previous_active_settlement <- rep(NA_real_, nrow(selected))
  has_previous_signal <- !is.na(previous_signal_position)
  has_previous_active <- !is.na(previous_active_position)
  previous_signal_settlement[has_previous_signal] <-
    canonical$settlement_price[previous_signal_position[has_previous_signal]]
  previous_active_settlement[has_previous_active] <-
    canonical$settlement_price[previous_active_position[has_previous_active]]
  prior_bridge_available <- has_signal_history &
    !same_contract &
    signal_coordinate_known_before_session &
    is.finite(previous_signal_settlement) &
    previous_signal_settlement > 0 &
    is.finite(previous_active_settlement) &
    previous_active_settlement > 0
  order_factor <- rep(NA_real_, nrow(selected))
  same_contract_order_available <- has_signal_history &
    same_contract &
    signal_coordinate_known_before_session
  order_factor[same_contract_order_available] <-
    signal_coordinate_factor[same_contract_order_available]
  order_factor[prior_bridge_available] <-
    signal_coordinate_factor[prior_bridge_available] *
    previous_signal_settlement[prior_bridge_available] /
    previous_active_settlement[prior_bridge_available]
  order_factor_valid <- is.finite(order_factor) & order_factor > 0
  order_factor_asof <- signal_coordinate_asof
  order_factor_asof[prior_bridge_available] <-
    previous_date[prior_bridge_available]
  order_factor_source <- rep("unavailable", nrow(selected))
  order_factor_source[same_contract_order_available] <-
    "same_contract_identity"
  order_factor_source[prior_bridge_available] <-
    "prior_official_settlement_bridge"

  selected$signal_factor <- signal_coordinate_factor
  selected$signal_inverse_factor <- 1 / signal_coordinate_factor
  selected$execution_factor <- execution_factor
  selected$execution_inverse_factor <- ifelse(
    execution_bridge_valid,
    1 / execution_factor,
    NA_real_
  )
  selected$signal_coordinate_asof_date <- signal_coordinate_asof
  selected$order_transform_factor <- order_factor
  selected$order_transform_inverse_factor <- ifelse(
    order_factor_valid,
    1 / order_factor,
    NA_real_
  )
  selected$order_transform_asof_date <- order_factor_asof
  selected$order_transform_source <- order_factor_source
  selected$order_transform_available <- order_factor_valid
  signal_series <- .brf_bundle_signal_series(
    selected = signal_selected,
    synthetic_ticker = synthetic_ticker
  )
  .brf_bundle_validate_adjusted_ohlc(signal_series)

  contract_map <- .brf_bundle_contract_map(
    selected = selected,
    signal_selected = signal_coordinate_selected,
    signal_rolls = signal_rolls,
    rolls = rolls,
    synthetic_ticker = synthetic_ticker,
    days_before_roll = days_before_roll,
    adjustment_direction = adjustment_direction
  )
  execution_data <- .brf_bundle_execution_export(
    canonical = canonical,
    original = data,
    columns = columns,
    synthetic_ticker = synthetic_ticker
  )

  execution_supported <- mapping_complete && all(contract_map$marking_supported) &&
    any(contract_map$signal_available) &&
    (!nrow(rolls) || all(rolls$validated))
  manifest <- list(
    schema_version = 3L,
    bundle_type = "b3_daily_continuous",
    source = "B3 official daily",
    synthetic_ticker = synthetic_ticker,
    root = main_root,
    data_start = min(contract_map$date),
    data_end = max(contract_map$date),
    adjustment_method = "multiplicative_ratio",
    adjustment_direction = adjustment_direction,
    adjustment_anchor = "official_settlement",
    roll_rule = "calendar_days_to_last_trade",
    roll_offset = days_before_roll,
    roll_grace_sessions = roll_grace_sessions,
    roll_timing_policy = "causal_prebuffer_first_executable",
    roll_grace_policy = "symmetric_prebuffer_then_causal_extension",
    signal_contract_policy = "nominal_calendar_sparse_observed_ohlc",
    signal_coordinate_policy = "last_observed_signal_contract_causal_carry",
    execution_session_policy = "official_settlement_mark_trade_only_with_valid_ohlc",
    strategy_fill_policy = "defer_until_execution_tradable",
    signal_execution_factor_policy =
      "eod_settlement_bridge_and_prior_session_order_transform",
    early_roll_count = length(early_rolls),
    delayed_roll_count = length(delayed_rolls),
    coalesced_roll_count = length(coalesced_rolls),
    successor_fallback_count = if (nrow(rolls)) {
      sum(
        rolls$target_selection_reason ==
          "nearest_executable_later_successor"
      )
    } else {
      0L
    },
    skipped_nominal_contract_count = if (nrow(rolls)) {
      sum(rolls$skipped_nominal_count)
    } else {
      0L
    },
    pending_execution_at_end = identical(
      utils::tail(contract_map$roll_mapping_state, 1L),
      "execution_delayed"
    ),
    settlement_only_session_count = sum(!contract_map$execution_tradable),
    signal_unavailable_session_count = sum(!contract_map$signal_available),
    pre_signal_session_count = sum(!has_signal_history),
    delayed_signal_roll_count = sum(
      signal_rolls$first_signal_date > signal_rolls$effective_date,
      na.rm = TRUE
    ),
    max_grace_sessions_used = if (nrow(rolls)) {
      max(rolls$grace_sessions_used)
    } else {
      0L
    },
    max_early_sessions_used = if (nrow(rolls)) {
      max(rolls$early_sessions_used)
    } else {
      0L
    },
    roll_date_col = "last_trade_date",
    maturities = maturity_filter,
    strict = strict,
    mapping_complete = mapping_complete,
    unmapped_sessions = unmapped_sessions,
    unmapped_reasons = unmapped_reasons,
    execution_supported = execution_supported,
    signal_usage = "signal_only",
    execution_usage = "real_contracts_only",
    row_counts = list(
      signal_series = nrow(signal_series),
      contract_map = nrow(contract_map),
      roll_schedule = nrow(rolls),
      execution_data = nrow(execution_data)
    ),
    contracts = sort(unique(execution_data$contract))
  )

  structure(
    list(
      signal_series = signal_series,
      contract_map = contract_map,
      roll_schedule = rolls,
      execution_data = execution_data,
      manifest = manifest
    ),
    class = c("brf_continuous_bundle", "list")
  )
}

.brf_bundle_scalar_logical <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

.brf_bundle_normalize_nonnegative_integer <- function(value, name) {
  if (length(value) != 1L || is.na(value) || !is.numeric(value) ||
      !is.finite(value) || value < 0 || abs(value - round(value)) > 1e-10) {
    stop("`", name, "` must be a single non-negative integer.", call. = FALSE)
  }
  as.integer(round(value))
}

.brf_bundle_resolve_column <- function(data, aliases, label, required = TRUE) {
  normalized <- tolower(names(data))
  hit <- match(tolower(aliases), normalized, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (!length(hit)) {
    if (required) {
      stop("Missing required column `", label, "` for continuous bundle v2.", call. = FALSE)
    }
    return(NA_character_)
  }
  names(data)[hit[1L]]
}

.brf_bundle_resolve_columns <- function(data) {
  list(
    date = .brf_bundle_resolve_column(data, c("date"), "date"),
    root = .brf_bundle_resolve_column(data, c("root"), "root"),
    contract = .brf_bundle_resolve_column(
      data,
      c("ticker", "contract", "contract_symbol", "contract_code"),
      "ticker"
    ),
    maturity = .brf_bundle_resolve_column(data, c("maturity", "maturity_date"), "maturity"),
    last_trade_date = .brf_bundle_resolve_column(
      data,
      c("last_trade_date"),
      "last_trade_date"
    ),
    open = .brf_bundle_resolve_column(data, c("open"), "open"),
    high = .brf_bundle_resolve_column(data, c("high"), "high"),
    low = .brf_bundle_resolve_column(data, c("low"), "low"),
    close = .brf_bundle_resolve_column(data, c("close"), "close"),
    settlement_price = .brf_bundle_resolve_column(
      data,
      c("settlement_price", "official_settlement", "settlement"),
      "settlement_price"
    ),
    volume = .brf_bundle_resolve_column(data, c("volume"), "volume", required = FALSE),
    volume_qty = .brf_bundle_resolve_column(
      data,
      c("volume_qty", "contracts_traded"),
      "volume_qty",
      required = FALSE
    ),
    open_interest = .brf_bundle_resolve_column(
      data,
      c("open_interest"),
      "open_interest",
      required = FALSE
    ),
    close_interest = .brf_bundle_resolve_column(
      data,
      c("close_interest"),
      "close_interest",
      required = FALSE
    ),
    trade_count = .brf_bundle_resolve_column(
      data,
      c("trade_count"),
      "trade_count",
      required = FALSE
    ),
    source = .brf_bundle_resolve_column(data, c("source"), "source", required = FALSE)
  )
}

.brf_bundle_numeric <- function(data, column) {
  if (is.na(column)) {
    return(rep(NA_real_, nrow(data)))
  }
  value <- data[[column]]
  if (is.numeric(value)) {
    return(as.numeric(value))
  }
  suppressWarnings(as.numeric(as.character(value)))
}

.brf_bundle_character <- function(data, column) {
  if (is.na(column)) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[column]])
}

.brf_bundle_canonical_data <- function(data, columns) {
  data.frame(
    .input_row = seq_len(nrow(data)),
    date = as.Date(data[[columns$date]]),
    root = toupper(trimws(as.character(data[[columns$root]]))),
    contract = toupper(trimws(as.character(data[[columns$contract]]))),
    maturity = as.Date(data[[columns$maturity]]),
    last_trade_date = as.Date(data[[columns$last_trade_date]]),
    open = .brf_bundle_numeric(data, columns$open),
    high = .brf_bundle_numeric(data, columns$high),
    low = .brf_bundle_numeric(data, columns$low),
    close = .brf_bundle_numeric(data, columns$close),
    settlement_price = .brf_bundle_numeric(data, columns$settlement_price),
    volume = .brf_bundle_numeric(data, columns$volume),
    volume_qty = .brf_bundle_numeric(data, columns$volume_qty),
    open_interest = .brf_bundle_numeric(data, columns$open_interest),
    close_interest = .brf_bundle_numeric(data, columns$close_interest),
    trade_count = .brf_bundle_numeric(data, columns$trade_count),
    source = .brf_bundle_character(data, columns$source),
    stringsAsFactors = FALSE
  )
}

.brf_bundle_maturity_filter <- function(maturities) {
  if (is.character(maturities) && length(maturities) == 1L &&
      !is.na(maturities) && identical(toupper(trimws(maturities)), "ALL")) {
    return("ALL")
  }
  values <- unique(toupper(trimws(as.character(maturities))))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) {
    stop("Supply at least one maturity month code or 'all'.", call. = FALSE)
  }
  values
}

.brf_bundle_synthetic_ticker <- function(synthetic_ticker,
                                         root,
                                         days_before_roll,
                                         adjustment_direction) {
  if (is.null(synthetic_ticker)) {
    marker <- if (identical(adjustment_direction, "backward")) "B" else "F"
    return(paste0(root, "FUT_", marker, days_before_roll))
  }
  if (length(synthetic_ticker) != 1L || is.na(synthetic_ticker) ||
      !nzchar(trimws(as.character(synthetic_ticker)))) {
    stop("`synthetic_ticker` must be NULL or one non-empty string.", call. = FALSE)
  }
  toupper(trimws(as.character(synthetic_ticker)))
}

.brf_bundle_validate_unique_rows <- function(data) {
  key <- paste(data$date, data$contract, sep = "\r")
  if (anyDuplicated(key)) {
    duplicate_key <- key[duplicated(key) | duplicated(key, fromLast = TRUE)][1L]
    parts <- strsplit(duplicate_key, "\r", fixed = TRUE)[[1L]]
    stop(
      "Exact continuous data has duplicate contract/session rows for '",
      parts[2L], "' on ", parts[1L], ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.brf_bundle_validate_contract_metadata <- function(data) {
  by_contract <- split(data, data$contract)
  for (contract in names(by_contract)) {
    rows <- by_contract[[contract]]
    maturity <- unique(rows$maturity[!is.na(rows$maturity)])
    last_trade <- unique(rows$last_trade_date[!is.na(rows$last_trade_date)])
    roots <- unique(rows$root[!is.na(rows$root)])
    if (length(maturity) != 1L || length(last_trade) != 1L || length(roots) != 1L) {
      stop(
        "Contract '", contract,
        "' has inconsistent root, maturity, or last_trade_date metadata.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.brf_bundle_select_contracts <- function(data, days_before_roll, strict) {
  sessions <- sort(unique(data$date))
  contract_metadata <- unique(data[
    , c("root", "contract", "maturity", "last_trade_date"), drop = FALSE
  ])
  contract_metadata <- contract_metadata[
    order(
      contract_metadata$last_trade_date,
      contract_metadata$maturity,
      contract_metadata$contract
    ),
    ,
    drop = FALSE
  ]
  selected <- vector("list", length(sessions))
  kept <- 0L
  missing_sessions <- as.Date(character())
  missing_reasons <- character()

  for (session in sessions) {
    session <- as.Date(session, origin = "1970-01-01")
    days_to_last_trade <- as.integer(contract_metadata$last_trade_date - session)
    eligible <- !is.na(days_to_last_trade) & days_to_last_trade > days_before_roll
    candidates <- contract_metadata[eligible, , drop = FALSE]
    if (!nrow(candidates)) {
      missing_sessions <- c(missing_sessions, session)
      missing_reasons <- c(missing_reasons, "no_eligible_successor")
      next
    }
    active_contract <- candidates$contract[1L]
    active_row <- data[
      data$date == session & data$contract == active_contract,
      ,
      drop = FALSE
    ]
    if (nrow(active_row) != 1L) {
      missing_sessions <- c(missing_sessions, session)
      missing_reasons <- c(
        missing_reasons,
        paste0("missing_active_contract_row:", active_contract)
      )
      next
    }
    active_row$days_to_last_trade <- as.integer(active_row$last_trade_date - session)
    kept <- kept + 1L
    selected[[kept]] <- active_row
  }

  if (length(missing_sessions) && strict) {
    first_reason <- missing_reasons[1L]
    if (identical(first_reason, "no_eligible_successor")) {
      stop(
        "No contract remains more than ", days_before_roll,
        " calendar day(s) from last_trade_date on ",
        format(missing_sessions[1L]),
        "; exact mapping cannot invent a successor.",
        call. = FALSE
      )
    }
    missing_contract <- sub(
      "^missing_active_contract_row:",
      "",
      first_reason
    )
    stop(
      "Expected active contract '", missing_contract,
      "' has no unique official row on ", format(missing_sessions[1L]),
      "; exact mapping cannot switch early or fill the missing session.",
      call. = FALSE
    )
  }
  if (!kept) {
    stop("Unable to map any exact active contract session.", call. = FALSE)
  }
  out <- do.call(rbind, selected[seq_len(kept)])
  rownames(out) <- NULL
  attr(out, "mapping_complete") <- !length(missing_sessions)
  attr(out, "unmapped_sessions") <- missing_sessions
  attr(out, "unmapped_reasons") <- missing_reasons
  out
}

.brf_bundle_empty_roll_timing <- function() {
  data.frame(
    decision_date = as.Date(character()),
    eligibility_start_date = as.Date(character()),
    nominal_execution_date = as.Date(character()),
    nominal_effective_date = as.Date(character()),
    execution_date = as.Date(character()),
    effective_date = as.Date(character()),
    from_contract = character(),
    to_contract = character(),
    requested_nominal_contract = character(),
    target_selection_reason = character(),
    execution_offset_sessions = integer(),
    early_sessions_used = integer(),
    grace_sessions_used = integer(),
    roll_early = logical(),
    roll_delayed = logical(),
    roll_warning_threshold_exceeded = logical(),
    roll_grace_sessions = integer(),
    candidate_count = integer(),
    total_pending_sessions = integer(),
    skipped_nominal_contracts = character(),
    skipped_nominal_count = integer(),
    execution_days_to_last_trade = integer(),
    incoming_ohlc_valid = logical(),
    rejected_candidate_reasons = character(),
    stringsAsFactors = FALSE
  )
}

.brf_bundle_roll_candidate_quality <- function(data,
                                               session,
                                               from_contract,
                                               to_contract,
                                               root) {
  from_row <- data[
    data$date == session & data$contract == from_contract,
    ,
    drop = FALSE
  ]
  if (nrow(from_row) != 1L) {
    return(list(
      feasible = FALSE,
      fatal = TRUE,
      reason = paste0("missing_outgoing_row:", from_contract)
    ))
  }
  from_quality <- .brf_bundle_price_quality(from_row)
  if (!isTRUE(from_quality$has_official_settlement[[1L]])) {
    return(list(
      feasible = FALSE,
      fatal = TRUE,
      reason = paste0(
        "outgoing_missing_official_settlement:",
        from_quality$data_quality[[1L]]
      )
    ))
  }
  if (!isTRUE(from_quality$has_valid_ohlc[[1L]])) {
    return(list(
      feasible = FALSE,
      fatal = FALSE,
      reason = paste0("outgoing_not_tradable:", from_quality$data_quality[[1L]])
    ))
  }

  to_row <- data[
    data$date == session & data$contract == to_contract,
    ,
    drop = FALSE
  ]
  if (nrow(to_row) != 1L) {
    return(list(
      feasible = FALSE,
      fatal = FALSE,
      reason = paste0("missing_incoming_row:", to_contract)
    ))
  }
  if (!identical(from_row$root[[1L]], root) ||
      !identical(to_row$root[[1L]], root)) {
    return(list(
      feasible = FALSE,
      fatal = TRUE,
      reason = "roll_contract_root_mismatch"
    ))
  }
  to_quality <- .brf_bundle_price_quality(to_row)
  if (!isTRUE(to_quality$has_official_settlement[[1L]])) {
    return(list(
      feasible = FALSE,
      fatal = FALSE,
      reason = paste0(
        "incoming_missing_official_settlement:",
        to_quality$data_quality[[1L]]
      )
    ))
  }
  if (!isTRUE(to_quality$has_valid_ohlc[[1L]])) {
    return(list(
      feasible = FALSE,
      fatal = FALSE,
      reason = paste0("incoming_not_tradable:", to_quality$data_quality[[1L]])
    ))
  }
  list(
    feasible = TRUE,
    fatal = FALSE,
    reason = "ok",
    incoming_ohlc_valid = TRUE
  )
}

.brf_bundle_apply_roll_grace <- function(data,
                                         nominal_selected,
                                         roll_grace_sessions) {
  n <- nrow(nominal_selected)
  if (n <= 1L) {
    nominal_selected$nominal_active_contract <- nominal_selected$contract
    nominal_selected$roll_pending <- FALSE
    nominal_selected$roll_grace_session <- 0L
    nominal_selected$roll_lead_session <- 0L
    nominal_selected$roll_mapping_state <- "aligned"
    nominal_selected$roll_delay_reason <- NA_character_
    nominal_selected$is_roll_execution <- FALSE
    return(list(
      selected = nominal_selected,
      roll_timing = .brf_bundle_empty_roll_timing()
    ))
  }
  changes <- which(
    nominal_selected$contract[-1L] !=
      utils::head(nominal_selected$contract, -1L)
  ) + 1L
  if (!length(changes)) {
    nominal_selected$nominal_active_contract <- nominal_selected$contract
    nominal_selected$roll_pending <- FALSE
    nominal_selected$roll_grace_session <- 0L
    nominal_selected$roll_lead_session <- 0L
    nominal_selected$roll_mapping_state <- "aligned"
    nominal_selected$roll_delay_reason <- NA_character_
    nominal_selected$is_roll_execution <- FALSE
    return(list(
      selected = nominal_selected,
      roll_timing = .brf_bundle_empty_roll_timing()
    ))
  }

  nominal_execution <- changes - 1L
  eligibility_start <- pmax(
    1L,
    nominal_execution - roll_grace_sessions
  )
  candidate_transition <- findInterval(
    seq_len(n - 1L),
    eligibility_start
  )
  actual_contract <- rep(NA_character_, n)
  actual_contract[[1L]] <- nominal_selected$contract[[1L]]
  delay_reason <- rep(NA_character_, n)
  is_roll_execution <- rep(FALSE, n)
  timing <- list()
  timing_count <- 0L
  current_target <- actual_contract[[1L]]
  pending_start <- NA_integer_
  pending_candidate_count <- 0L
  pending_rejected <- character()
  skipped_nominal <- character()

  contract_metadata <- unique(data[
    , c("contract", "last_trade_date"), drop = FALSE
  ])
  contract_last_trade <- contract_metadata$last_trade_date
  names(contract_last_trade) <- contract_metadata$contract
  ordered_contracts <- contract_metadata$contract[
    order(contract_metadata$last_trade_date, contract_metadata$contract)
  ]
  nominal_contracts <- unique(nominal_selected$contract)

  for (candidate in seq_len(n - 1L)) {
    transition <- candidate_transition[[candidate]]
    target_contract <- if (transition > 0L) {
      nominal_selected$contract[[changes[[transition]]]]
    } else {
      nominal_selected$contract[[candidate + 1L]]
    }
    from_contract <- actual_contract[[candidate]]
    from_last_trade <- as.Date(
      contract_last_trade[[from_contract]],
      origin = "1970-01-01"
    )
    target_last_trade <- as.Date(
      contract_last_trade[[target_contract]],
      origin = "1970-01-01"
    )
    if (!is.na(target_last_trade) &&
        !is.na(from_last_trade) &&
        target_last_trade <= from_last_trade) {
      actual_contract[[candidate + 1L]] <- from_contract
      next
    }

    if (!identical(target_contract, current_target)) {
      if (!identical(current_target, from_contract)) {
        skipped_nominal <- unique(c(skipped_nominal, current_target))
      }
      current_target <- target_contract
      if (identical(from_contract, target_contract)) {
        pending_start <- NA_integer_
        pending_candidate_count <- 0L
        pending_rejected <- character()
        skipped_nominal <- character()
      }
    }

    if (identical(from_contract, target_contract)) {
      actual_contract[[candidate + 1L]] <- from_contract
      next
    }
    if (is.na(pending_start)) {
      pending_start <- candidate
    }
    pending_candidate_count <- pending_candidate_count + 1L
    session <- nominal_selected$date[[candidate]]
    if (is.na(from_last_trade) || session > from_last_trade) {
      stop(
        "Buffered roll would carry expired contract '", from_contract,
        "' past last_trade_date ", format(from_last_trade), ".",
        call. = FALSE
      )
    }

    requested_nominal_contract <- target_contract
    target_selection_reason <- "nominal_target"
    transition_position <- if (transition > 0L) {
      transition
    } else {
      which(changes == candidate + 1L)[[1L]]
    }
    nominal_execution_position <-
      nominal_execution[[transition_position]]
    primary_execution_offset <-
      candidate - nominal_execution_position
    successor_fallback_allowed <-
      primary_execution_offset >= roll_grace_sessions ||
      nominal_selected$date[[candidate + 1L]] > from_last_trade
    candidate_quality <- function(to_contract) {
      .brf_bundle_roll_candidate_quality(
        data = data,
        session = session,
        from_contract = from_contract,
        to_contract = to_contract,
        root = nominal_selected$root[[candidate]]
      )
    }
    quality <- candidate_quality(target_contract)
    if (isTRUE(quality$fatal)) {
      stop(
        "Exact buffered roll from '", from_contract, "' to '",
        target_contract, "' cannot carry the outgoing contract on ",
        format(session), ": ", quality$reason, ".",
        call. = FALSE
      )
    }
    primary_rejection <- if (!isTRUE(quality$feasible)) {
      paste0(
        format(session), "=", from_contract, "->",
        target_contract, ":", quality$reason
      )
    } else {
      character()
    }
    if (!isTRUE(quality$feasible) && successor_fallback_allowed) {
      later_contracts <- ordered_contracts[
        contract_last_trade[ordered_contracts] > from_last_trade
      ]
      later_contracts <- later_contracts[
        later_contracts != target_contract
      ]
      for (later_contract in later_contracts) {
        later_quality <- candidate_quality(later_contract)
        if (isTRUE(later_quality$fatal)) {
          stop(
            "Exact buffered successor scan from '", from_contract,
            "' to '", later_contract, "' failed on ", format(session),
            ": ", later_quality$reason, ".",
            call. = FALSE
          )
        }
        if (isTRUE(later_quality$feasible)) {
          target_contract <- later_contract
          quality <- later_quality
          target_selection_reason <-
            "nearest_executable_later_successor"
          skipped_before_target <- nominal_contracts[
            contract_last_trade[nominal_contracts] > from_last_trade &
              contract_last_trade[nominal_contracts] <
                contract_last_trade[[target_contract]]
          ]
          skipped_nominal <- unique(c(
            skipped_nominal,
            requested_nominal_contract,
            skipped_before_target
          ))
          break
        }
      }
    }

    if (isTRUE(quality$feasible) &&
        identical(
          target_selection_reason,
          "nearest_executable_later_successor"
        ) &&
        length(primary_rejection)) {
      pending_rejected <- c(
        pending_rejected,
        primary_rejection
      )
    }
    if (!isTRUE(quality$feasible)) {
      pending_rejected <- c(
        pending_rejected,
        primary_rejection
      )
      delay_reason[[candidate + 1L]] <- paste(
        pending_rejected,
        collapse = "; "
      )
      actual_contract[[candidate + 1L]] <- from_contract
      if (nominal_selected$date[[candidate + 1L]] > from_last_trade) {
        stop(
          "Exact buffered roll from '", from_contract, "' had no executable ",
          "bridge before last_trade_date ", format(from_last_trade), ": ",
          paste(pending_rejected, collapse = "; "),
          ". No close/last-observation fallback is allowed.",
          call. = FALSE
        )
      }
      next
    }

    nominal_effective_position <- changes[[transition_position]]
    signed_offset <- candidate - nominal_execution_position
    early_used <- max(-signed_offset, 0L)
    late_used <- max(signed_offset, 0L)
    rejected_text <- if (length(pending_rejected)) {
      paste(pending_rejected, collapse = "; ")
    } else {
      NA_character_
    }
    skipped_nominal <- unique(skipped_nominal[
      skipped_nominal %in% nominal_contracts &
        contract_last_trade[skipped_nominal] > from_last_trade &
        contract_last_trade[skipped_nominal] <
          contract_last_trade[[target_contract]]
    ])
    skipped_text <- if (length(skipped_nominal)) {
      paste(skipped_nominal, collapse = ",")
    } else {
      NA_character_
    }
    timing_count <- timing_count + 1L
    timing[[timing_count]] <- data.frame(
      decision_date =
        nominal_selected$date[[eligibility_start[[transition_position]]]],
      eligibility_start_date =
        nominal_selected$date[[eligibility_start[[transition_position]]]],
      nominal_execution_date =
        nominal_selected$date[[nominal_execution_position]],
      nominal_effective_date =
        nominal_selected$date[[nominal_effective_position]],
      execution_date = session,
      effective_date = nominal_selected$date[[candidate + 1L]],
      from_contract = from_contract,
      to_contract = target_contract,
      requested_nominal_contract = requested_nominal_contract,
      target_selection_reason = target_selection_reason,
      execution_offset_sessions = as.integer(signed_offset),
      early_sessions_used = as.integer(early_used),
      grace_sessions_used = as.integer(late_used),
      roll_early = signed_offset < 0L,
      roll_delayed = signed_offset > 0L,
      roll_warning_threshold_exceeded =
        late_used > roll_grace_sessions,
      roll_grace_sessions = roll_grace_sessions,
      candidate_count = as.integer(pending_candidate_count),
      total_pending_sessions = as.integer(candidate - pending_start + 1L),
      skipped_nominal_contracts = skipped_text,
      skipped_nominal_count = as.integer(length(skipped_nominal)),
      execution_days_to_last_trade = as.integer(from_last_trade - session),
      incoming_ohlc_valid = TRUE,
      rejected_candidate_reasons = rejected_text,
      stringsAsFactors = FALSE
    )
    actual_contract[[candidate + 1L]] <- target_contract
    is_roll_execution[[candidate]] <- TRUE
    pending_start <- NA_integer_
    pending_candidate_count <- 0L
    pending_rejected <- character()
    skipped_nominal <- character()
  }

  selected_key <- paste(nominal_selected$date, actual_contract, sep = "\r")
  data_key <- paste(data$date, data$contract, sep = "\r")
  positions <- match(selected_key, data_key)
  if (anyNA(positions)) {
    first <- which(is.na(positions))[[1L]]
    stop(
      "Roll grace expected carried contract '", actual_contract[[first]],
      "' on ", format(nominal_selected$date[[first]]),
      ", but no unique official row exists.",
      call. = FALSE
    )
  }
  selected <- data[positions, , drop = FALSE]
  rownames(selected) <- NULL
  selected$nominal_active_contract <- nominal_selected$contract
  actual_last_trade <- as.Date(
    contract_last_trade[actual_contract],
    origin = "1970-01-01"
  )
  nominal_last_trade <- nominal_selected$last_trade_date
  mapping_state <- ifelse(
    actual_contract == nominal_selected$contract,
    "aligned",
    ifelse(
      actual_last_trade < nominal_last_trade,
      "execution_delayed",
      "execution_ahead"
    )
  )
  grace_session <- lead_session <- integer(n)
  for (i in seq_len(n)) {
    if (identical(mapping_state[[i]], "execution_delayed")) {
      grace_session[[i]] <- if (
        i > 1L &&
          identical(mapping_state[[i - 1L]], "execution_delayed")
      ) {
        grace_session[[i - 1L]] + 1L
      } else {
        1L
      }
    }
    if (identical(mapping_state[[i]], "execution_ahead")) {
      lead_session[[i]] <- if (
        i > 1L &&
          identical(mapping_state[[i - 1L]], "execution_ahead")
      ) {
        lead_session[[i - 1L]] + 1L
      } else {
        1L
      }
    }
  }
  selected$roll_pending <- actual_contract != nominal_selected$contract
  selected$roll_grace_session <- grace_session
  selected$roll_lead_session <- lead_session
  selected$roll_mapping_state <- mapping_state
  selected$roll_delay_reason <- delay_reason
  selected$is_roll_execution <- is_roll_execution
  attr(selected, "mapping_complete") <- isTRUE(
    attr(nominal_selected, "mapping_complete", exact = TRUE)
  )
  attr(selected, "unmapped_sessions") <- attr(
    nominal_selected, "unmapped_sessions", exact = TRUE
  )
  attr(selected, "unmapped_reasons") <- attr(
    nominal_selected, "unmapped_reasons", exact = TRUE
  )
  list(
    selected = selected,
    roll_timing = if (timing_count) {
      do.call(rbind, timing)
    } else {
      .brf_bundle_empty_roll_timing()
    }
  )
}

.brf_bundle_empty_signal_roll_schedule <- function() {
  data.frame(
    event_id = character(),
    effective_date = as.Date(character()),
    first_signal_date = as.Date(character()),
    from_contract = character(),
    to_contract = character(),
    anchor_date = as.Date(character()),
    anchor_lag_sessions = integer(),
    from_settlement_price = numeric(),
    to_settlement_price = numeric(),
    adjustment_ratio = numeric(),
    inverse_adjustment_ratio = numeric(),
    validated = logical(),
    stringsAsFactors = FALSE
  )
}

.brf_bundle_signal_roll_schedule <- function(selected,
                                             execution_data,
                                             synthetic_ticker,
                                             root) {
  if (nrow(selected) <= 1L) {
    return(.brf_bundle_empty_signal_roll_schedule())
  }
  changes <- which(
    selected$contract[-1L] != utils::head(selected$contract, -1L)
  ) + 1L
  if (!length(changes)) {
    return(.brf_bundle_empty_signal_roll_schedule())
  }

  out <- vector("list", length(changes))
  for (j in seq_along(changes)) {
    position <- changes[[j]]
    from_contract <- selected$contract[[position - 1L]]
    to_contract <- selected$contract[[position]]
    from_date <- selected$date[[position - 1L]]
    effective_date <- selected$date[[position]]
    candidate_dates <- sort(unique(execution_data$date[
      execution_data$date >= from_date &
        execution_data$date <= effective_date
    ]))
    before_effective <- candidate_dates[candidate_dates < effective_date]
    candidate_dates <- c(
      rev(before_effective),
      effective_date
    )
    anchor <- NULL
    anchor_date <- as.Date(NA)
    for (candidate_index in seq_along(candidate_dates)) {
      candidate_date <- candidate_dates[[candidate_index]]
      from_row <- execution_data[
        execution_data$date == candidate_date &
          execution_data$contract == from_contract,
        ,
        drop = FALSE
      ]
      to_row <- execution_data[
        execution_data$date == candidate_date &
          execution_data$contract == to_contract,
        ,
        drop = FALSE
      ]
      from_quality <- if (nrow(from_row) == 1L) {
        .brf_bundle_price_quality(from_row)
      } else {
        NULL
      }
      to_quality <- if (nrow(to_row) == 1L) {
        .brf_bundle_price_quality(to_row)
      } else {
        NULL
      }
      valid <- nrow(from_row) == 1L && nrow(to_row) == 1L &&
        identical(from_row$root[[1L]], root) &&
        identical(to_row$root[[1L]], root) &&
        isTRUE(from_quality$has_official_settlement[[1L]]) &&
        isTRUE(to_quality$has_official_settlement[[1L]])
      if (valid) {
        anchor <- list(from = from_row, to = to_row)
        anchor_date <- candidate_date
        break
      }
    }
    if (is.null(anchor)) {
      stop(
        "Signal adjustment from '", from_contract, "' to '", to_contract,
        "' lacks simultaneous positive official settlements between ",
        format(from_date), " and ",
        format(effective_date), ".",
        call. = FALSE
      )
    }
    from_settlement <- anchor$from$settlement_price[[1L]]
    to_settlement <- anchor$to$settlement_price[[1L]]
    ratio <- to_settlement / from_settlement
    inverse_ratio <- from_settlement / to_settlement
    if (!is.finite(ratio) || ratio <= 0 ||
        !is.finite(inverse_ratio) || inverse_ratio <= 0) {
      stop("Signal adjustment produced an invalid official-settlement ratio.", call. = FALSE)
    }
    session_dates <- sort(unique(execution_data$date[
      execution_data$date >= from_date &
        execution_data$date <= effective_date
    ]))
    out[[j]] <- data.frame(
      event_id = paste(
        synthetic_ticker,
        "signal",
        format(effective_date, "%Y%m%d"),
        from_contract,
        to_contract,
        sep = "::"
      ),
      effective_date = effective_date,
      first_signal_date = effective_date,
      from_contract = from_contract,
      to_contract = to_contract,
      anchor_date = anchor$from$date[[1L]],
      anchor_lag_sessions = as.integer(
        match(anchor_date, session_dates) - 1L
      ),
      from_settlement_price = from_settlement,
      to_settlement_price = to_settlement,
      adjustment_ratio = ratio,
      inverse_adjustment_ratio = inverse_ratio,
      validated = TRUE,
      stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result
}

.brf_bundle_validate_selection <- function(selected) {
  last_trade_step <- diff(as.numeric(selected$last_trade_date))
  if (any(last_trade_step < 0)) {
    stop("Exact active-contract mapping moved backward to an older expiry.", call. = FALSE)
  }
  same_expiry_change <- selected$contract[-1L] != utils::head(selected$contract, -1L) &
    selected$last_trade_date[-1L] == utils::head(selected$last_trade_date, -1L)
  if (length(same_expiry_change) && any(same_expiry_change)) {
    stop("Exact active-contract mapping is ambiguous within one last_trade_date.", call. = FALSE)
  }
  invisible(TRUE)
}

.brf_bundle_price_quality <- function(data) {
  settlement_ok <- is.finite(data$settlement_price) & data$settlement_price > 0
  ohlc_finite <- is.finite(data$open) & is.finite(data$high) &
    is.finite(data$low) & is.finite(data$close)
  ohlc_positive <- data$open > 0 & data$high > 0 &
    data$low > 0 & data$close > 0
  lower_ok <- data$low <= pmin(data$open, data$close, data$high)
  upper_ok <- data$high >= pmax(data$open, data$close, data$low)
  ohlc_ok <- ohlc_finite & ohlc_positive & lower_ok & upper_ok
  execution_ok <- settlement_ok & ohlc_ok
  quality <- rep("ok", nrow(data))
  quality[!settlement_ok & ohlc_ok] <- "missing_or_invalid_official_settlement"
  quality[settlement_ok & !ohlc_ok] <- "missing_or_invalid_ohlc"
  quality[!settlement_ok & !ohlc_ok] <- "invalid_settlement_and_ohlc"
  list(
    has_official_settlement = settlement_ok,
    has_valid_ohlc = ohlc_ok,
    execution_supported = execution_ok,
    data_quality = quality
  )
}

.brf_bundle_empty_roll_schedule <- function() {
  data.frame(
    event_id = character(),
    synthetic_ticker = character(),
    root = character(),
    decision_date = as.Date(character()),
    eligibility_start_date = as.Date(character()),
    nominal_execution_date = as.Date(character()),
    nominal_effective_date = as.Date(character()),
    execution_date = as.Date(character()),
    effective_date = as.Date(character()),
    from_contract = character(),
    to_contract = character(),
    requested_nominal_contract = character(),
    target_selection_reason = character(),
    from_maturity = as.Date(character()),
    to_maturity = as.Date(character()),
    from_last_trade_date = as.Date(character()),
    to_last_trade_date = as.Date(character()),
    from_settlement_price = numeric(),
    to_settlement_price = numeric(),
    adjustment_ratio = numeric(),
    inverse_adjustment_ratio = numeric(),
    reason = character(),
    execution_offset_sessions = integer(),
    early_sessions_used = integer(),
    grace_sessions_used = integer(),
    roll_early = logical(),
    roll_delayed = logical(),
    roll_warning_threshold_exceeded = logical(),
    roll_grace_sessions = integer(),
    candidate_count = integer(),
    total_pending_sessions = integer(),
    skipped_nominal_contracts = character(),
    skipped_nominal_count = integer(),
    execution_days_to_last_trade = integer(),
    incoming_ohlc_valid = logical(),
    rejected_candidate_reasons = character(),
    same_root = logical(),
    anchors_present = logical(),
    anchors_positive = logical(),
    overlap_valid = logical(),
    validated = logical(),
    stringsAsFactors = FALSE
  )
}

.brf_bundle_roll_schedule <- function(selected,
                                      execution_data,
                                      synthetic_ticker,
                                      root,
                                      roll_timing = NULL) {
  if (nrow(selected) <= 1L) {
    return(.brf_bundle_empty_roll_schedule())
  }
  changes <- which(
    selected$contract[-1L] != utils::head(selected$contract, -1L)
  ) + 1L
  if (!length(changes)) {
    return(.brf_bundle_empty_roll_schedule())
  }

  result <- vector("list", length(changes))
  for (i in seq_along(changes)) {
    position <- changes[i]
    from_selected <- selected[position - 1L, , drop = FALSE]
    to_selected <- selected[position, , drop = FALSE]
    execution_date <- from_selected$date
    effective_date <- to_selected$date
    from_contract <- from_selected$contract
    to_contract <- to_selected$contract
    timing <- if (is.data.frame(roll_timing) && nrow(roll_timing)) {
      hit <- which(
        roll_timing$execution_date == execution_date &
          roll_timing$effective_date == effective_date &
          roll_timing$from_contract == from_contract &
          roll_timing$to_contract == to_contract
      )
      if (length(hit) != 1L) {
        stop("Actual roll map does not match its nominal timing audit.", call. = FALSE)
      }
      roll_timing[hit, , drop = FALSE]
    } else {
      data.frame(
        decision_date = execution_date,
        eligibility_start_date = execution_date,
        nominal_execution_date = execution_date,
        nominal_effective_date = effective_date,
        execution_date = execution_date,
        effective_date = effective_date,
        from_contract = from_contract,
        to_contract = to_contract,
        requested_nominal_contract = to_contract,
        target_selection_reason = "nominal_target",
        execution_offset_sessions = 0L,
        early_sessions_used = 0L,
        grace_sessions_used = 0L,
        roll_early = FALSE,
        roll_delayed = FALSE,
        roll_warning_threshold_exceeded = FALSE,
        roll_grace_sessions = 0L,
        candidate_count = 1L,
        total_pending_sessions = 1L,
        skipped_nominal_contracts = NA_character_,
        skipped_nominal_count = 0L,
        execution_days_to_last_trade = as.integer(
          from_selected$last_trade_date - execution_date
        ),
        incoming_ohlc_valid = TRUE,
        rejected_candidate_reasons = NA_character_,
        stringsAsFactors = FALSE
      )
    }

    from_anchor <- execution_data[
      execution_data$date == execution_date & execution_data$contract == from_contract,
      ,
      drop = FALSE
    ]
    to_anchor <- execution_data[
      execution_data$date == execution_date & execution_data$contract == to_contract,
      ,
      drop = FALSE
    ]
    overlap_valid <- nrow(from_anchor) == 1L && nrow(to_anchor) == 1L
    from_settlement <- if (nrow(from_anchor) == 1L) from_anchor$settlement_price else NA_real_
    to_settlement <- if (nrow(to_anchor) == 1L) to_anchor$settlement_price else NA_real_
    anchors_present <- is.finite(from_settlement) && is.finite(to_settlement)
    anchors_positive <- anchors_present && from_settlement > 0 && to_settlement > 0
    same_root <- nrow(from_anchor) == 1L && nrow(to_anchor) == 1L &&
      identical(from_anchor$root, root) && identical(to_anchor$root, root)
    validated <- overlap_valid && anchors_present && anchors_positive && same_root

    if (!validated) {
      stop(
        "Exact roll from '", from_contract, "' to '", to_contract,
        "' requires positive official settlement_price for both contracts on ",
        format(execution_date), "; no close/last-observation fallback is allowed.",
        call. = FALSE
      )
    }

    ratio <- to_settlement / from_settlement
    inverse_ratio <- from_settlement / to_settlement
    if (!is.finite(ratio) || ratio <= 0 || !is.finite(inverse_ratio) || inverse_ratio <= 0) {
      stop("Exact roll produced an invalid multiplicative adjustment ratio.", call. = FALSE)
    }
    event_id <- paste(
      synthetic_ticker,
      format(effective_date, "%Y%m%d"),
      from_contract,
      to_contract,
      sep = "::"
    )
    result[[i]] <- data.frame(
      event_id = event_id,
      synthetic_ticker = synthetic_ticker,
      root = root,
      decision_date = timing$decision_date,
      eligibility_start_date = timing$eligibility_start_date,
      nominal_execution_date = timing$nominal_execution_date,
      nominal_effective_date = timing$nominal_effective_date,
      execution_date = execution_date,
      effective_date = effective_date,
      from_contract = from_contract,
      to_contract = to_contract,
      requested_nominal_contract = timing$requested_nominal_contract,
      target_selection_reason = timing$target_selection_reason,
      from_maturity = from_selected$maturity,
      to_maturity = to_selected$maturity,
      from_last_trade_date = from_selected$last_trade_date,
      to_last_trade_date = to_selected$last_trade_date,
      from_settlement_price = from_settlement,
      to_settlement_price = to_settlement,
      adjustment_ratio = ratio,
      inverse_adjustment_ratio = inverse_ratio,
      reason = "calendar_days_to_last_trade",
      execution_offset_sessions = timing$execution_offset_sessions,
      early_sessions_used = timing$early_sessions_used,
      grace_sessions_used = timing$grace_sessions_used,
      roll_early = timing$roll_early,
      roll_delayed = timing$roll_delayed,
      roll_warning_threshold_exceeded =
        timing$roll_warning_threshold_exceeded,
      roll_grace_sessions = timing$roll_grace_sessions,
      candidate_count = timing$candidate_count,
      total_pending_sessions = timing$total_pending_sessions,
      skipped_nominal_contracts = timing$skipped_nominal_contracts,
      skipped_nominal_count = timing$skipped_nominal_count,
      execution_days_to_last_trade = timing$execution_days_to_last_trade,
      incoming_ohlc_valid = timing$incoming_ohlc_valid,
      rejected_candidate_reasons = timing$rejected_candidate_reasons,
      same_root = same_root,
      anchors_present = anchors_present,
      anchors_positive = anchors_positive,
      overlap_valid = overlap_valid,
      validated = validated,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  out
}

.brf_bundle_adjustment_factors <- function(selected,
                                           roll_schedule,
                                           adjustment_direction) {
  factors <- rep(1, nrow(selected))
  if (!nrow(roll_schedule)) {
    return(factors)
  }
  for (i in seq_len(nrow(roll_schedule))) {
    position <- match(roll_schedule$effective_date[i], selected$date)
    if (is.na(position) || position <= 1L) {
      stop("Internal roll schedule does not match the selected contract map.", call. = FALSE)
    }
    if (identical(adjustment_direction, "backward")) {
      factors[seq_len(position - 1L)] <-
        factors[seq_len(position - 1L)] * roll_schedule$adjustment_ratio[i]
    } else {
      factors[position:length(factors)] <-
        factors[position:length(factors)] * roll_schedule$inverse_adjustment_ratio[i]
    }
  }
  factors
}

.brf_bundle_signal_series <- function(selected, synthetic_ticker) {
  factor <- selected$factor
  data.frame(
    date = selected$date,
    synthetic_ticker = synthetic_ticker,
    root = selected$root,
    active_contract = selected$contract,
    signal_contract = selected$contract,
    open = selected$open * factor,
    high = selected$high * factor,
    low = selected$low * factor,
    close = selected$close * factor,
    settlement_price = selected$settlement_price * factor,
    volume = selected$volume,
    volume_qty = selected$volume_qty,
    open_interest = selected$open_interest,
    close_interest = selected$close_interest,
    trade_count = selected$trade_count,
    factor = factor,
    inverse_factor = selected$inverse_factor,
    stringsAsFactors = FALSE
  )
}

.brf_bundle_validate_adjusted_ohlc <- function(signal_series) {
  quality <- .brf_bundle_price_quality(signal_series)
  if (any(!quality$has_valid_ohlc)) {
    stop("Adjusted continuous OHLC failed the price-ordering invariant.", call. = FALSE)
  }
  invisible(TRUE)
}

.brf_bundle_contract_map <- function(selected,
                                     signal_selected,
                                     signal_rolls,
                                     rolls,
                                     synthetic_ticker,
                                     days_before_roll,
                                     adjustment_direction) {
  is_roll <- c(
    FALSE,
    selected$contract[-1L] != utils::head(selected$contract, -1L)
  )
  event_id <- rep(NA_character_, nrow(selected))
  if (nrow(rolls)) {
    idx <- match(rolls$effective_date, selected$date)
    event_id[idx] <- rolls$event_id
  }
  signal_event_id <- rep(NA_character_, nrow(selected))
  signal_anchor_date <- as.Date(rep(NA_character_, nrow(selected)))
  signal_anchor_ratio <- rep(NA_real_, nrow(selected))
  signal_anchor_lag_sessions <- rep(NA_integer_, nrow(selected))
  signal_first_date <- as.Date(rep(NA_character_, nrow(selected)))
  if (nrow(signal_rolls)) {
    idx <- match(signal_rolls$effective_date, selected$date)
    signal_event_id[idx] <- signal_rolls$event_id
    signal_anchor_date[idx] <- signal_rolls$anchor_date
    signal_anchor_ratio[idx] <- signal_rolls$adjustment_ratio
    signal_anchor_lag_sessions[idx] <- signal_rolls$anchor_lag_sessions
    signal_first_date[idx] <- signal_rolls$first_signal_date
  }
  data.frame(
    date = selected$date,
    synthetic_ticker = synthetic_ticker,
    root = selected$root,
    active_contract = selected$contract,
    nominal_active_contract = selected$nominal_active_contract,
    signal_contract = signal_selected$contract,
    maturity = selected$maturity,
    last_trade_date = selected$last_trade_date,
    days_to_last_trade = as.integer(selected$last_trade_date - selected$date),
    signal_maturity = signal_selected$maturity,
    signal_last_trade_date = signal_selected$last_trade_date,
    signal_days_to_last_trade = as.integer(
      signal_selected$last_trade_date - signal_selected$date
    ),
    roll_offset = days_before_roll,
    factor = selected$signal_factor,
    inverse_factor = selected$signal_inverse_factor,
    signal_adjustment_factor = selected$signal_factor,
    signal_inverse_factor = selected$signal_inverse_factor,
    execution_adjustment_factor = selected$execution_factor,
    execution_inverse_factor = selected$execution_inverse_factor,
    signal_coordinate_asof_date = selected$signal_coordinate_asof_date,
    order_transform_factor = selected$order_transform_factor,
    order_transform_inverse_factor =
      selected$order_transform_inverse_factor,
    order_transform_asof_date = selected$order_transform_asof_date,
    order_transform_source = selected$order_transform_source,
    order_transform_available = selected$order_transform_available,
    contracts_aligned = selected$contract == signal_selected$contract,
    adjustment_direction = adjustment_direction,
    adjustment_anchor = "official_settlement",
    roll_rule = "calendar_days_to_last_trade",
    is_roll_effective = is_roll,
    is_roll_execution = selected$is_roll_execution,
    roll_pending = selected$roll_pending,
    roll_grace_session = selected$roll_grace_session,
    roll_lead_session = selected$roll_lead_session,
    roll_mapping_state = selected$roll_mapping_state,
    roll_delay_reason = selected$roll_delay_reason,
    roll_event_id = event_id,
    signal_roll_event_id = signal_event_id,
    signal_adjustment_anchor_date = signal_anchor_date,
    signal_first_observed_date = signal_first_date,
    signal_adjustment_ratio = signal_anchor_ratio,
    signal_adjustment_anchor_lag_sessions = signal_anchor_lag_sessions,
    signal_available = signal_selected$signal_available,
    signal_data_valid = signal_selected$signal_available,
    has_official_settlement = selected$has_official_settlement,
    has_valid_ohlc = selected$has_valid_ohlc,
    marking_supported = selected$marking_supported,
    execution_tradable = selected$execution_tradable,
    execution_supported = selected$execution_supported,
    data_quality = selected$data_quality,
    stringsAsFactors = FALSE
  )
}

.brf_bundle_execution_export <- function(canonical,
                                         original,
                                         columns,
                                         synthetic_ticker) {
  fixed <- data.frame(
    date = canonical$date,
    synthetic_ticker = synthetic_ticker,
    root = canonical$root,
    contract = canonical$contract,
    maturity = canonical$maturity,
    last_trade_date = canonical$last_trade_date,
    open = canonical$open,
    high = canonical$high,
    low = canonical$low,
    close = canonical$close,
    settlement_price = canonical$settlement_price,
    volume = canonical$volume,
    volume_qty = canonical$volume_qty,
    open_interest = canonical$open_interest,
    close_interest = canonical$close_interest,
    trade_count = canonical$trade_count,
    source = canonical$source,
    stringsAsFactors = FALSE
  )
  used_columns <- unique(unname(unlist(columns, use.names = FALSE)))
  used_columns <- used_columns[!is.na(used_columns)]
  extra_names <- setdiff(names(original), used_columns)
  if (length(extra_names)) {
    extras <- original[canonical$.input_row, extra_names, drop = FALSE]
    duplicate <- names(extras) %in% names(fixed)
    extras <- extras[, !duplicate, drop = FALSE]
    if (ncol(extras)) {
      fixed <- cbind(fixed, extras)
    }
  }
  fixed <- fixed[order(fixed$date, fixed$last_trade_date, fixed$contract), , drop = FALSE]
  rownames(fixed) <- NULL
  fixed
}
