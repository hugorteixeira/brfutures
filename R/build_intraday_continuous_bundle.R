#' Build a provider-neutral intraday B3 continuous futures bundle
#'
#' Expands an execution-aware daily B3 bundle produced by
#' [build_continuous_bundle()] onto raw intraday bars from dated contracts. The
#' adjusted continuous series is signal-only. Raw dated-contract bars and
#' official daily settlements remain separate execution inputs.
#'
#' `timestamp` is the bar-open timestamp. Unless supplied explicitly,
#' `available_at` and `information_cutoff` are set to the bar close implied by
#' `timeframe`. The function never invents an intraday timestamp for an
#' official settlement: settlements retain a `Date` and an explicit
#' end-of-session phase.
#'
#' @param daily_bundle A schema-v3 `brf_continuous_bundle` built from official
#'   B3 daily data.
#' @param bars Non-empty data frame of raw intraday dated-contract bars.
#'   Required columns are `timestamp`, `root`, `contract`, `open`, `high`,
#'   `low`, and `close`. Optional canonical columns include `session_date`,
#'   `available_at`, `information_cutoff`, `maturity`, `source_volume`,
#'   `source_volume_semantics`, `source_series_id`, `source_table`,
#'   `source_timezone`, and `data_quality`.
#'   Provider adapters that aggregate a finer raw grid should also supply
#'   `observed_child_bars`, `expected_child_bars`, and
#'   `observed_grid_policy`.
#' @param timeframe Intraday bar width, for example `"1m"`, `"15m"`, `"1h"`,
#'   or `"4h"`.
#' @param source Provider identifier such as `"mt5"` or `"barchart"`.
#' @param session_tz Olson timezone used to derive `session_date` from bar-open
#'   timestamps. B3 data normally uses `"America/Sao_Paulo"`.
#' @param strict Logical scalar. In strict mode unknown sessions/contracts,
#'   invalid OHLC, missing official active-contract settlements, or missing
#'   mapped contract coverage stop the build. Sessions without an intraday bar
#'   are valid: they remain in `session_map` and can still be marked by their
#'   official settlement, but cannot produce a fill.
#' @param grid_policy Raw-child completeness policy when `bars` carries
#'   `observed_child_bars` and `expected_child_bars`. The default,
#'   `"require_complete_grid"`, fails closed on partial buckets.
#'   `"allow_observed_sparse"` is an explicit, manifest-recorded opt-in; it
#'   preserves observed rows and never fabricates missing children.
#'
#' @return An object of class `brf_intraday_continuous_bundle` containing
#'   adjusted `signal_series`, union-clock `bar_map`, daily `session_map`,
#'   official `roll_schedule`, raw dated-contract `execution_data`, daily
#'   `settlement_data`, and a versioned `manifest`.
#' @export
build_intraday_continuous_bundle <- function(daily_bundle,
                                              bars,
                                              timeframe,
                                              source,
                                              session_tz = "America/Sao_Paulo",
                                              strict = TRUE,
                                              grid_policy =
                                                c(
                                                  "require_complete_grid",
                                                  "allow_observed_sparse"
                                                )) {
  strict <- .brf_intraday_scalar_logical(strict, "strict")
  grid_policy <- match.arg(grid_policy)
  timeframe_info <- .brf_intraday_timeframe(timeframe)
  source <- .brf_intraday_scalar_character(source, "source")
  session_tz <- .brf_intraday_timezone(session_tz)
  daily <- .brf_intraday_validate_daily_bundle(daily_bundle)

  if (!is.data.frame(bars) || !nrow(bars)) {
    stop("`bars` must be a non-empty data frame of raw dated-contract bars.", call. = FALSE)
  }
  required <- c("timestamp", "root", "contract", "open", "high", "low", "close")
  missing <- setdiff(required, names(bars))
  if (length(missing)) {
    stop(
      "`bars` is missing required canonical column(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  synthetic_ticker <- paste0(
    daily$manifest$synthetic_ticker,
    "_",
    timeframe_info$label
  )
  canonical <- .brf_intraday_canonical_bars(
    bars = bars,
    source = source,
    session_tz = session_tz,
    timeframe_seconds = timeframe_info$seconds,
    synthetic_ticker = synthetic_ticker,
    strict = strict
  )
  row_grid_policy <- unique(as.character(canonical$observed_grid_policy))
  row_grid_policy <- row_grid_policy[
    !is.na(row_grid_policy) & nzchar(row_grid_policy)
  ]
  if (length(row_grid_policy) &&
      (length(row_grid_policy) != 1L ||
        !identical(row_grid_policy, grid_policy))) {
    stop(
      "Intraday bar grid policy disagrees with the requested grid_policy.",
      call. = FALSE
    )
  }
  child_grid_audited <- !is.na(canonical$observed_child_bars) &
    !is.na(canonical$expected_child_bars)
  sparse_observed <- child_grid_audited &
    canonical$observed_child_bars < canonical$expected_child_bars
  if (identical(grid_policy, "require_complete_grid") &&
      any(sparse_observed)) {
    stop(
      "Sparse observed child grids require explicit ",
      "grid_policy = 'allow_observed_sparse'.",
      call. = FALSE
    )
  }

  root <- as.character(daily$manifest$root)
  root_match <- canonical$root == root
  if (any(!root_match)) {
    message <- paste0(
      "Intraday bars include ", sum(!root_match),
      " row(s) outside daily bundle root '", root, "'."
    )
    if (strict) {
      stop(message, call. = FALSE)
    }
    canonical <- canonical[root_match, , drop = FALSE]
  }
  if (!nrow(canonical)) {
    stop("No intraday bars remain for the daily bundle root.", call. = FALSE)
  }

  date_range <- range(canonical$session_date)
  session_map <- daily$contract_map[
    daily$contract_map$date >= date_range[[1L]] &
      daily$contract_map$date <= date_range[[2L]],
    ,
    drop = FALSE
  ]
  if (!nrow(session_map)) {
    stop("Intraday bar dates do not overlap the daily bundle session map.", call. = FALSE)
  }
  session_map$date <- as.Date(session_map$date)
  if (anyDuplicated(session_map$date)) {
    stop("Daily bundle contract_map must contain one row per session.", call. = FALSE)
  }

  unknown_sessions <- setdiff(unique(canonical$session_date), session_map$date)
  if (length(unknown_sessions)) {
    message <- paste0(
      "Intraday bars contain session(s) absent from the official daily map: ",
      paste(format(utils::head(unknown_sessions, 5L)), collapse = ", "), "."
    )
    if (strict) {
      stop(message, call. = FALSE)
    }
    canonical <- canonical[
      canonical$session_date %in% session_map$date,
      ,
      drop = FALSE
    ]
  }
  if (!nrow(canonical)) {
    stop("No intraday bars remain on official mapped sessions.", call. = FALSE)
  }

  known_contracts <- unique(as.character(daily$execution_data$contract))
  unknown_contract <- !canonical$contract %in% known_contracts
  if (any(unknown_contract)) {
    unknown <- sort(unique(canonical$contract[unknown_contract]))
    message <- paste0(
      "Intraday bars contain contract(s) absent from official daily data: ",
      paste(utils::head(unknown, 10L), collapse = ", "), "."
    )
    if (strict) {
      stop(message, call. = FALSE)
    }
    canonical <- canonical[!unknown_contract, , drop = FALSE]
  }
  if (!nrow(canonical)) {
    stop("No known dated-contract intraday bars remain.", call. = FALSE)
  }

  map_index <- match(canonical$session_date, session_map$date)
  contract_metadata <- unique(daily$execution_data[
    ,
    c("contract", "maturity", "last_trade_date"),
    drop = FALSE
  ])
  metadata_key <- as.character(contract_metadata$contract)
  if (anyDuplicated(metadata_key)) {
    maturity_conflict <- vapply(
      split(contract_metadata, metadata_key),
      function(rows) {
        length(unique(as.Date(rows$maturity))) > 1L ||
          length(unique(as.Date(rows$last_trade_date))) > 1L
      },
      logical(1L)
    )
    if (any(maturity_conflict)) {
      stop("Official daily contract metadata is not stable by contract.", call. = FALSE)
    }
    contract_metadata <- contract_metadata[!duplicated(metadata_key), , drop = FALSE]
    metadata_key <- as.character(contract_metadata$contract)
  }
  metadata_index <- match(canonical$contract, metadata_key)
  canonical$maturity[is.na(canonical$maturity)] <- as.Date(
    contract_metadata$maturity[metadata_index][is.na(canonical$maturity)]
  )
  canonical$last_trade_date <- as.Date(
    contract_metadata$last_trade_date[metadata_index]
  )
  canonical$active_contract <- as.character(
    session_map$active_contract[map_index]
  )
  canonical$signal_contract <- as.character(
    session_map$signal_contract[map_index]
  )
  canonical$is_active_contract <- canonical$contract == canonical$active_contract
  canonical$is_signal_contract <- canonical$contract == canonical$signal_contract

  relevant_contracts <- unique(c(
    as.character(session_map$active_contract),
    as.character(session_map$signal_contract),
    as.character(daily$roll_schedule$from_contract),
    as.character(daily$roll_schedule$to_contract)
  ))
  relevant_contracts <- relevant_contracts[
    !is.na(relevant_contracts) & nzchar(relevant_contracts)
  ]
  canonical <- canonical[canonical$contract %in% relevant_contracts, , drop = FALSE]
  if (!nrow(canonical)) {
    stop("No intraday bars remain for mapped signal or execution contracts.", call. = FALSE)
  }

  settlement_data <- .brf_intraday_settlement_data(
    daily = daily,
    session_map = session_map,
    synthetic_ticker = synthetic_ticker,
    strict = strict
  )

  mapped_contracts <- unique(c(
    as.character(session_map$active_contract),
    as.character(session_map$signal_contract)
  ))
  mapped_contracts <- mapped_contracts[
    !is.na(mapped_contracts) & nzchar(mapped_contracts)
  ]
  missing_contracts <- setdiff(mapped_contracts, unique(canonical$contract))
  if (strict && length(missing_contracts)) {
    stop(
      "Exact intraday bundle has no raw dated-contract bars for mapped ",
      "contract(s): ", paste(missing_contracts, collapse = ", "), ".",
      call. = FALSE
    )
  }

  execution_data <- .brf_intraday_execution_data(
    canonical = canonical,
    synthetic_ticker = synthetic_ticker
  )
  active_coverage <- unique(execution_data$contract[
    execution_data$is_active_contract & execution_data$execution_tradable
  ])
  signal_coverage <- unique(execution_data$contract[
    execution_data$is_signal_contract & execution_data$execution_tradable
  ])
  expected_active <- unique(as.character(session_map$active_contract))
  expected_signal <- unique(as.character(session_map$signal_contract))
  missing_active_contracts <- setdiff(expected_active, active_coverage)
  missing_signal_contracts <- setdiff(expected_signal, signal_coverage)
  missing_active_contracts <- missing_active_contracts[
    !is.na(missing_active_contracts) & nzchar(missing_active_contracts)
  ]
  missing_signal_contracts <- missing_signal_contracts[
    !is.na(missing_signal_contracts) & nzchar(missing_signal_contracts)
  ]
  if (strict && length(missing_active_contracts)) {
    stop(
      "Exact intraday execution has no tradable bar while mapped active for ",
      "contract(s): ", paste(missing_active_contracts, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (strict && length(missing_signal_contracts)) {
    stop(
      "Exact intraday signal construction has no tradable bar while mapped ",
      "as signal contract(s): ",
      paste(missing_signal_contracts, collapse = ", "), ".",
      call. = FALSE
    )
  }
  signal_series <- .brf_intraday_signal_series(
    canonical = canonical,
    session_map = session_map,
    synthetic_ticker = synthetic_ticker
  )
  bar_map <- .brf_intraday_bar_map(
    execution_data = execution_data,
    session_map = session_map,
    synthetic_ticker = synthetic_ticker,
    session_tz = session_tz
  )
  roll_schedule <- .brf_intraday_roll_schedule(
    daily$roll_schedule,
    date_range
  )

  coverage_complete <- !length(missing_contracts) &&
    !length(missing_active_contracts) &&
    !length(missing_signal_contracts)
  rolls_supported <- !nrow(roll_schedule) ||
    all(as.logical(roll_schedule$validated))
  execution_supported <- isTRUE(daily$manifest$execution_supported) &&
    coverage_complete &&
    all(session_map$marking_supported) &&
    rolls_supported &&
    nrow(settlement_data) > 0L
  execution_sparse <- grepl(
    "^sparse_observed_child_grid",
    as.character(execution_data$data_quality)
  )
  execution_sparse[is.na(execution_sparse)] <- FALSE
  execution_grid_audited <- !is.na(execution_data$observed_child_bars) &
    !is.na(execution_data$expected_child_bars)
  execution_quality <- if (any(execution_sparse)) {
    "exact_real_contract_bars_observed_sparse_opt_in"
  } else if (length(execution_grid_audited) &&
             all(execution_grid_audited)) {
    "exact_real_contract_bars_complete_observed_grid"
  } else {
    "real_contract_target_bars_child_grid_unreported"
  }

  manifest <- list(
    schema_version = 1L,
    schema_id = "b3_intraday_continuous_v1",
    parent_schema_version = daily$manifest$schema_version,
    parent_execution_supported = isTRUE(daily$manifest$execution_supported),
    bundle_type = "b3_intraday_continuous",
    source = source,
    source_category = "provider_intraday_dated_contracts",
    synthetic_ticker = synthetic_ticker,
    daily_synthetic_ticker = daily$manifest$synthetic_ticker,
    root = root,
    timeframe = timeframe_info$canonical,
    timeframe_seconds = timeframe_info$seconds,
    timestamp_semantics = "bar_open",
    bar_availability_policy = "bar_close_or_later",
    session_timezone = session_tz,
    adjustment_method = daily$manifest$adjustment_method,
    adjustment_direction = daily$manifest$adjustment_direction,
    adjustment_anchor = daily$manifest$adjustment_anchor,
    adjustment_usage = "signal_only",
    execution_price_domain = "raw_dated_contract",
    settlement_source = "B3 official daily",
    settlement_availability = "end_of_session_phase_no_invented_timestamp",
    roll_rule = daily$manifest$roll_rule,
    roll_offset = daily$manifest$roll_offset,
    roll_timing_policy = daily$manifest$roll_timing_policy,
    strategy_fill_policy = "next_eligible_real_contract_bar_after_signal",
    sparse_clock_policy = "union_signal_and_execution_events_no_inner_join",
    observed_grid_policy = grid_policy,
    sparse_observed_opt_in =
      identical(grid_policy, "allow_observed_sparse"),
    sparse_observed_bar_count = sum(execution_sparse),
    complete_observed_bar_count =
      sum(execution_grid_audited & !execution_sparse),
    unaudited_child_grid_bar_count = sum(!execution_grid_audited),
    execution_quality = execution_quality,
    source_volume_semantics = if (identical(tolower(source), "mt5")) {
      "mt5_tick_or_trade_count_unverified"
    } else {
      "provider_reported_unverified_unless_row_metadata_says_otherwise"
    },
    source_volume_usage = "audit_only_not_contract_liquidity_or_sizing",
    requested_start = date_range[[1L]],
    requested_end = date_range[[2L]],
    actual_execution_start = .brf_intraday_date_extreme(
      execution_data$session_date[
        execution_data$is_active_contract & execution_data$execution_tradable
      ],
      min
    ),
    actual_execution_end = .brf_intraday_date_extreme(
      execution_data$session_date[
        execution_data$is_active_contract & execution_data$execution_tradable
      ],
      max
    ),
    strict = strict,
    coverage_complete = coverage_complete,
    missing_mapped_contracts = missing_contracts,
    missing_active_contracts = missing_active_contracts,
    missing_signal_contracts = missing_signal_contracts,
    active_contracts_with_bars = sort(active_coverage),
    signal_contracts_with_bars = sort(signal_coverage),
    execution_supported = execution_supported,
    signal_usage = "adjusted_intraday_signal_only",
    execution_usage = "raw_real_contract_bars_plus_official_daily_settlement",
    row_counts = list(
      signal_series = nrow(signal_series),
      bar_map = nrow(bar_map),
      session_map = nrow(session_map),
      roll_schedule = nrow(roll_schedule),
      execution_data = nrow(execution_data),
      settlement_data = nrow(settlement_data)
    )
  )

  structure(
    list(
      signal_series = signal_series,
      bar_map = bar_map,
      session_map = session_map,
      roll_schedule = roll_schedule,
      execution_data = execution_data,
      settlement_data = settlement_data,
      manifest = manifest
    ),
    class = c("brf_intraday_continuous_bundle", "list")
  )
}

.brf_intraday_scalar_logical <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

.brf_intraday_date_extreme <- function(value, fun) {
  value <- as.Date(value)
  value <- value[!is.na(value)]
  if (!length(value)) {
    return(as.Date(NA))
  }
  as.Date(fun(value))
}

.brf_intraday_scalar_character <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    stop("`", name, "` must be a non-empty character scalar.", call. = FALSE)
  }
  trimws(value)
}

.brf_intraday_timezone <- function(value) {
  value <- .brf_intraday_scalar_character(value, "session_tz")
  if (!value %in% OlsonNames()) {
    stop("`session_tz` must be a valid Olson timezone.", call. = FALSE)
  }
  value
}

.brf_intraday_timeframe <- function(value) {
  value <- tolower(.brf_intraday_scalar_character(value, "timeframe"))
  match <- regexec("^([1-9][0-9]*)(m|h)$", value)
  parts <- regmatches(value, match)[[1L]]
  if (!length(parts)) {
    stop(
      "`timeframe` must be an intraday width such as '1m', '15m', '1h', or '4h'.",
      call. = FALSE
    )
  }
  amount <- as.integer(parts[[2L]])
  unit <- parts[[3L]]
  seconds <- amount * if (identical(unit, "m")) 60L else 3600L
  if (!is.finite(seconds) || seconds <= 0L || seconds >= 86400L) {
    stop("`timeframe` must be shorter than one day.", call. = FALSE)
  }
  canonical <- paste0(amount, unit)
  list(
    canonical = canonical,
    label = toupper(canonical),
    seconds = as.integer(seconds)
  )
}

.brf_intraday_validate_daily_bundle <- function(daily_bundle) {
  required <- c(
    "signal_series", "contract_map", "roll_schedule",
    "execution_data", "manifest"
  )
  if (!is.list(daily_bundle) || !all(required %in% names(daily_bundle))) {
    stop(
      "`daily_bundle` must be an execution-aware daily continuous bundle.",
      call. = FALSE
    )
  }
  manifest <- daily_bundle$manifest
  if (!identical(manifest$bundle_type, "b3_daily_continuous") ||
      !identical(as.integer(manifest$schema_version), 3L)) {
    stop(
      "`daily_bundle` must use b3_daily_continuous schema version 3.",
      call. = FALSE
    )
  }
  if (!identical(manifest$adjustment_anchor, "official_settlement")) {
    stop("Intraday expansion requires official-settlement adjustment anchors.", call. = FALSE)
  }
  map_required <- c(
    "date", "active_contract", "signal_contract",
    "signal_adjustment_factor", "signal_inverse_factor",
    "order_transform_factor", "order_transform_inverse_factor",
    "order_transform_asof_date", "order_transform_available",
    "marking_supported"
  )
  missing_map <- setdiff(map_required, names(daily_bundle$contract_map))
  if (length(missing_map)) {
    stop(
      "Daily bundle contract_map is missing field(s): ",
      paste(missing_map, collapse = ", "), ".",
      call. = FALSE
    )
  }
  execution_required <- c(
    "date", "root", "contract", "maturity", "last_trade_date",
    "settlement_price"
  )
  missing_execution <- setdiff(execution_required, names(daily_bundle$execution_data))
  if (length(missing_execution)) {
    stop(
      "Daily bundle execution_data is missing field(s): ",
      paste(missing_execution, collapse = ", "), ".",
      call. = FALSE
    )
  }
  daily_bundle
}

.brf_intraday_rejected_symbol <- function(contract) {
  grepl(
    "(FUT|\\$)(?:$|_)|_(OLD|AGG)(?:$|_)",
    contract,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.brf_intraday_canonical_bars <- function(bars,
                                         source,
                                         session_tz,
                                         timeframe_seconds,
                                         synthetic_ticker,
                                         strict) {
  timestamp <- bars$timestamp
  if (!inherits(timestamp, "POSIXct")) {
    stop("`bars$timestamp` must be POSIXct and represent bar-open time.", call. = FALSE)
  }
  if (anyNA(timestamp)) {
    stop("`bars$timestamp` cannot contain missing values.", call. = FALSE)
  }

  contract <- toupper(trimws(as.character(bars$contract)))
  root <- toupper(trimws(as.character(bars$root)))
  invalid_symbol <- is.na(contract) | !nzchar(contract) |
    .brf_intraday_rejected_symbol(contract) |
    contract == toupper(synthetic_ticker)
  if (any(invalid_symbol)) {
    bad <- sort(unique(contract[invalid_symbol]))
    stop(
      "Intraday execution requires raw dated-contract symbols; rejected: ",
      paste(utils::head(bad, 10L), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(is.na(root) | !nzchar(root))) {
    stop("Every intraday bar must have a non-empty `root`.", call. = FALSE)
  }

  derived_session_date <- as.Date(timestamp, tz = session_tz)
  session_date <- if ("session_date" %in% names(bars)) {
    as.Date(bars$session_date)
  } else {
    derived_session_date
  }
  if (anyNA(session_date)) {
    stop("Every intraday bar must map to a valid `session_date`.", call. = FALSE)
  }
  if (any(session_date != derived_session_date)) {
    stop(
      "Supplied `session_date` disagrees with bar-open timestamp in `session_tz`.",
      call. = FALSE
    )
  }

  bar_close_at <- timestamp + timeframe_seconds
  available_at <- if ("available_at" %in% names(bars)) {
    bars$available_at
  } else {
    bar_close_at
  }
  if (!inherits(available_at, "POSIXct")) {
    stop("`bars$available_at` must be POSIXct when supplied.", call. = FALSE)
  }
  information_cutoff <- if ("information_cutoff" %in% names(bars)) {
    bars$information_cutoff
  } else {
    bar_close_at
  }
  if (!inherits(information_cutoff, "POSIXct")) {
    stop("`bars$information_cutoff` must be POSIXct when supplied.", call. = FALSE)
  }
  causal_invalid <- is.na(available_at) | is.na(information_cutoff) |
    available_at < bar_close_at |
    information_cutoff < bar_close_at |
    information_cutoff > available_at
  if (any(causal_invalid)) {
    stop(
      "Intraday bar availability must be at or after bar close, and ",
      "information_cutoff cannot exceed available_at.",
      call. = FALSE
    )
  }

  prices <- lapply(
    c("open", "high", "low", "close"),
    function(name) suppressWarnings(as.numeric(bars[[name]]))
  )
  names(prices) <- c("open", "high", "low", "close")
  finite_positive <- Reduce(
    `&`,
    lapply(prices, function(value) is.finite(value) & value > 0)
  )
  ordered <- prices$low <= pmin(prices$open, prices$close, prices$high) &
    prices$high >= pmax(prices$open, prices$close, prices$low)
  valid_ohlc <- finite_positive & ordered
  if (strict && any(!valid_ohlc)) {
    bad <- which(!valid_ohlc)[[1L]]
    stop(
      "Invalid raw intraday OHLC for contract '", contract[[bad]],
      "' at ", format(timestamp[[bad]], tz = session_tz), ".",
      call. = FALSE
    )
  }

  source_volume <- if ("source_volume" %in% names(bars)) {
    suppressWarnings(as.numeric(bars$source_volume))
  } else {
    rep(NA_real_, nrow(bars))
  }
  source_volume_semantics <- if (identical(tolower(source), "mt5")) {
    rep("mt5_tick_or_trade_count_unverified", nrow(bars))
  } else if ("source_volume_semantics" %in% names(bars)) {
    as.character(bars$source_volume_semantics)
  } else {
    rep("provider_reported_unverified", nrow(bars))
  }
  source_volume_semantics[
    is.na(source_volume_semantics) | !nzchar(source_volume_semantics)
  ] <- "provider_reported_unverified"

  maturity <- if ("maturity" %in% names(bars)) {
    as.Date(bars$maturity)
  } else {
    as.Date(rep(NA_character_, nrow(bars)))
  }
  input_quality <- if ("data_quality" %in% names(bars)) {
    as.character(bars$data_quality)
  } else {
    rep("provider_raw", nrow(bars))
  }
  input_quality[is.na(input_quality) | !nzchar(input_quality)] <- "provider_raw"
  data_quality <- ifelse(
    valid_ohlc,
    input_quality,
    "invalid_ohlc_non_executable"
  )
  has_observed_children <- "observed_child_bars" %in% names(bars)
  has_expected_children <- "expected_child_bars" %in% names(bars)
  if (xor(has_observed_children, has_expected_children)) {
    stop(
      "Intraday child-grid audit requires both observed_child_bars and ",
      "expected_child_bars.",
      call. = FALSE
    )
  }
  observed_child_bars <- if (has_observed_children) {
    suppressWarnings(as.integer(bars$observed_child_bars))
  } else {
    rep(NA_integer_, nrow(bars))
  }
  expected_child_bars <- if (has_expected_children) {
    suppressWarnings(as.integer(bars$expected_child_bars))
  } else {
    rep(NA_integer_, nrow(bars))
  }
  if (has_observed_children) {
    invalid_children <- is.na(observed_child_bars) |
      is.na(expected_child_bars) |
      observed_child_bars <= 0L |
      expected_child_bars <= 0L |
      observed_child_bars > expected_child_bars
    if (any(invalid_children)) {
      stop("Intraday bars contain invalid raw-child grid counts.", call. = FALSE)
    }
    count_sparse <- observed_child_bars < expected_child_bars
    quality_sparse <- grepl("^sparse_observed_child_grid", data_quality)
    quality_sparse[is.na(quality_sparse)] <- FALSE
    if (any(count_sparse != quality_sparse)) {
      stop(
        "Intraday data_quality disagrees with observed raw-child grid counts.",
        call. = FALSE
      )
    }
  }

  optional_character <- function(name, default) {
    if (name %in% names(bars)) as.character(bars[[name]]) else rep(default, nrow(bars))
  }
  out <- data.frame(
    timestamp = timestamp,
    bar_close_at = bar_close_at,
    available_at = available_at,
    information_cutoff = information_cutoff,
    session_date = session_date,
    root = root,
    contract = contract,
    maturity = maturity,
    open = prices$open,
    high = prices$high,
    low = prices$low,
    close = prices$close,
    source_volume = source_volume,
    source_volume_semantics = source_volume_semantics,
    source = rep(source, nrow(bars)),
    source_series_id = optional_character("source_series_id", NA_character_),
    source_table = optional_character("source_table", NA_character_),
    source_timezone = optional_character("source_timezone", session_tz),
    observed_child_bars = observed_child_bars,
    expected_child_bars = expected_child_bars,
    observed_grid_policy =
      optional_character("observed_grid_policy", NA_character_),
    data_quality = data_quality,
    execution_tradable = valid_ohlc,
    stringsAsFactors = FALSE
  )
  duplicate_key <- paste(as.numeric(out$timestamp), out$contract, sep = "::")
  if (anyDuplicated(duplicate_key)) {
    stop("Intraday bars must be unique by timestamp and contract.", call. = FALSE)
  }
  out <- out[order(out$timestamp, out$contract), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.brf_intraday_settlement_data <- function(daily,
                                           session_map,
                                           synthetic_ticker,
                                           strict) {
  raw <- daily$execution_data
  raw$date <- as.Date(raw$date)
  keep <- raw$date %in% session_map$date
  raw <- raw[keep, , drop = FALSE]
  fixed <- data.frame(
    date = raw$date,
    synthetic_ticker = synthetic_ticker,
    root = as.character(raw$root),
    contract = as.character(raw$contract),
    maturity = as.Date(raw$maturity),
    last_trade_date = as.Date(raw$last_trade_date),
    settlement_price = suppressWarnings(as.numeric(raw$settlement_price)),
    source = if ("source" %in% names(raw)) {
      as.character(raw$source)
    } else {
      rep("B3 official daily", nrow(raw))
    },
    availability_phase = "end_of_session_official_settlement",
    stringsAsFactors = FALSE
  )
  duplicate <- paste(fixed$date, fixed$contract, sep = "::")
  if (anyDuplicated(duplicate)) {
    stop("Official settlement data must be unique by date and contract.", call. = FALSE)
  }

  active_key <- paste(session_map$date, session_map$active_contract, sep = "::")
  settlement_key <- paste(fixed$date, fixed$contract, sep = "::")
  hit <- match(active_key, settlement_key)
  missing <- is.na(hit) |
    !is.finite(fixed$settlement_price[hit]) |
    fixed$settlement_price[hit] <= 0
  if (strict && any(missing)) {
    bad <- which(missing)[[1L]]
    stop(
      "Missing positive official settlement for active contract '",
      session_map$active_contract[[bad]], "' on ",
      format(session_map$date[[bad]]), ".",
      call. = FALSE
    )
  }
  fixed <- fixed[order(fixed$date, fixed$last_trade_date, fixed$contract), , drop = FALSE]
  rownames(fixed) <- NULL
  fixed
}

.brf_intraday_execution_data <- function(canonical, synthetic_ticker) {
  keep <- c(
    "timestamp", "bar_close_at", "available_at", "information_cutoff",
    "session_date", "root", "contract", "maturity",
    "last_trade_date",
    "open", "high", "low", "close",
    "source_volume", "source_volume_semantics", "source",
    "source_series_id", "source_table", "source_timezone",
    "observed_child_bars", "expected_child_bars",
    "observed_grid_policy",
    "active_contract", "signal_contract",
    "is_active_contract", "is_signal_contract",
    "execution_tradable", "data_quality"
  )
  out <- canonical[, keep, drop = FALSE]
  out$synthetic_ticker <- synthetic_ticker
  out <- out[, c("timestamp", "synthetic_ticker", setdiff(names(out), "timestamp")), drop = FALSE]
  rownames(out) <- NULL
  out
}

.brf_intraday_signal_series <- function(canonical,
                                         session_map,
                                         synthetic_ticker) {
  selected <- canonical[
    canonical$is_signal_contract & canonical$execution_tradable,
    ,
    drop = FALSE
  ]
  if (!nrow(selected)) {
    stop("No valid mapped signal-contract intraday bars remain.", call. = FALSE)
  }
  idx <- match(selected$session_date, session_map$date)
  factor <- suppressWarnings(as.numeric(
    session_map$signal_adjustment_factor[idx]
  ))
  inverse_factor <- suppressWarnings(as.numeric(
    session_map$signal_inverse_factor[idx]
  ))
  valid_factor <- is.finite(factor) & factor > 0 &
    is.finite(inverse_factor) & inverse_factor > 0
  if (any(!valid_factor)) {
    bad <- which(!valid_factor)[[1L]]
    stop(
      "Missing positive signal adjustment factor on ",
      format(selected$session_date[[bad]]), ".",
      call. = FALSE
    )
  }
  out <- data.frame(
    timestamp = selected$timestamp,
    bar_close_at = selected$bar_close_at,
    available_at = selected$available_at,
    information_cutoff = selected$information_cutoff,
    session_date = selected$session_date,
    synthetic_ticker = synthetic_ticker,
    root = selected$root,
    signal_contract = selected$contract,
    open = selected$open * factor,
    high = selected$high * factor,
    low = selected$low * factor,
    close = selected$close * factor,
    source_volume = selected$source_volume,
    source_volume_semantics = selected$source_volume_semantics,
    observed_child_bars = selected$observed_child_bars,
    expected_child_bars = selected$expected_child_bars,
    observed_grid_policy = selected$observed_grid_policy,
    factor = factor,
    inverse_factor = inverse_factor,
    adjustment_usage = "signal_only",
    source = selected$source,
    data_quality = selected$data_quality,
    stringsAsFactors = FALSE
  )
  valid <- is.finite(out$open) & is.finite(out$high) &
    is.finite(out$low) & is.finite(out$close) &
    out$low <= pmin(out$open, out$close, out$high) &
    out$high >= pmax(out$open, out$close, out$low)
  if (any(!valid)) {
    stop("Adjusted intraday signal OHLC failed its ordering invariant.", call. = FALSE)
  }
  out <- out[order(out$timestamp), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.brf_intraday_bar_map <- function(execution_data,
                                   session_map,
                                   synthetic_ticker,
                                   session_tz) {
  relevant <- execution_data[
    execution_data$is_active_contract | execution_data$is_signal_contract,
    ,
    drop = FALSE
  ]
  timestamps <- sort(unique(relevant$timestamp))
  session_dates <- as.Date(timestamps, tz = session_tz)
  idx <- match(session_dates, session_map$date)

  first_or_na <- function(value) {
    if (length(value)) value[[1L]] else as.POSIXct(NA)
  }
  signal_available_at <- as.POSIXct(
    rep(NA_real_, length(timestamps)),
    origin = "1970-01-01",
    tz = session_tz
  )
  execution_available_at <- signal_available_at
  signal_present <- execution_present <- logical(length(timestamps))
  for (i in seq_along(timestamps)) {
    at <- timestamps[[i]]
    rows <- relevant$timestamp == at
    signal_rows <- rows & relevant$is_signal_contract &
      relevant$execution_tradable
    execution_rows <- rows & relevant$is_active_contract &
      relevant$execution_tradable
    signal_present[[i]] <- any(signal_rows)
    execution_present[[i]] <- any(execution_rows)
    signal_available_at[[i]] <- first_or_na(relevant$available_at[signal_rows])
    execution_available_at[[i]] <- first_or_na(
      relevant$available_at[execution_rows]
    )
  }

  order_asof <- as.Date(session_map$order_transform_asof_date[idx])
  order_available_raw <- as.logical(session_map$order_transform_available[idx])
  order_available_raw[is.na(order_available_raw)] <- FALSE
  order_available <- order_available_raw &
    !is.na(order_asof) & order_asof < session_dates
  data.frame(
    timestamp = timestamps,
    session_date = session_dates,
    synthetic_ticker = synthetic_ticker,
    root = as.character(session_map$root[idx]),
    active_contract = as.character(session_map$active_contract[idx]),
    signal_contract = as.character(session_map$signal_contract[idx]),
    signal_adjustment_factor = suppressWarnings(as.numeric(
      session_map$signal_adjustment_factor[idx]
    )),
    signal_inverse_factor = suppressWarnings(as.numeric(
      session_map$signal_inverse_factor[idx]
    )),
    order_transform_factor = suppressWarnings(as.numeric(
      session_map$order_transform_factor[idx]
    )),
    order_transform_inverse_factor = suppressWarnings(as.numeric(
      session_map$order_transform_inverse_factor[idx]
    )),
    order_transform_asof_date = order_asof,
    order_transform_available = order_available,
    signal_bar_available = signal_present,
    signal_bar_available_at = signal_available_at,
    execution_bar_available = execution_present,
    execution_bar_available_at = execution_available_at,
    stringsAsFactors = FALSE
  )
}

.brf_intraday_roll_schedule <- function(roll_schedule, date_range) {
  if (!nrow(roll_schedule)) {
    return(roll_schedule)
  }
  execution_date <- as.Date(roll_schedule$execution_date)
  effective_date <- as.Date(roll_schedule$effective_date)
  keep <- (execution_date >= date_range[[1L]] & execution_date <= date_range[[2L]]) |
    (effective_date >= date_range[[1L]] & effective_date <= date_range[[2L]])
  out <- roll_schedule[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}
