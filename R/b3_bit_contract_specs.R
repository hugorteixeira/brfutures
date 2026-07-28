#' Versioned B3 Bitcoin futures contract specifications
#'
#' Returns the authoritative, side-effect-free BIT contract registry used by
#' brfutures. The two rows preserve the original `0.1` BTC contract and the
#' `0.01` BTC contract effective from 2025-06-16. They also distinguish the
#' specification-validity interval from the administrative position snapshot
#' on 2025-06-13, when open quantities were multiplied by ten without a trade,
#' cost, or cash flow.
#'
#' Contract economics and official-source references live here. Trading-cost
#' assumptions deliberately do not: downstream portfolio/data packages retain
#' ownership of their configured fee and slippage models.
#'
#' @return A data frame with one row per BIT contract-size regime.
#' @export
brf_b3_bit_contract_specs <- function() {
  contract_size_btc <- c(0.1, 0.01)
  tick_size_brl_per_btc <- rep(20, 2L)
  price_scale_brl <- rep(0.01, 2L)
  administrative_conversion_ratio <- 10
  administrative_conversion_asof_date <- as.Date("2025-06-13")
  administrative_conversion_effective_date <- as.Date("2025-06-16")
  administrative_conversion_source <- "B3 Circular Letter 013/2025-VPC"

  out <- data.frame(
    schema_id = rep("brfutures_b3_bit_contract_specs_v1", 2L),
    schema_version = rep(1L, 2L),
    root = rep("BIT", 2L),
    contract_size_regime = c(
      "legacy_0.1_btc",
      "current_0.01_btc"
    ),
    effective_from = as.Date(c("2024-04-17", "2025-06-16")),
    effective_to = as.Date(c("2025-06-15", NA_character_)),
    contract_size_btc = contract_size_btc,
    multiplier = contract_size_btc,
    tick_size_brl_per_btc = tick_size_brl_per_btc,
    tick_value_brl = tick_size_brl_per_btc * contract_size_btc,
    quote_currency = rep("BRL", 2L),
    settlement_currency = rep("BRL", 2L),
    unit_of_measure = rep("BTC", 2L),
    price_scale_brl = price_scale_brl,
    cash_scale_brl = price_scale_brl * contract_size_btc,
    pnl_formula_id = rep("linear_brl", 2L),
    auxiliary_series = rep(
      "nqbtcs_settlement_usd+b3_brl_usd_d1",
      2L
    ),
    rounding_rule = rep("no_intermediate_rounding", 2L),
    position_conversion_ratio = c(1, administrative_conversion_ratio),
    position_conversion_date = as.Date(c(
      NA_character_,
      format(administrative_conversion_effective_date)
    )),
    position_conversion_asof_date = as.Date(c(
      NA_character_,
      format(administrative_conversion_asof_date)
    )),
    position_conversion_reason = c(
      NA_character_,
      "b3_bit_contract_size_reduction_0.1_to_0.01_btc"
    ),
    position_conversion_source = c(
      NA_character_,
      administrative_conversion_source
    ),
    administrative_conversion_ratio = rep(
      administrative_conversion_ratio,
      2L
    ),
    administrative_conversion_asof_date = rep(
      administrative_conversion_asof_date,
      2L
    ),
    administrative_conversion_effective_date = rep(
      administrative_conversion_effective_date,
      2L
    ),
    administrative_position_transform = rep(
      "open_quantity_multiply_10",
      2L
    ),
    administrative_conversion_source = rep(
      administrative_conversion_source,
      2L
    ),
    specification_source = c(
      "B3 Circular Letter 044/2024-PRE",
      administrative_conversion_source
    ),
    source_reference = c(
      paste(
        "B3 Bitcoin Futures original contract specification, terminal",
        "NQBTCS x B3 BRL/USD D+1 settlement formula, and Circular",
        "Letter 044/2024-PRE"
      ),
      paste(
        "B3 Bitcoin Futures contract specification, terminal NQBTCS x",
        "B3 BRL/USD D+1 settlement formula, and Circular Letter",
        "013/2025-VPC"
      )
    ),
    final_settlement_formula_id = rep(
      "b3_bit_final_settlement_nqbtcs_fx_v2",
      2L
    ),
    final_settlement_index = rep("NQBTCS", 2L),
    final_settlement_source_indicator = rep("BTCLIQUSD", 2L),
    final_settlement_fx = rep(
      "B3 BRL per USD rate for settlement in one business day",
      2L
    ),
    final_settlement_fx_source_indicator = rep("RTDOL-D1", 2L),
    final_settlement_direct_brl_source_indicator = rep("RTBITLIQ", 2L),
    final_settlement_rounding_rule = rep(
      "official_adjstdqt_or_half_up_2dp",
      2L
    ),
    final_settlement_cash_lag_business_days = rep(1L, 2L),
    registry_source = rep(
      "brfutures authoritative versioned B3 BIT contract specs",
      2L
    ),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

.brf_b3_bit_contract_specification <- function() {
  specs <- brf_b3_bit_contract_specs()
  legacy <- specs[
    specs$contract_size_regime == "legacy_0.1_btc",
    ,
    drop = FALSE
  ]
  current <- specs[
    specs$contract_size_regime == "current_0.01_btc",
    ,
    drop = FALSE
  ]
  if (nrow(legacy) != 1L || nrow(current) != 1L) {
    stop("The authoritative BIT contract registry is ambiguous.", call. = FALSE)
  }
  list(
    legacy_contract_size_btc = legacy$contract_size_btc[[1L]],
    current_contract_size_btc = current$contract_size_btc[[1L]],
    tick_size_brl_per_btc = current$tick_size_brl_per_btc[[1L]],
    position_conversion_asof_date =
      current$administrative_conversion_asof_date[[1L]],
    position_conversion_effective_date =
      current$administrative_conversion_effective_date[[1L]],
    position_conversion_ratio =
      current$administrative_conversion_ratio[[1L]],
    administrative_position_transform =
      current$administrative_position_transform[[1L]],
    source = current$administrative_conversion_source[[1L]],
    registry_source = current$registry_source[[1L]],
    pnl_formula_id = current$pnl_formula_id[[1L]],
    final_settlement_formula_id =
      current$final_settlement_formula_id[[1L]],
    final_settlement_index = current$final_settlement_index[[1L]],
    final_settlement_source_indicator =
      current$final_settlement_source_indicator[[1L]],
    final_settlement_fx = current$final_settlement_fx[[1L]],
    final_settlement_fx_source_indicator =
      current$final_settlement_fx_source_indicator[[1L]],
    final_settlement_direct_brl_source_indicator =
      current$final_settlement_direct_brl_source_indicator[[1L]],
    final_settlement_rounding_rule =
      current$final_settlement_rounding_rule[[1L]],
    final_settlement_cash_lag_business_days =
      current$final_settlement_cash_lag_business_days[[1L]]
  )
}

.brf_b3_bit_contract_size_metadata <- function(contract_size_btc) {
  specs <- brf_b3_bit_contract_specs()
  position <- which(
    is.finite(contract_size_btc) &
      abs(specs$contract_size_btc - contract_size_btc) <=
        1e-12 * pmax(1, abs(specs$contract_size_btc), abs(contract_size_btc))
  )
  matched <- length(position) == 1L
  regime <- if (matched) {
    specs$contract_size_regime[[position]]
  } else {
    "source_observed_other"
  }
  conversion <- specs[
    specs$contract_size_regime == "current_0.01_btc",
    ,
    drop = FALSE
  ]
  if (nrow(conversion) != 1L) {
    stop("The authoritative BIT conversion registry is ambiguous.", call. = FALSE)
  }
  list(
    contract_size_regime = regime,
    contract_size_effective_from = if (
      identical(regime, "current_0.01_btc")
    ) {
      conversion$administrative_conversion_effective_date[[1L]]
    } else {
      as.Date(NA)
    },
    contract_size_effective_to = if (
      identical(regime, "legacy_0.1_btc")
    ) {
      conversion$administrative_conversion_asof_date[[1L]]
    } else {
      as.Date(NA)
    },
    position_conversion_asof_date =
      conversion$administrative_conversion_asof_date[[1L]],
    position_conversion_effective_date =
      conversion$administrative_conversion_effective_date[[1L]],
    position_conversion_ratio =
      conversion$administrative_conversion_ratio[[1L]],
    administrative_position_transform =
      conversion$administrative_position_transform[[1L]],
    specification_source =
      conversion$administrative_conversion_source[[1L]]
  )
}
