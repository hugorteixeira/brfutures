test_that("public BIT registry is pure, versioned, and causally contiguous", {
  specs <- brf_b3_bit_contract_specs()

  expect_s3_class(specs, "data.frame")
  expect_equal(nrow(specs), 2L)
  expect_identical(
    specs$schema_id,
    rep("brfutures_b3_bit_contract_specs_v1", 2L)
  )
  expect_identical(specs$schema_version, c(1L, 1L))
  expect_identical(specs$root, c("BIT", "BIT"))
  expect_identical(
    specs$contract_size_regime,
    c("legacy_0.1_btc", "current_0.01_btc")
  )
  expect_equal(
    specs$effective_from,
    as.Date(c("2024-04-17", "2025-06-16"))
  )
  expect_equal(
    specs$effective_to,
    as.Date(c("2025-06-15", NA))
  )
  expect_equal(specs$effective_to[[1L]] + 1, specs$effective_from[[2L]])
  expect_equal(specs$contract_size_btc, c(0.1, 0.01))
  expect_equal(specs$multiplier, specs$contract_size_btc)
  expect_equal(specs$tick_size_brl_per_btc, c(20, 20))
  expect_equal(specs$tick_value_brl, c(2, 0.2))
  expect_equal(specs$cash_scale_brl, c(0.001, 0.0001))
  expect_equal(
    specs$contract_size_btc[[1L]] / specs$contract_size_btc[[2L]],
    10
  )
  expect_equal(specs$position_conversion_ratio, c(1, 10))
  expect_equal(
    specs$administrative_conversion_ratio,
    c(10, 10)
  )
  expect_equal(
    specs$administrative_conversion_asof_date,
    rep(as.Date("2025-06-13"), 2L)
  )
  expect_equal(
    specs$administrative_conversion_effective_date,
    rep(as.Date("2025-06-16"), 2L)
  )
  expect_true(all(grepl("044/2024-PRE|013/2025-VPC", specs$source_reference)))
  expect_true(all(nzchar(specs$registry_source)))

  specs$contract_size_btc[[1L]] <- 999
  expect_equal(brf_b3_bit_contract_specs()$contract_size_btc, c(0.1, 0.01))
})

test_that("legacy BIT helpers derive unchanged economics from public specs", {
  specs <- brf_b3_bit_contract_specs()
  legacy <- specs[specs$contract_size_regime == "legacy_0.1_btc", ]
  current <- specs[specs$contract_size_regime == "current_0.01_btc", ]
  internal <- brfutures:::.brf_b3_bit_contract_specification()

  expect_equal(internal$legacy_contract_size_btc, legacy$contract_size_btc)
  expect_equal(internal$current_contract_size_btc, current$contract_size_btc)
  expect_equal(
    internal$tick_size_brl_per_btc,
    current$tick_size_brl_per_btc
  )
  expect_equal(
    internal$position_conversion_asof_date,
    current$administrative_conversion_asof_date
  )
  expect_equal(
    internal$position_conversion_effective_date,
    current$administrative_conversion_effective_date
  )
  expect_equal(
    internal$position_conversion_ratio,
    current$administrative_conversion_ratio
  )
  expect_identical(internal$pnl_formula_id, current$pnl_formula_id)
  expect_identical(
    internal$final_settlement_formula_id,
    current$final_settlement_formula_id
  )

  legacy_metadata <-
    brfutures:::.brf_b3_bit_contract_size_metadata(0.1)
  current_metadata <-
    brfutures:::.brf_b3_bit_contract_size_metadata(0.01)
  expect_identical(
    legacy_metadata$contract_size_regime,
    legacy$contract_size_regime
  )
  expect_identical(
    current_metadata$contract_size_regime,
    current$contract_size_regime
  )
  expect_equal(
    legacy_metadata$contract_size_effective_to,
    current$administrative_conversion_asof_date
  )
  expect_equal(
    current_metadata$contract_size_effective_from,
    current$administrative_conversion_effective_date
  )
  expect_equal(legacy_metadata$position_conversion_ratio, 10)
  expect_equal(current_metadata$position_conversion_ratio, 10)
})
