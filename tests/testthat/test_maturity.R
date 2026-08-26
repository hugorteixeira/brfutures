test_that("historical maturity rules use B3 trading sessions", {
  cases <- data.frame(
    ticker = c(
      "BGIG17", "BGIZ12", "BGIZ17", "BGIZ18", "CCMK10", "DI1K12",
      "WDOF17", "DOLF19", "WINJ17", "INDJ17", "ICFZ14", "ICFZ17",
      "ICFZ20", "XFIJ23", "BITJ24", "SOLM25", "ETRM25"
    ),
    root = c(
      "BGI", "BGI", "BGI", "BGI", "CCM", "DI1", "WDO", "DOL",
      "WIN", "IND", "ICF", "ICF", "ICF", "XFI", "BIT", "SOL", "ETR"
    ),
    maturity = as.Date(c(
      "2017-02-24", "2012-12-28", "2017-12-28", "2018-12-28",
      "2010-05-17", "2012-05-02", "2017-01-02", "2019-01-02",
      "2017-04-12", "2017-04-12", "2014-12-18", "2017-12-19",
      "2020-12-18", "2023-04-20", "2024-04-26", "2025-06-27",
      "2025-06-27"
    )),
    last_trade_date = as.Date(c(
      "2017-02-24", "2012-12-28", "2017-12-28", "2018-12-28",
      "2010-05-17", "2012-04-30", "2016-12-29", "2018-12-28",
      "2017-04-12", "2017-04-12", "2014-12-18", "2017-12-19",
      "2020-12-18", "2023-04-20", "2024-04-26", "2025-06-27",
      "2025-06-27"
    )),
    stringsAsFactors = FALSE
  )
  input <- cases[c("ticker", "root")]
  input$date <- cases$maturity - 30L

  estimated <- brfutures:::`.brf_estimate_maturity`(input)

  expect_equal(estimated$maturity, cases$maturity)
  expect_equal(estimated$last_trade_date, cases$last_trade_date)
})

test_that("year-end historical contracts stop on the actual B3 session", {
  cases <- data.frame(
    ticker = c(
      paste0("BGIZ", 10:17),
      paste0("ICFZ", 10:17)
    ),
    root = rep(c("BGI", "ICF"), each = 8L),
    maturity = as.Date(c(
      "2010-12-30", "2011-12-29", "2012-12-28", "2013-12-30",
      "2014-12-30", "2015-12-30", "2016-12-29", "2017-12-28",
      "2010-12-21", "2011-12-21", "2012-12-18", "2013-12-18",
      "2014-12-18", "2015-12-18", "2016-12-21", "2017-12-19"
    )),
    stringsAsFactors = FALSE
  )
  input <- cases[c("ticker", "root")]
  input$date <- cases$maturity - 30L

  estimated <- brfutures:::`.brf_estimate_maturity`(input)

  expect_equal(estimated$maturity, cases$maturity)
  expect_equal(estimated$last_trade_date, cases$maturity)
})

test_that("published contract dates are never overwritten by estimates", {
  input <- data.frame(
    ticker = c("BITZ25", "DI1F19"),
    root = c("BIT", "DI1"),
    date = as.Date(c("2025-10-30", "2018-12-01")),
    maturity = as.Date(c("2025-12-23", NA)),
    last_trade_date = as.Date(c("2025-12-23", NA)),
    stringsAsFactors = FALSE
  )

  estimated <- brfutures:::`.brf_estimate_maturity`(input)

  expect_equal(
    estimated$maturity,
    as.Date(c("2025-12-23", "2019-01-02"))
  )
  expect_equal(
    estimated$last_trade_date,
    as.Date(c("2025-12-23", "2018-12-28"))
  )
})

test_that("unsupported futures roots fail closed", {
  input <- data.frame(
    ticker = "ZZZF17",
    root = "ZZZ",
    date = as.Date("2016-12-01"),
    stringsAsFactors = FALSE
  )

  estimated <- brfutures:::`.brf_estimate_maturity`(input)

  expect_true(is.na(estimated$maturity))
  expect_true(is.na(estimated$last_trade_date))
})

test_that("post-2018 enrichment never falls back to an estimated date", {
  input <- data.frame(
    ticker = "CCMU34",
    root = "CCM",
    date = as.Date("2026-08-26"),
    stringsAsFactors = FALSE
  )

  enriched <- brfutures:::`.brf_estimate_maturity`(input)

  expect_true(is.na(enriched$maturity))
  expect_true(is.na(enriched$last_trade_date))
})
