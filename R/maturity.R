.brf_futures_month_map <- c(
  F = 1L, G = 2L, H = 3L, J = 4L, K = 5L, M = 6L,
  N = 7L, Q = 8L, U = 9L, V = 10L, X = 11L, Z = 12L
)

.brf_maturity_rule <- function(root) {
  root <- toupper(trimws(as.character(root)[1L]))
  if (!length(root) || is.na(root) || !nzchar(root)) {
    return(NA_character_)
  }
  if (root == "CCM") return("day_15_following_session")
  if (root == "BGI") return("last_session")
  if (root %in% c("BIT", "SOL", "ETR")) {
    return("last_friday_preceding_session")
  }
  if (root == "XFI") return("third_friday_preceding_session")
  if (root %in% c("IND", "WIN")) {
    return("closest_wednesday_following_session")
  }
  if (root %in% c("WDO", "DOL", "DI1")) return("first_session")
  if (root == "ICF") return("sixth_session_before_last")
  NA_character_
}

.brf_last_trade_rule <- function(root) {
  root <- toupper(trimws(as.character(root)[1L]))
  if (!length(root) || is.na(root) || !nzchar(root)) {
    return(NA_character_)
  }
  if (root %in% c("WDO", "DOL", "DI1")) {
    return("previous_session_before_expiry")
  }
  if (root %in% c(
    "WIN", "IND", "CCM", "BGI", "BIT", "XFI", "SOL", "ETR", "ICF"
  )) {
    return("expiry_session")
  }
  NA_character_
}

.brf_month_bounds <- function(year, month) {
  year <- suppressWarnings(as.integer(year))
  month <- suppressWarnings(as.integer(month))
  if (!is.finite(year) || !is.finite(month) || month < 1L || month > 12L) {
    return(NULL)
  }
  start <- as.Date(sprintf("%04d-%02d-01", year, month))
  next_year <- if (month == 12L) year + 1L else year
  next_month <- if (month == 12L) 1L else month + 1L
  end <- as.Date(sprintf("%04d-%02d-01", next_year, next_month)) - 1L
  list(start = start, end = end)
}

.brf_last_weekday_in_month <- function(month_end, weekday) {
  weekday_end <- as.POSIXlt(as.Date(month_end))$wday
  as.Date(month_end) - ((weekday_end - weekday + 7L) %% 7L)
}

.brf_nth_weekday_in_month <- function(month_start, weekday, occurrence) {
  weekday_start <- as.POSIXlt(as.Date(month_start))$wday
  days_to_weekday <- (weekday - weekday_start + 7L) %% 7L
  as.Date(month_start) + days_to_weekday + 7L * (occurrence - 1L)
}

.brf_closest_weekday <- function(date, weekday) {
  date <- as.Date(date)
  current <- as.POSIXlt(date)$wday
  days_back <- (current - weekday + 7L) %% 7L
  days_forward <- (weekday - current + 7L) %% 7L
  if (days_back <= days_forward) date - days_back else date + days_forward
}

.brf_maturity_date <- function(root, year, month, cal = NULL) {
  rule <- .brf_maturity_rule(root)
  bounds <- .brf_month_bounds(year, month)
  if (is.na(rule) || is.null(bounds)) return(as.Date(NA))

  calendar <- .brf_di_resolve_session_calendar(cal)
  result <- switch(
    rule,
    day_15_following_session = bizdays::following(
      as.Date(sprintf("%04d-%02d-15", as.integer(year), as.integer(month))),
      calendar
    ),
    last_session = bizdays::preceding(bounds$end, calendar),
    last_friday_preceding_session = bizdays::preceding(
      .brf_last_weekday_in_month(bounds$end, weekday = 5L),
      calendar
    ),
    third_friday_preceding_session = bizdays::preceding(
      .brf_nth_weekday_in_month(bounds$start, weekday = 5L, occurrence = 3L),
      calendar
    ),
    closest_wednesday_following_session = bizdays::following(
      .brf_closest_weekday(bounds$start + 14L, weekday = 3L),
      calendar
    ),
    first_session = bizdays::following(bounds$start, calendar),
    sixth_session_before_last = bizdays::offset(
      bizdays::preceding(bounds$end, calendar),
      -6L,
      calendar
    ),
    as.Date(NA)
  )
  result <- as.Date(result)
  if (is.na(result) || as.integer(format(result, "%m")) != as.integer(month)) {
    return(as.Date(NA))
  }
  result
}

.brf_last_trade_date <- function(root, maturity, cal = NULL) {
  maturity <- as.Date(maturity)
  if (length(maturity) != 1L || is.na(maturity)) return(as.Date(NA))
  rule <- .brf_last_trade_rule(root)
  if (is.na(rule)) return(as.Date(NA))
  if (identical(rule, "previous_session_before_expiry")) {
    return(as.Date(bizdays::offset(
      maturity,
      -1L,
      .brf_di_resolve_session_calendar(cal)
    )))
  }
  maturity
}

.brf_maturity_date_column <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1970-01-01"))
  suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
}

.brf_estimate_maturity <- function(df, cal = NULL) {
  if (!is.data.frame(df) || !nrow(df) || !"ticker" %in% names(df)) {
    return(df)
  }

  tickers <- toupper(trimws(as.character(df$ticker)))
  obs_dates <- if ("date" %in% names(df)) {
    .brf_maturity_date_column(df$date)
  } else {
    rep(as.Date(NA), length(tickers))
  }
  reference_dates <- obs_dates
  reference_dates[is.na(reference_dates)] <- Sys.Date()
  group_key <- paste(tickers, reference_dates, sep = "\r")
  representative <- which(!duplicated(group_key))
  representative_key <- group_key[representative]
  resolved <- brf_contract_resolve(
    tickers = tickers[representative],
    reference_date = reference_dates[representative],
    strict = FALSE,
    cal = cal
  )
  matched_group <- match(group_key, representative_key)
  resolved_maturity <- resolved$maturity_date[matched_group]
  resolved_last_trade <- resolved$last_trade_date[matched_group]
  if ("maturity" %in% names(df)) {
    maturity <- .brf_maturity_date_column(df$maturity)
    missing <- is.na(maturity)
    maturity[missing] <- resolved_maturity[missing]
  } else {
    maturity <- resolved_maturity
  }
  df$maturity <- as.Date(maturity)

  if ("last_trade_date" %in% names(df)) {
    last_trade <- .brf_maturity_date_column(df$last_trade_date)
    missing <- is.na(last_trade)
    last_trade[missing] <- resolved_last_trade[missing]
  } else {
    last_trade <- resolved_last_trade
  }
  df$last_trade_date <- as.Date(last_trade)
  df
}
