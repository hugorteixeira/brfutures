# brfutures

Tools to download, parse, and organise Brazilian futures bulletins from B3 (formerly BM&F) into tidy data frames and ready-to-use time series.

## Features

- Resilient downloaders with automatic retry, HTML fallback when Excel exports are unavailable, and automatic pruning/blacklisting of "no data" bulletins.
- Format-agnostic parsers (`parse_bmf_report()`) that harmonise column names (`open`, `high`, `low`, `close`, `volume`, etc.) across ticker roots and keep track of per-contract tickers.
- Cached storage in the user directory via `tools::R_user_dir("brfutures", which)` so local artifacts never ship with the package.
- High-level helpers to collect contract data (`bmf_collect_contracts()`), build xts series (`bmf_build_contract_series()`), download historical ranges (`bmf_download_history()`), and fetch aggregated datasets (`bmf_get_aggregate()`). Optional maturity estimation tags each contract with an inferred expiry date.
- Live ticker discovery with `bmf_list_ticker_roots()` to explore available bulletin roots.

## Installation

```r
# install.packages("remotes")
remotes::install_github("hugorteixeira/brfutures")
```

## Quick start

```r
library(brfutures)

# Discover futures roots
roots <- bmf_list_ticker_roots()
head(roots)

# Download a day of IND bulletins into the cache
path <- download_bmf_report("2024-04-19", "IND")

# Parse the saved report
data <- parse_bmf_report(path)

# Build xts series, materialise an aggregate, and estimate maturities
series <- bmf_build_contract_series("IND", add_agg = TRUE, estimate_maturity = TRUE)
aggregate <- bmf_get_aggregate("IND", ohlc_locf = TRUE)
```

## Caching strategy

All helpers default to `which = "cache"` so downloads and derived artifacts live under the user's cache directory (`tools::R_user_dir("brfutures", "cache")`). To point the cache somewhere else, set an option at startup:

```r
options(brfutures.cachedir = "~/senhormercado/Dados/Cache")
```

The directory is created on demand. You can still override the location per call by passing explicit paths or changing `which`.

## Contributing

Issues and pull requests are welcome! Feel free to open a ticket or submit enhancements that broaden coverage to other ticker roots or derived datasets.

## License

GPL (>=3)
