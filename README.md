# brfutures

Tools to download, parse, and organise Brazilian futures bulletins from B3 (formerly BM&F) into tidy data frames and ready-to-use time series.

## Features

- Resilient downloaders with automatic retry and HTML fallback when Excel exports are unavailable.
- Format-agnostic parsers (`parse_bmf_report()`) that harmonise column names (`open`, `high`, `low`, `close`, `volume`, etc.) across ticker roots and keep track of per-contract tickers.
- Cached storage in the user directory via `tools::R_user_dir("brfutures", which)` so local artifacts never ship with the package.
- High-level helpers to collect contract data (`bmf_collect_contracts()`), build xts series (`bmf_build_contract_series()`), download historical ranges (`bmf_download_history()`), and fetch the aggregated datasets (`bmf_get_aggregate()` with `add_agg = TRUE`).
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
download_bmf_report("2024-04-19", "IND")

# Parse a saved report
data <- parse_bmf_report(bmf_storage_dir("IND"))

# Build xts series and materialise an aggregate
series <- bmf_build_contract_series("IND", add_agg = TRUE)
aggregate <- bmf_get_aggregate("IND")
```

## Caching strategy

All helpers default to `which = "cache"` so downloads and derived artifacts live under the user's cache directory (`tools::R_user_dir("brfutures", "cache")`). Override the `which` parameter or pass explicit directories if you prefer a custom layout.

## Contributing

Issues and pull requests are welcome! Feel free to open a ticket or submit enhancements that broaden coverage to other ticker roots or derived datasets.

## License

GPL (>=3)
