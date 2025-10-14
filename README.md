# brfutures 🇧🇷

<p align="center">
  <a href="https://cran.r-project.org/">
    <img src="https://img.shields.io/badge/R-%3E%3D%204.1.0-276DC3?logo=r&logoColor=white" alt="R version badge" />
  </a>
  <a href="https://github.com/hugorteixeira/brfutures">
    <img src="https://img.shields.io/badge/status-stable-green?logo=github&logoColor=white" alt="Status badge" />
  </a>
  <a href="https://opensource.org/licenses/GPL-3.0">
    <img src="https://img.shields.io/badge/license-GPLv3-blue.svg" alt="License badge" />
  </a>
  <a href="https://www.r-project.org/">
    <img src="https://img.shields.io/badge/Made%20with-R-276DC3?logo=r&logoColor=white" alt="Made with R" />
  </a>
</p>

> 📈 Download, parse, and organize Brazilian futures bulletins from B3 (BM&F) into tidy data frames and ready-to-use time series.

---

## ✨ Highlights

- 🚦 **Robust downloaders** with automatic retries, Excel ➜ HTML fallback, and "no data" caching
- 🧭 **Format-aware parsing** via `parse_brf_report()` with clean OHLC columns
- 🗂️ **Cache-friendly storage** under `tools::R_user_dir("brfutures", which)`
- 🧱 **High-level pipelines** (`brf_build_contract_series()`, `brf_get_aggregate()`, `brf_get_series()`)
- 📡 **Quick discovery** of ticker roots using `brf_list_ticker_roots()`
- 🇧🇷 **Brazil-specific** with business day calendars and DI futures calculations
- ♻️ **Legacy naming support** that bridges Portuguese month tickers and root
  renames (e.g. `ICN` → `CCM`, `BOI` → `BGI`) for seamless histories

---

## 📚 Features

### 📊 Data Retrieval
- Download daily bulletins from B3 (BM&F) for any futures contract
- Automatic retry mechanism with fallback options
- Smart caching to avoid re-downloading data

### 🛠️ Parsing & Processing
- Parse both Excel and HTML bulletin formats
- Tidy data conversion with consistent column names
- Automatic data validation and error handling

### 📈 Time Series Tools
- Build xts time series from contract data
- Compute aggregate data sets
- Estimate contract maturities
- Handle missing data and gaps
- Roll front/next contracts with `brf_build_continuous_series()` using
  forward-ratio or Panama-style adjustments, including legacy tickers

### 💰 DI Futures Support
- Convert between DI rates and PU (Present Value)
- Calculate tick values and tick sizes
- Estimate settlement prices from OHLC data
- Handle different DI tick regimes (pre/post 2025-08-25)

---

## 📦 Installation

### From GitHub (latest development version)

```r
# Install remotes if you haven't already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install the package
remotes::install_github("hugorteixeira/brfutures")
```

---

## 🚀 Quick Start

```r
library(brfutures)

# 🔍 Discover B3 futures roots
roots <- brf_list_ticker_roots()
head(roots)

# 📥 Download a day of IND bulletins into the cache
path <- download_brf_report("2024-04-19", "IND")

# 📝 Parse the saved report
data <- parse_brf_report(path)

# 🔄 Build xts series, materialise an aggregate, and estimate maturities
series <- brf_build_contract_series("IND", add_agg = TRUE, estimate_maturity = TRUE)
aggregate <- brf_get_aggregate("IND", ohlc_locf = TRUE)

# 📊 Fetch a ready-to-plot OHLCV xts for a single contract
wdo_xts <- brf_get_series("WDOZ24", type = "ohlcv_locf")

# 🔁 Build a continuous CCM series, stitching legacy ICN data and Portuguese
#     month tickers via a Panama-style back adjustment
ccm_cont <- brf_build_continuous_series(
  "CCM",
  build_type = "panama",
  roll_type = days_before_roll(days_before_expiry = 20),
  include_older = TRUE
)

# 💰 DI futures example - convert rates to PU
di_result <- calculate_futures_di_notional(
  rates = 13.5,           # Annualized rate in percent
  maturity_date = "2025-01-03",  # Contract maturity
  basis_date = Sys.Date()  # Valuation date
)
print(di_result$pu)
```

---

The `build_type` argument of `brf_build_continuous_series()` lets you pick between
`"fwd_rt"` (keep recent prices fixed and scale history forward) and `"panama"`
(classic Panama back-adjustment). Pair that with a roll specification such as
`days_before_roll()` or `windsor_log_spread_roll()` to control how and when each
seam is calculated, including staggered rolls and diagnostic output.
Legacy tickers with Portuguese month abbreviations and historical root aliases
are merged automatically when `include_older = TRUE`, so you get uninterrupted
series across naming changes.

> 💡 For DI futures, the continuous output now carries the \`PU_*\` notional columns as well.
> If you prefer to align seams on notionals, pass `roll_type = days_before_roll(roll_control = list(price_column = "PU_c"))`.

---

## 🧰 Function Reference

### 📥 Data Download & Management
- `download_brf_report()` - Retry-friendly bulletin downloader (Excel ↔ HTML)
- `brf_bulletin_url()` - Build download URL for B3 futures bulletin
- `brf_download_history()` - Download a range of bulletins for a ticker root
- `brf_list_ticker_roots()` - Retrieve available ticker roots from B3 bulletin

### 📝 Parsing & Data Processing
- `parse_brf_report()` - Harmonised tibble per contract, regardless of source format
- `brf_collect_contracts()` - Collate multiple bulletins into one data frame
- `brf_build_contract_series()` - Build per-contract `xts` series + maturity estimates
- `brf_build_continuous_series()` - Produce front-adjusted back-adjusted chains with modern and legacy tickers
- `days_before_roll()` / `windsor_log_spread_roll()` / `regression_roll()` / `custom_roll()` - Declare roll specifications for continuous chains
- `brf_get_aggregate()` - Load or split the cached aggregate data set
- `brf_get_series()` - One contract → OHLC/LOCF xts in your timezone

### 💰 DI Futures Calculations
- `calculate_futures_di_notional()` - DI notional (PU) from annualized rates
- `calculate_futures_di_rates()` - Inverse: DI rates from notional (PU)
- `di_maturity_from_ticker()` - Convert DI codes (e.g. `DI1F25`) to business-day maturities
- `di_ohlc_to_pu_augmented_xts()` - Convert DI OHLC rates to PU columns (xts)

---

## 📂 Caching System

### Default Location
- `tools::R_user_dir("brfutures", "cache")`

### Custom Configuration
```r
# Override globally
options(brfutures.cachedir = "~/fin-data/br-cache")

# Pass explicit directory to functions
download_brf_report("2024-04-19", "IND", dest_dir = "/path/to/cache")
```

### Caching Features
- "No data" bulletins are memoized so later runs skip them automatically
- Files are organized by ticker root and date
- Efficient storage with compression
- Automatic cleanup of invalid files

---

## 🇧🇷 Brazilian Financial Market Support

This package is specifically designed for the Brazilian financial market with:

- **Business day adjustment** following B3 conventions
- **DI futures calculations** according to B3 methodology
- **Month code support** for all Brazilian futures contracts
- **Real-time bulletin parsing** from B3 official sources
- **Local timezone handling** (America/Sao_Paulo)

### Supported Contract Types
- Index futures (IND, WIN)
- Currency futures (DOL, WDO)
- DI futures (DI1)
- Commodity futures (various types)
- And more [discoverable with `brf_list_ticker_roots()`]

---

## 💡 Pro Tips

### 🚀 Performance Optimization
```r
# Download historical data in bulk
brf_download_history("WDO", "2024-01-01", "2024-03-31")

# Requesting CCM automatically fetches legacy ICN bulletins when needed
brf_download_history("CCM", "2004-01-01", "2004-12-31")

# Same goes for soybean cattle futures: BGI pulls legacy BOI bulletins too
brf_download_history("BGI", "1999-01-01", "1999-12-31")

# Build all series at once with caching
brf_build_contract_series("IND", add_agg = TRUE, save_series = TRUE)
```

### 📊 Data Quality
```r
# Use LOCF (Last Observation Carried Forward) for gaps
brf_get_series("WDOZ24", type = "ohlcv_locf")

# Check for missing data
aggregate <- brf_get_aggregate("DI1", ohlc_locf = TRUE)
```

### 🔧 Advanced Options
```r
# Specify custom timezone
brf_get_series("WDOZ24", tz = "UTC")

# Get raw aggregate data without processing
brf_get_aggregate("DI1", return = "agg")
```

---

## 🤝 Contributing

Bug reports, feature ideas, and pull requests are always welcome. Help us expand to new ticker roots, refine DI tooling, or improve docs.

### Development Setup
```r
# Install development dependencies
remotes::install_dev_deps()

# Build documentation
devtools::document()
```

### Code of Conduct
Please note that the brfutures project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/). By contributing to this project, you agree to abide by its terms.

---

## 📄 License

GPL (≥ 3) – share, remix, and improve responsibly.

---

## 👨‍💻 About the Author

Hi, I’m Hugo. I build tools around trading and backtesting in R to streamline workflow and help iterate on strategies faster. If you find brfutures useful (or frustrating!), feedback is welcome.

---

## 🙏 Acknowledgments

- Data provided by B3 (Brasil, Bolsa, Balcão)
- Built with R and the greater open-source ecosystem
- Special thanks to the Brazilian financial market community

---

<p align="center">
  Made with ❤️ for the Brazilian financial market
</p>
