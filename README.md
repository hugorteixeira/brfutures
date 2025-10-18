# brfutures

Lightweight helpers to keep a local cache of B3 (BM&F) futures bulletins using
the public HTML endpoint only. The focus is fast incremental updates and simple
query helpers for the cached data.

## Installation

```r
# development version
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("hugorteixeira/brfutures")
```

## Usage

1. Pick a writable cache directory and tell the package where it is:

```r
options(brfutures.cache_dir = "~/data/brfutures")
```

2. Seed the cache for one or more roots. The first run will download and parse
   all requested dates; subsequent calls only fetch the missing sessions.

```r
library(brfutures)

update_brfut(
  root = "WIN",
  start = as.Date("2024-01-01"),
  end = Sys.Date()
)
```

3. Retrieve data straight from the cache:

```r
# Individual contract(s)
get_brfut("WINM24")

# Full data for every cached root between two dates
get_brfut_agg(start = "2024-03-01", end = "2024-04-01")
```

### What gets cached?

```
<cache_dir>/
  ├─ WIN/
  │   ├─ raw/                # downloaded HTML bulletins
  │   └─ data.rds            # parsed rows for the root
  └─ aggregate.rds           # quick access to every cached row
```

- `update_brfut()` never downloads Excel files and never touches the network
  when the cache already contains the requested sessions.
- If `root` is omitted, the function updates every root that already has a
  folder inside the cache directory.
- Passing `start = NULL` resumes from the first day not yet cached for each
  root and defaults `end` to `Sys.Date()`.

### Retrieving data

`get_brfut()` and `get_brfut_agg()` always read from the aggregate RDS file.
They keep the original bulletin columns, and `get_brfut()` lets you transform
the data on the fly via the `treatment` argument (e.g. raw data frames,
renamed tibbles, or ready-to-use OHLCV `xts`).

## Testing

The package ships with a small test suite based on synthetic HTML fixtures:

```r
pkgload::load_all()
testthat::test_dir("tests/testthat")
```

## License

GPL-3.
