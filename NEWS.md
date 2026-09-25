# brfutures 0.0.6

- Preserve DI rows whose maturity or observation date is unresolved when
  augmenting rate prices with PU and tick columns. Derived values remain
  missing instead of aborting the full update or inventing a maturity.
- Add an offline regression covering mixed known and missing dates across
  the DI tick-rule change, including unchanged valid-row calculations.

# brfutures 0.0.5

- Fix incremental `update_brfut()` calls without an explicit `start` when
  legacy HTML reports already exist in the cache. Cached report dates now
  retain their `Date` class, including after filtering known no-data days.
  This prevents `/ not defined for "Date" objects` during resume planning.
- Add offline regressions covering initial acquisition, incremental resume,
  skipped source dates and repeated updates of an already current cache.
