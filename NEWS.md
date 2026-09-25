# brfutures 0.0.7

- Exclude the officially identified SOLN39 BDR from futures price results and
  existing root/aggregate caches. The check uses both its ticker and B3
  instrument id; unknown identities and a future with the same ticker and a
  different id remain eligible. Raw B3 reports stay intact, and ordinary
  updates publish the corrected root and aggregate rows.
- Add offline regressions for fresh and cached PR reads, read-only cache
  filtering and persistent repair through `update_brfut()`.

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
