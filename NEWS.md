# brfutures 0.0.5

- Fix incremental `update_brfut()` calls without an explicit `start` when
  legacy HTML reports already exist in the cache. Cached report dates now
  retain their `Date` class, including after filtering known no-data days.
  This prevents `/ not defined for "Date" objects` during resume planning.
- Add offline regressions covering initial acquisition, incremental resume,
  skipped source dates and repeated updates of an already current cache.
