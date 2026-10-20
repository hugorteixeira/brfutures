# brfutures agent contract

## Role

`brfutures` owns Brazilian futures source facts: explicit B3 acquisition,
parsing and caches; official contract lifecycle and settlement metadata; and
deterministic assembly of dated and continuous futures data.

## Boundary

- May use `positionsizer` for pure, explicit financial formulas.
- Must not depend on gateways, `finrunner`, `finmaestro`, `finstrat`, bots,
  broker accounts, order state, execution readiness, reconciliation or
  scheduling.
- Fetch functions may perform their documented B3 reads. Parsers, contract
  resolution and calculations must not hide network access or invent missing
  source facts.
- Public names are real APIs, not compatibility aliases. Remove superseded
  download paths once their consumers have migrated.

Tests are offline and deterministic: mock downloads, use synthetic fixtures and
never require credentials, broker connections or trading services.

Annual BVBG assembly operates by column over the complete pending batch.
Preserve chronological replacement for multi-date reports, existing column
coercions and atomic write/pending-marker recovery. Root XML reuse requires
equality of normalized observations and provenance, not just a maximum date.
Verify internal gaps, corrections, interruption recovery and realistic full
update memory before changing these paths.
