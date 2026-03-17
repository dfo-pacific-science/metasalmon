# ADR-0002: Separate one-shot SDP creation from advanced/manual package writing

## Status

Accepted

## Context

`metasalmon` had multiple overlapping package-creation entrypoints and a flatter output layout that mixed review files, canonical metadata CSVs, and data tables at the package root. That made the API harder to explain and the resulting folders noisier to review.

## Decision

- Keep `create_sdp()` as the single start-here, one-shot entrypoint from raw tables.
- Rename the advanced/manual writer to `write_salmon_datapackage()` and remove the older overlapping aliases.
- Write canonical metadata CSVs under `metadata/` and table data under `data/`.
- Keep `README-review.txt`, `semantic_suggestions.csv` (when present), and derived `datapackage.json` at the package root.
- Continue reading legacy root-level metadata CSVs for backwards compatibility.

## Consequences

### Positive

- Clearer public API: one obvious beginner path and one explicit advanced/manual path.
- Cleaner package layout that separates canonical metadata from data files and review artifacts.
- Easier pkgdown/reference wording because the entrypoints now map cleanly to user intent.

### Negative

- Existing code that called the removed aliases must update to the new names.
- Existing examples and tests needed coordinated updates.

### Neutral

- `read_salmon_datapackage()` now carries a small compatibility branch for legacy root-level metadata layouts.
- `datapackage.json` remains a derived export rather than the canonical metadata source.

## More Information

This change intentionally favors a clean pre-1.0 surface over temporary compatibility wrappers because the package does not yet have a large installed base.

## Related

- [docs/entrypoints.md](../entrypoints.md) - Canonical entrypoints and wiring notes
