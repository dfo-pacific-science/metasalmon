# ADR-0004: Prefer post-review EDH XML regeneration from the finalized package

## Status

Accepted

## Context

`create_sdp(include_edh_xml = TRUE)` can emit `metadata/metadata-edh-hnap.xml` during the first package write, but that happens before the human review loop is finished. In practice, the authoritative metadata lives in the edited package CSVs after review, not in the initial `create_sdp()` inputs.

## Decision

Prefer a **post-review EDH XML regeneration step** that reads the finalized Salmon Data Package and rebuilds the EDH XML from the edited package metadata, rather than widening `create_sdp()` intake or adding more pre-review EDH-specific knobs there.

## Consequences

### Positive

- Keeps `create_sdp()` focused on package creation + review scaffolding.
- Makes the generated EDH XML reflect the metadata the user actually finalized.
- Avoids coupling more EDH workflow complexity to the start-here path.

### Negative

- Users who want XML refreshed after edits need an explicit regeneration step.

### Neutral

- The existing `include_edh_xml = TRUE` path can remain as a convenience starter export, but it should not be treated as the final authoritative EDH artifact.

## Related

- [ADR-0002](0002-start-here-vs-advanced-writer.md)
- [ADR-0003](0003-llm-semantic-assessment-via-openai-compatible-reranker.md)
