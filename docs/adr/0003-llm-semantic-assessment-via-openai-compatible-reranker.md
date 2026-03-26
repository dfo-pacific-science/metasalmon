# ADR-0003: LLM semantic assessment via an OpenAI-compatible reranker with local context files

## Status

Accepted

## Context

`metasalmon` already has a deterministic semantic retrieval path: `find_terms()` produces candidate IRIs and `suggest_semantics()` packages them into reviewable suggestions. The new requirement is to let users optionally ask an LLM to judge those candidates more intelligently, using local context files such as README files or PDF reports, without replacing the deterministic search system.

The key architectural choice is whether the LLM becomes the primary term finder or a secondary assessor over retrieved candidates.

## Decision

`metasalmon` will implement LLM support as an **opt-in reranking / assessment layer** over the existing candidate-retrieval pipeline.

Specifically:

- `find_terms()` remains the canonical candidate generator.
- `suggest_semantics()` may optionally call an LLM to assess the top-N candidates for each target.
- The transport will use an **OpenAI-compatible HTTP client** so the same code path can support OpenAI, OpenRouter, and custom compatible endpoints.
- Local context files will be handled via lightweight local text extraction + chunk selection before prompt assembly.
- Applying the chosen IRI remains explicit via `apply_semantic_suggestions()`; the package will not silently auto-apply LLM choices by default.

## Consequences

### Positive

- Keeps one clear active path: deterministic retrieval first, optional model judgment second.
- Avoids asking the model to invent raw IRIs from scratch.
- Supports OpenRouter and its `:free` model ecosystem without provider-specific SDK sprawl.
- Lets users include domain context from reports/readmes while keeping prompts bounded.
- Preserves explicit review/apply ergonomics already used by the package.

### Negative

- Function signatures become broader and slightly more complex.
- PDF support adds an optional dependency surface (`pdftools`).
- Free or weak models may produce unstable review quality, so users still need review thresholds and judgment.

### Neutral

- Introduces a new helper module for LLM/config/context plumbing.
- Extends tests with mocked provider responses rather than live network dependence.
- Requires docs to shift from GPT-only language to package-native LLM workflows.

## More Information

Alternatives considered:

1. **LLM as primary term finder**
   Rejected because it increases hallucination risk, makes results harder to explain, and duplicates `find_terms()`.

2. **Provider-specific SDKs**
   Rejected because they create unnecessary surface area when OpenAI-compatible HTTP already covers the needed providers.

3. **Whole-document prompt stuffing**
   Rejected because it is slow, expensive, and usually makes judgments worse; local chunk selection is the cleaner default.

## Related

- [ADR-0002](0002-start-here-vs-advanced-writer.md) - Preserves the repo pattern of explicit start-here vs advanced/manual paths
- [ExecPlan: LLM semantic assessment + local context files + OpenRouter support](../plans/2026-03-26-llm-semantic-assessment.md)
