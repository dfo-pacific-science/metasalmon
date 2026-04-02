# DRAFT ExecPlan — route measurement / compound-variable targets through I-ADOPT chat decomposition

This ExecPlan is a living document. Keep `Progress`, `Decision Log`, and `Validation + Acceptance` current as the work moves.

## Purpose / Big Picture

metasalmon already builds role-aware semantic search targets, but LLM shortlist review still uses one generic assessment prompt for everything. That is good enough for broad term picking, but it is too blunt for **measurement variables** and other **compound-variable-like targets**, where the real question is usually “what observable is this made of?” rather than “which generic label looks closest?”.

The goal of this change is to route those targets through an I-ADOPT-aware decomposition pass, with the chat function named `chat_decomposition()`. That should improve variable-level term selection for `term_iri` on measurement rows, reduce false-positive broad variable matches, and make the review path more consistent with the package’s existing I-ADOPT framing.

## Scope

In scope:
- measurement-row variable targets that populate `term_iri`
- other targets that clearly behave like compound variables, even if the role signal is imperfect
- LLM prompt / request routing and the tests that prove the routing works

Out of scope for this draft:
- changing the deterministic search index itself
- changing ontology sources or ranking weights
- rewriting vignettes, pkgdown pages, or publication docs in the same slice
- broad package UX wording outside the affected LLM path

## Why this lives in `notes/`

This draft lives in `notes/exec-plans/` specifically so it does **not** affect package building or publisher docs:
- `notes/` is excluded by `.Rbuildignore`
- it is outside `man/`, `vignettes/`, `doc/`, and `docs/`
- it can be committed to `main` as a planning artifact without changing the built package surface

## Ontology conventions that now constrain this plan

After reviewing `code/dfo-salmon-ontology/docs/CONVENTIONS.md`, this plan needs to follow four non-optional modeling rules:

1. **Compound variables / metrics are SKOS concepts, not OWL classes.**
   If routing leads to a stronger variable pick or a new-term recommendation, the target identity should be a SKOS variable concept in the appropriate scheme.
2. **Canonical I-ADOPT authoring is annotation-centric.**
   The ontology’s canonical pattern is local annotation properties such as `gcdfo:iadoptProperty`, `gcdfo:iadoptEntity`, `gcdfo:iadoptConstraint`, and `gcdfo:usedProcedure`.
3. **Procedure is not an I-ADOPT role.**
   The conventions explicitly replace `gcdfo:iadoptMethod` with `gcdfo:usedProcedure`; methods/protocols are handled as procedures, not as an I-ADOPT decomposition slot.
4. **Keep concept alignment separate from OWL equivalence.**
   If the routing logic or downstream issue templates talk about mappings, concept-to-concept mappings stay in SKOS space; do not blur them into OWL class equivalence language.

These rules do not change the immediate metasalmon routing goal, but they do change how the ExecPlan must describe the decomposition target and what “good” output looks like.

## Current State / Orientation

### Canonical files in play

- `R/semantics-helpers.R`
  - builds semantic targets and assigns measurement search roles like `variable`, `property`, `entity`, `constraint`, `method`, and `unit`
  - already contains the best current signal for deciding whether a row is a measurement-like / compound-variable target
- `R/llm-semantic-helpers.R`
  - builds LLM payloads
  - currently uses a generic candidate-assessment prompt for single-target and batched review
  - currently sends all review traffic through `.ms_llm_chat_json_request`
- `tests/testthat/test-llm-semantic-helpers.R`
  - already covers prompt batching, exploration, acceptance thresholds, and malformed LLM outputs

### Problem shape

Right now a measurement-row `term_iri` suggestion is reviewed the same way as a generic attribute or code-term suggestion. That collapses an important distinction:
- **simple label matching** works fine for many non-measurement terms
- **compound-variable interpretation** needs the model to reason about property + entity + qualifiers in an I-ADOPT frame

That mismatch is where broad-but-wrong variable picks sneak in.

## Desired Behavior

When a target looks like a measurement variable or another compound variable:
1. metasalmon should route LLM review through an I-ADOPT-aware path
2. that path should call the chat function `chat_decomposition()`
3. the decomposition-oriented response should still resolve back onto the existing candidate shortlist machinery
4. the decomposition framing should treat the selected variable as a **SKOS concept** whose meaning can be explained through local annotation-style slots (`property`, `entity`, `constraint`, optional `procedure`)
5. non-measurement targets should keep using the current generic review flow

Crucial nuance from the ontology conventions: this path should **not** talk as if method is a native I-ADOPT role. If procedure context matters, the plan should frame it as `usedProcedure`-style context, not `iadoptMethod`.

## Detection Rule (draft)

Start conservative.

Treat a target as decomposition-routed when **either** of these is true:
- the parent dictionary row has `column_role == "measurement"` **and** the target field is `term_iri`
- the target otherwise shows strong compound-variable signals (for example: observable-like variable language, combined property/entity phrasing, or other heuristics that already imply I-ADOPT-style decomposition)

Draft implementation preference:
- add one explicit routing helper in `R/llm-semantic-helpers.R`, something like `.ms_llm_should_route_to_decomposition(target_row)`
- make that helper easy to unit-test in isolation
- keep the first pass narrow rather than trying to infer every imaginable compound-variable case on day one

## Plan of Work

### Slice 1 — isolate the routing decision

Add a small internal predicate that decides whether a target should use the decomposition path.

Expected edits:
- `R/llm-semantic-helpers.R`
- maybe one tiny helper in `R/semantics-helpers.R` only if needed to avoid duplicating measurement/compound-variable heuristics

Acceptance for Slice 1:
- a plain measurement `term_iri` target routes to decomposition
- a measurement `property_iri` / `entity_iri` / `unit_iri` target does **not** route there by default unless explicitly intended
- a `method_iri` target is **not** treated as an I-ADOPT slot; if anything procedure-aware is added later, it must be framed separately as procedure context
- ordinary categorical / attribute / code-term targets stay on the generic route

### Slice 2 — add decomposition-specific chat payload generation

Add a decomposition-focused prompt builder for routed targets.

Expected characteristics:
- explicitly frames the task as I-ADOPT-style decomposition
- reminds the model that it must still choose from the provided candidates only
- instructs the chat layer to call `chat_decomposition()`
- tells the model that compound variables are represented as SKOS concepts, not OWL classes
- uses the ontology’s canonical local decomposition language: property, entity in ObjectOfInterest role, constraint, and optional procedure context via `usedProcedure`
- avoids any wording that implies an `iadoptMethod` slot exists in the canonical ontology pattern
- preserves the same downstream decision contract (`accept`, `review`, `propose_new_term`, candidate index, confidence, rationale, missing context) unless a stricter bridge is genuinely needed

Likely edit:
- `R/llm-semantic-helpers.R`

### Slice 3 — wire request selection without breaking the existing generic path

Introduce a routing step so target/batch assessment chooses between:
- current generic chat assessment path
- new decomposition chat path

Implementation preference:
- keep the generic request path intact for non-routed targets
- if batching mixed target types becomes awkward, prefer correctness over batching and fall back to per-record review for decomposition-routed records
- do **not** over-optimize this early; the hard part is semantic correctness, not shaving one request

### Slice 4 — regression tests

Add focused tests that prove:
- measurement `term_iri` records use decomposition routing
- compound-variable-like non-obvious cases use decomposition routing when intended
- generic non-measurement targets still use the current path
- decomposition prompts describe the target as a SKOS variable concept and do not instruct the model to use an `iadoptMethod` slot
- exploration / retry / validation behavior still works after routing is added

Likely edit:
- `tests/testthat/test-llm-semantic-helpers.R`

### Slice 5 — minimal release notes

If code ships, add a short `NEWS.md` note describing the routing change. Do **not** touch vignettes or publisher docs in the first slice unless the implementation changes public usage.

## Concrete Implementation Notes

### Suggested internal shape

Possible helper set:
- `.ms_llm_should_route_to_decomposition(target_row)`
- `.ms_llm_messages_for_decomposition_target(target_row, candidate_rows, context_chunks)`
- `.ms_llm_messages_for_decomposition_batch(records)` only if batching remains clean
- `.ms_llm_chat_decomposition_request(...)` **or** a thin branch inside the existing request layer, depending on how `chat_decomposition()` must be invoked

### Ontology-convention guardrails

The decomposition prompt and any downstream new-term guidance should explicitly preserve these ontology rules:
- variable identity lives in a SKOS concept scheme
- local annotation properties are the canonical decomposition pattern
- `entity` means the object of interest role, not just any nearby noun
- `procedure` is optional context and maps to `usedProcedure`-style handling, not an `iadoptMethod` role
- if a downstream issue/template is generated, it should ask for required ontology annotations (`prefLabel`/`definition`/`isDefinedBy` and scheme membership as appropriate) instead of speaking in package-only shorthand

### Important constraint

Do not make decomposition routing depend on publisher-doc files or build-time artifacts. Everything needed for the routing decision should come from the existing semantic target rows and their immediate context.

### Preferred fallback behavior

If decomposition routing fails for transport or parsing reasons:
- log the error in the existing LLM assessment error fields
- degrade gracefully to review instead of auto-selecting a candidate
- only fall back to the generic route if that fallback is deliberate and visible in code/tests

## Validation + Acceptance

Minimum validation for implementation:

1. Run targeted tests:
   - `devtools::test(filter = "llm-semantic-helpers")`
2. Run one measurement-target fixture that proves routed behavior
3. Run one non-measurement fixture that proves no accidental route bleed
4. Confirm no package-manifest or publisher-doc files changed unintentionally

Acceptance signals:
- routed measurement `term_iri` tests pass
- generic LLM assessment tests still pass
- decomposition prompt text matches the ontology conventions (SKOS variable concept + local annotation pattern + `usedProcedure` wording)
- no changes under `man/`, `vignettes/`, `doc/`, or `docs/` for this planning-only slice

## Risks / Watch-outs

- **Over-routing risk:** if the heuristic is too broad, normal attribute/categorical targets may get pushed through an unnecessary decomposition path.
- **Batching complexity:** mixed routed/non-routed batches may complicate the current batched assessment flow.
- **Prompt drift:** a decomposition-specific prompt could accidentally stop respecting the shortlist-only rule.
- **Procedure confusion:** it is easy to slide back into `iadoptMethod` language even though the ontology conventions explicitly moved procedure handling to `usedProcedure`.
- **Punning / class drift:** if prompt wording gets sloppy, downstream issue text could imply OWL classes where the conventions require SKOS variable concepts.
- **Function-call portability:** `chat_decomposition()` must work across the supported chat backends or degrade cleanly.

## Decision Log

- Decision: Keep the draft ExecPlan in `notes/exec-plans/` rather than `docs/plans/`.
  Rationale: Brett explicitly wants a location that will not affect package building or publisher docs; `notes/` is excluded from package builds and stays out of pkgdown/published documentation paths.
  Date/Author: 2026-04-02 / Alan

- Decision: First implementation slice should change routing only, not ontology sources or public-facing documentation.
  Rationale: That keeps the change small, testable, and semantically focused.
  Date/Author: 2026-04-02 / Alan

- Decision: Measurement `term_iri` routing is the primary trigger; broader compound-variable detection is secondary and should start conservative.
  Rationale: This captures the clear win first without turning heuristics into soup.
  Date/Author: 2026-04-02 / Alan

- Decision: The ExecPlan now treats compound-variable identity as a SKOS concept outcome, not an OWL class outcome.
  Rationale: `code/dfo-salmon-ontology/docs/CONVENTIONS.md` explicitly says compound variables / metrics should be modeled as SKOS concepts in the appropriate scheme.
  Date/Author: 2026-04-02 / Alan

- Decision: Procedure context must be described with `usedProcedure`-style language, not `iadoptMethod`.
  Rationale: The ontology conventions explicitly replace `gcdfo:iadoptMethod` with `gcdfo:usedProcedure` and state that method is not an I-ADOPT role.
  Date/Author: 2026-04-02 / Alan

## Progress

- [x] (2026-04-02) Draft ExecPlan created in `notes/exec-plans/`.
- [x] (2026-04-02) Draft positioned to avoid package-build and publisher-doc side effects.
- [x] (2026-04-02) Reviewed `code/dfo-salmon-ontology/docs/CONVENTIONS.md` and tightened the plan around SKOS variable concepts, local annotation-style decomposition, and `usedProcedure` wording.
- [ ] Implementation branch opened from clean `main` state.
- [ ] Routing predicate added.
- [ ] `chat_decomposition()` path wired.
- [ ] Regression tests added and passing.
- [ ] Short `NEWS.md` entry added if implementation lands.

## Outcomes & Retrospective

Current outcome: planning artifact only. No runtime behavior changed yet.

If this ships cleanly, the likely payoff is better measurement-variable term review with less generic “close enough” candidate selection. The thing to watch is whether the compound-variable heuristic stays sharp or starts hauling half the repo into I-ADOPT theater.
