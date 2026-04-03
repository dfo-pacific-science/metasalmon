# ExecPlan — bundle-aware semantic fit, slot-aware retrieval, and ontology-gap escalation

This ExecPlan is a living document. Keep `Progress`, `Decision Log`, and `Validation + Acceptance` current as the work moves.

## Purpose / Big Picture

metasalmon currently does a useful first pass:
- deterministic search builds a candidate shortlist per semantic slot
- LLM review picks from that shortlist
- `create_sdp()` can prefill review-ready IRIs from the reviewed shortlist

That flow is good enough when the right concept is already in the shortlist and each slot is easy to interpret in isolation.

It breaks down when:
- the **best available** term in the shortlist is not the **right** term
- the model evaluates slots independently instead of checking whether the **whole decomposition** makes sense
- the ontology appears to lack the precise concept needed, but the model still forces a near miss instead of escalating a gap

The goal of this update is to move from **slotwise nearest-neighbour selection** toward **bundle-aware semantic fit**:
1. understand the full variable first
2. decompose it across `term_iri` / `property_iri` / `entity_iri` / `unit_iri` / optional `constraint_iri` / optional `method_iri`
3. judge whether the chosen bundle is coherent as a whole
4. if the shortlist is wrong, retry retrieval more intelligently or recommend a new term request in `smn` / `gcdfo`

## Relationship to the I-ADOPT chat decomposition plan

This roadmap now assumes the routing / architecture foundation described in:
- `notes/exec-plans/2026-04-02-i-adopt-chat-decomposition-draft.md`

That means this plan should be implemented **through** the decomposition route rather than beside it.

Concretely:
- Phase 1 prompt changes should ride on the `chat_decomposition()`-style path introduced by the I-ADOPT plan
- bundle-aware review should reuse that route for measurement-like targets instead of creating a second competing LLM-review architecture
- package-level `method_iri` handling must be treated as a **procedure-context bridge** (`usedProcedure`-style reasoning), not as if method were a canonical I-ADOPT slot
- richer outcomes such as `retry_search` and `request_new_term` are meant to extend the decomposition route, not replace it with a new API family

## Why this plan exists now

Live experiments on the DFO Salish Sea juvenile salmon trawl catch table showed that simply raising confidence thresholds is not a satisfying fix.

What actually happened:
- **`openrouter/free`** produced weak but confident-enough picks such as `Fork-length field method` for `CATCH_WEIGHT`
- **`qwen/qwen3.6-plus:free`** improved with better prompting and often stopped selecting `CatchContext`, which shows prompt/guidance can materially improve a medium model
- **`openai/gpt-5.4-mini`** produced the best core picks (`CatchAbundance`, `Count`, `FishWeight`) but still selected `CatchContext` in ways that were ontology-legal but not always semantically useful

That combination implies:
- prompt quality matters
- bundle-level reasoning matters
- retrieval quality matters
- confidence-throttling alone is an insufficient long-term answer

## Findings from the current experiments

### Real-model findings already observed

#### `openrouter/free`
- selected weak / misleading terms such as `Fork-length field method` for `CATCH_WEIGHT`
- behaved like a shoddy model, especially for `method_iri`

#### `qwen/qwen3.6-plus:free`
- baseline batched run over-selected `CatchContext`
- prompt experiments showed large improvements when guidance emphasized role-fit
- per-target or rubric-guided review often settled on the cleaner set:
  - `CATCH_COUNT -> CatchAbundance`
  - `CATCH_COUNT -> Count`
  - `CATCH_WEIGHT -> FishWeight`

#### `openai/gpt-5.4-mini`
- best useful picks among tested models
- still selected `CatchContext` for `CATCH_COUNT` / `CATCH_WEIGHT`
- those picks were structurally allowed but not obviously the clearest semantics

### Modeling interpretation from those runs

`CatchContext` is a real shared concept and is mapped to the I-ADOPT constraint family:
- `smn:CatchContext` definition: “Measurement context indicating values refer to fishery catch.”
- `smn:CatchContext skos:relatedMatch iadopt:Constraint`

But that does **not** imply it should be used automatically whenever a field name contains `CATCH_`.

The real question is whether the constraint:
- adds necessary disambiguating meaning
- or merely restates what is already obvious from the chosen property/entity/variable framing

This is where bundle-level reasoning matters.

## Problem statement (precise)

The current LLM path makes three avoidable mistakes:

1. **Best-available-term mistake**
   - The model chooses the best local candidate in `smn` / `gcdfo` rather than the most precise semantic fit for the column.

2. **Slot-isolation mistake**
   - The model picks one slot at a time without checking whether all selected slots compose into a coherent variable meaning.

3. **No-gap-escalation mistake**
   - When the right concept does not exist in the shortlist (or possibly in the ontology at all), the model still forces a nearby term rather than saying:
     - retry retrieval with more appropriate sources, or
     - propose a new term in `smn` / `gcdfo`

## Scope

In scope:
- LLM semantic-review flow used by `suggest_semantics()` and `create_sdp()`
- prompt design for shortlist review
- bundle-aware evaluation across semantic slots
- second-pass retrieval when shortlist fit is poor
- explicit ontology-gap / new-term escalation outputs
- tests and experiment harnesses that prove the change improves real cases

Out of scope for this slice:
- broad ontology refactoring in `smn` / `gcdfo`
- publishing new ontology terms in the same PR
- changing unrelated package UX or docs beyond what is needed to explain new behavior
- solving every long-tail semantic ambiguity in one pass

## Design stance

### Core principle

The LLM should not merely answer:
- “Which candidate label looks most similar?”

It should answer:
- “What does this variable mean as a whole?”
- “What decomposition best captures that meaning?”
- “Do these chosen pieces make sense together?”
- “If they do not, do I need better retrieval or a new term?”

### Intended decision hierarchy

For each measurement-like / decomposition-like target:

1. **Whole-variable hypothesis**
   - infer the complete semantic intent of the column from:
     - field name
     - column description
     - table context
     - dataset-level context files
     - candidate shortlist

2. **Decomposition proposal**
   - propose a full bundle:
     - `term_iri`
     - `property_iri`
     - `entity_iri`
     - `unit_iri`
     - optional `constraint_iri`
     - optional `method_iri`
   - ontology nuance: package output may still write `method_iri`, but decomposition reasoning should treat that as a bridge to procedure context (`usedProcedure`-style semantics), not as a native I-ADOPT role

3. **Bundle coherence check**
   - verify slot compatibility and semantic non-redundancy

4. **Escalation choice**
   - `use_existing_terms`
   - `retry_retrieval`
   - `propose_new_term`

## Product behavior to aim for

### New review outcome types

The LLM review layer should be able to return one of:
- `accept_existing`
- `accept_with_abstentions`
- `retry_search`
- `request_new_term`
- `reject_shortlist`

These are richer than today’s effective “pick something or abstain row-by-row” behavior.
For the first implementation slice, these richer actions may still be bridged onto the narrower package-level contract (`accept` / `review` / `request_new_term`) as long as the routing and stored metadata preserve the distinction needed for later expansion.

### Retry-search behavior

When shortlist fit is poor, the system should be able to say:
- this slot has the wrong candidate family
- retry with narrower or more appropriate source settings
- retry with role-specific query phrasing
- retry with a broader or alternate ontology scope only for the affected slot

### New-term behavior

When the ontology likely lacks the right term, the system should capture:
- suggested label
- draft definition
- proposed parent / scheme / slot family
- whether it belongs in shared `smn` or DFO-specific `gcdfo`
- why existing candidates were rejected

This should feed issue creation later rather than forcing a bad existing IRI.

## Architectural changes (proposed)

### 1) Add a whole-bundle review mode

Introduce a review path for measurement-like targets that:
- sees the target column as a **single semantic bundle problem**
- can reason across all slots before finalizing any one slot

This can still write back to the existing long-format suggestion table, but the reasoning should happen at bundle level first.

Potential implementation shape:
- build a `bundle_target` object per column
- include role-specific shortlist candidates under that target
- ask the LLM to choose a coherent bundle or reject the shortlist

### 2) Add slot-aware retrieval fallback

When bundle review says “none of these are quite right”, run a second deterministic retrieval pass.

Slot-aware source bias should be explicit:
- **unit** → QUDT first
- **property/entity/constraint/method** → `smn` / `gcdfo` first
- external vocabularies only when appropriate

Slot-aware query rewriting examples:
- `CATCH_WEIGHT` property retry → “weight of catch”, “catch mass”, “total catch mass”
- `CATCH_COUNT` entity retry → “catch abundance”, “number of fish in catch”
- `constraint` retry only when the field meaning actually implies contextual qualification

### 3) Add deterministic bundle-fit validators

The post-LLM layer should reject or downgrade bundles when:
- a `constraint_iri` merely restates obvious field context instead of disambiguating meaning
- a `method_iri` is chosen without evidence that the field describes a method/protocol/procedure
- unit, property, and entity do not form a plausible measurement bundle
- selected slots contradict the column definition or table context

This is not a generic confidence hack. It is a semantic compatibility check.

### 4) Add explicit gap escalation output

If the bundle cannot be made coherent from available candidates, return structured gap data instead of a forced match.

Draft object shape:

```json
{
  "action": "request_new_term",
  "slot": "property",
  "suggested_label": "Catch mass",
  "suggested_definition": "Total mass of organisms captured in a fishing or survey event.",
  "suggested_namespace": "smn|gcdfo",
  "reason": "Existing candidates such as FishWeight are too individual-organism oriented; CatchContext would only partially patch the mismatch."
}
```

## Prompting strategy changes

### Prompt rule: role-fit beats topical relatedness

Prompts should explicitly tell the model:
- a candidate can be topically related but still wrong for the slot
- `constraint` is only for contextual qualifiers that change meaning
- `method` is only for procedure/protocol/estimation context actually evidenced by the field
- if the right candidate family is absent, do **not** force a local winner

### Prompt rule: decide the whole variable first

Before choosing slot values, the prompt should require a short internal summary like:
- “This variable represents ___ about ___, in ___ units, under ___ context, using ___ procedure if known.”

Then slot choices should be checked against that summary.

### Prompt rule: explain abstentions and retries

If a slot is left blank or escalated, the review should explain whether that happened because:
- the field gives no evidence for that slot
- the shortlist lacks a fitting candidate
- the ontology likely lacks a needed term

## Constraint-slot guidance (special case)

Constraint is currently the hardest slot and should get specialized handling.

### Constraint should only be selected when it changes meaning

Good examples:
- run context
- ocean phase / terminal phase
- spawner stage
- natural origin
- benchmark framing
- age class / life stage

Weak or suspicious cases:
- adding `CatchContext` to a variable whose entity/property already fully encode catch meaning
- using generic context terms to patch a poor property/entity choice

### Working decision rule

A `constraint_iri` should usually require at least one of:
- explicit evidence in the field label or description
- explicit evidence in the broader table / dataset context
- clear semantic need after considering selected entity/property

If the variable meaning is already precise without the constraint, prefer leaving it blank.

## `method_iri` guidance (special case)

A `method_iri` should only be selected when the field meaning or dictionary explicitly signals:
- method
- protocol
- procedure
- gear-driven estimation framing
- lab/field measurement mode

This avoids repeats of failures like `Fork-length field method` for `CATCH_WEIGHT`.

## Ontology-namespace routing rule

Use the existing durable repo-role boundary:
- shared reusable semantics belong in `smn`
- DFO-specific operational/policy/program semantics belong in `gcdfo`

When proposing a new term, the system should decide namespace based on whether the concept is:
- broadly reusable across salmon data contexts → `smn`
- DFO-program-specific / policy-specific / reporting-specific → `gcdfo`

## Open modeling question flagged by current experiments

`CATCH_WEIGHT` may be exposing an ontology gap or at least a candidate-family weakness.

Current ambiguity:
- `FishWeight` exists, but may be too individual-organism oriented for “weight of catch”
- `CatchContext` partly helps, but may be semantically redundant or patch-like rather than ideal

Working hypothesis:
- the right long-term answer may be a more precise catch-mass / catch-weight concept, depending on ontology policy
- do **not** conflate this with biomass density unless the denominator (swept volume / swept area / filtered volume / effort framing) is actually present

## Implementation phases

### Phase 0 — capture the evidence pack
- [ ] Preserve today’s experiment outputs in a stable notes/artifact location
- [ ] Summarize the three real-model runs and qwen prompt experiments in a machine-readable comparison file
- [ ] Identify the minimum representative regression cases

### Phase 1 — role-fit prompting improvements
- [ ] Implement these prompt changes on top of the `chat_decomposition()` routing path from the I-ADOPT plan
- [ ] Add explicit role-fit rubric to LLM prompts
- [ ] Add whole-variable summary step before slot selection
- [ ] Add “do not force local winners when shortlist family is wrong” language
- [ ] Validate on qwen and gpt-5.4-mini

### Phase 2 — bundle-aware review object
- [ ] Build an internal bundle target per measurement-like column
- [ ] Allow the LLM to evaluate the full decomposition together
- [ ] Persist bundle-level rationale and slot-level outputs cleanly

### Phase 3 — slot-aware second-pass retrieval
- [ ] Add retry pathway when bundle fit is poor
- [ ] Add role-specific source/query tuning
- [ ] Record when retry changed the result

### Phase 4 — explicit gap escalation
- [ ] Define structured `retry_search` / `request_new_term` outputs
- [ ] Thread those outputs through `suggest_semantics()` results
- [ ] Add optional issue-draft helpers later if useful

### Phase 5 — deterministic post-validators
- [ ] Add bundle-fit validators for `constraint_iri` and `method_iri`
- [ ] Add compatibility checks for unit/property/entity combinations
- [ ] Ensure validators explain *why* a slot was rejected

### Phase 6 — prefill policy revision
- [ ] Revisit `create_sdp()` prefill rules after better prompting/retrieval land
- [ ] Prefer semantic fit / bundle coherence over crude confidence-only blocking
- [ ] Keep a defensive fallback for provider-wide failures

## Validation + Acceptance

### Acceptance criteria

The update is successful when:
- the Salish Sea catch benchmark no longer forces `CatchContext` where it adds no real clarity
- `method_iri` stops getting nonsensical procedure picks for plain measurement fields
- qwen medium-quality runs improve with the new prompt/bundle flow without requiring a blanket confidence clamp
- gpt-5.4-mini uses the new whole-bundle framing to either:
  - select a coherent existing decomposition, or
  - escalate a gap instead of forcing a near miss
- package output makes it obvious when a new ontology term is likely needed

### Benchmark cases to keep in the regression suite

At minimum:
- `CATCH_COUNT`
- `CATCH_WEIGHT`
- one clearly method-bearing field
- one clearly context-bearing field
- one example where the correct answer should be “request a new term”

### Validation modes

- unit tests for prompt builders, validators, and escalation outputs
- mocked JSON-response tests for retry / gap paths
- live-model benchmark runs for:
  - `qwen/qwen3.6-plus:free`
  - `openai/gpt-5.4-mini`
- diff checks on resulting `column_dictionary.csv`

## Risks / Failure modes

- prompt complexity could improve one provider while hurting another
- whole-bundle review could become too expensive or slow
- retry retrieval could create noisy oscillation if not bounded
- new-term escalation could become over-triggered if validators are too strict
- ontology boundary decisions (`smn` vs `gcdfo`) could become fuzzy without clear issue templates

## Decision Log

- 2026-04-02 — Decided that confidence-threshold-only mitigation is insufficient; the better target is improved semantic fit.
- 2026-04-02 — Accepted that `constraint_iri` is the hardest slot and requires special handling.
- 2026-04-02 — Decided that bundle-level reasoning should precede per-slot acceptance.
- 2026-04-02 — Decided that shortlist failure must support retry-search or new-term escalation instead of forced local winners.
- 2026-04-02 — Noted that `CATCH_WEIGHT` may surface an ontology gap or candidate-family weakness rather than a mere model error.

## Progress

- [x] Live model comparison completed across `openrouter/free`, `qwen/qwen3.6-plus:free`, and `openai/gpt-5.4-mini`
- [x] Prompt experiments completed for qwen, showing role-fit guidance materially improves results
- [ ] Convert prompt experiment findings into package-level prompt changes
- [ ] Design and implement bundle-aware review data structure
- [ ] Implement retry-search / new-term escalation outputs
- [ ] Rework prefill policy around semantic fit rather than confidence-only gating

## Notes for whoever implements this

Do not frame success as “the model filled more cells.”

Frame success as:
- fewer semantically wrong prefills
- clearer abstentions
- more defensible decompositions
- explicit ontology-gap surfacing when the right term is missing

That is a much better trade than squeezing a few extra dubious IRIs into `column_dictionary.csv`.
