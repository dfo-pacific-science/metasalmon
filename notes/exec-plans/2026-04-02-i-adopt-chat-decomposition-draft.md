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

Out of scope for the **first implementation slice**:
- changing the deterministic search index itself
- changing ontology sources or ranking weights
- rewriting vignettes, pkgdown pages, or publication docs in the same slice
- building a terminal wrapper before the core session engine exists
- turning the first pass into a freeform general-purpose chat toy

## What this draft now covers beyond the first routing slice

This draft started as a narrow routing plan, but it now also captures the broader architectural requirements for interactive chat-backed metadata and semantics curation so those decisions do not get lost in message history.

That means the document now covers two layers:
1. **Immediate slice:** route measurement / compound-variable `term_iri` review through `chat_decomposition()`.
2. **Follow-on architecture:** the shared session engine, state model, question-planning rules, context-window discipline, provenance outputs, provider abstraction, and R-console-first UX that the decomposition path should eventually live inside.

## Relationship to the bundle-aware semantic-fit roadmap

This plan is now the **routing and prompt-architecture foundation** for the broader bundle-aware semantic-fit work captured in:
- `notes/exec-plans/2026-04-02-llm-semantic-fit-retrieval-gap-escalation.md`

The intended division of labor is:
- this plan defines **when** metasalmon should switch from generic shortlist review to decomposition-oriented review and what ontology conventions constrain that route
- the bundle-aware plan defines **how** decomposition-oriented review should handle whole-variable reasoning, second-pass retrieval, bundle-fit validation, and ontology-gap escalation

To keep the two plans from drifting into parallel designs:
- measurement-like semantic review should route through the decomposition path described here, rather than inventing a second bundle-review prompt stack elsewhere
- the broader roadmap may temporarily preserve bridge outputs like `accept` / `review` / `request_new_term`, but richer actions such as `retry_search` should be treated as a forward-compatible extension of this same route, not a separate API family
- package-level `method_iri` behavior should continue to be described as a **bridge to procedure context** (`usedProcedure`-style reasoning), not as if `method` were a native I-ADOPT slot

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

## Broader chat-LLM architecture this mode must fit into

### Core stance

Do **not** build “just a chat function.” Build a **curation session engine with a chat front-end**.

The context window is a scratchpad, **not memory**. Real memory must live in metasalmon-owned session state on disk / in structured objects, not in whatever portion of the transcript happens to fit in the next prompt.

### Intended user-facing surface

The longer-term package surface should likely expose:
- `chat_semantics()` for column / variable semantics confirmation
- `chat_dataset_metadata()` for dataset-level metadata curation
- `chat_metadata()` as umbrella mode for dataset + columns + new-term handling
- `chat_iadopt()` as strict variable-decomposition mode for hard semantic cases

For the current slice, `chat_decomposition()` is the routing-level chat function name that should be called for decomposition-oriented review. Longer term, that function should sit underneath a shared curation engine rather than remain an orphan one-off path.

### Shared engine shape

Internally, the chat surface should converge on one engine with responsibilities like:
- `start_curation_session()`
- `run_curation_turn()`
- `propose_curation_patch()`
- `approve_curation_patch()`

The exact exported names can change; the architectural point is that decomposition, semantics, and dataset metadata should share one stateful session core.

### Session state, not transcript, is the source of truth

A representative session object should track things like:
- `session_id`
- `mode`
- `provider`
- `dataset_draft`
- `column_drafts`
- `evidence_index`
- `approved_facts`
- `unresolved_items`
- `question_queue`
- `transcript`
- `turn_summaries`
- `proposed_patch`
- `new_term_requests`

The important design rule is not the exact field names. It is that the chat session must persist **structured state** separately from the raw chat log.

### Recommended curation workflow

#### Phase 1 — infer first

Before asking the user anything, run:
- deterministic extraction from supplied files
- metadata heuristics
- existing `suggest_semantics()`-style candidate search
- LLM draft pass over **summarized** context

Expected outputs from the inference pass:
- draft dataset metadata
- draft column semantics
- confidence per field / column
- explicit unresolved items
- possible new-term candidates

#### Phase 2 — question planning

Do not ask arbitrary questions. Rank them by information gain using factors like:
- impact on final output
- uncertainty
- dependency centrality
- whether one answer unlocks multiple other fields

Useful question classes:
- **blocking** — needed to finish the output
- **disambiguating** — picks between plausible meanings
- **completeness** — useful but optional metadata
- **new-term** — proves existing ontology is insufficient

Each question should carry:
- target field(s)
- why it matters
- expected answer type
- default / current guess

#### Phase 3 — interactive rounds

For general metadata / semantics curation, ask **3–7 questions per round**, grouped sensibly by cluster:
- dataset identity + scope
- provenance + methods
- temporal/spatial coverage
- closely related columns
- one possible new-term cluster

Bad grouping is random grab-bag questioning across unrelated schema areas.

For strict I-ADOPT decomposition, keep rounds tighter: **2–4 questions per round**. Compound-variable decomposition gets mushy fast if the question batch is too wide.

After each round:
- update structured state
- record a turn summary and state delta
- re-rank open questions
- either ask the next batch or show the proposed output

#### Phase 4 — proposal + approval

The chat should end in explicit artifacts, not “cool, thanks.”

Each round / endpoint should be able to show:
- what changed
- what is still uncertain
- whether a new-term request is needed
- the exact resulting structured output

Offer explicit next actions:
- approve
- revise
- ask more questions
- mark unknown
- generate new-term request

### Structured outputs the chat must produce

The longer-term curation flow should be able to emit:

1. **Dataset metadata patch**
   - title
   - abstract
   - creator / owner / contact
   - license
   - spatial coverage
   - temporal coverage
   - methods summary
   - provenance summary
   - caveats / quality notes

2. **Column semantic patch**
   - role
   - label
   - description
   - chosen term IRI
   - confidence
   - rationale
   - evidence refs
   - whether the user confirmed it

3. **New-term request artifact**
   - proposed label
   - proposed definition
   - candidate parent / module
   - synonyms
   - why existing terms failed
   - supporting evidence
   - affected columns

4. **Provenance bundle**
   - transcript
   - turn summaries
   - evidence refs used
   - model / provider info
   - timestamp
   - user approval status

That provenance bundle matters because six weeks later someone will ask why a column was mapped to a particular term, and “the model seemed vibey about it” is not a serious answer.

### Context-window management rules

1. **Never keep resending raw files.**
   At session start: chunk files, summarize them, extract structured facts, and keep excerpt IDs / references.
2. **Maintain three memory layers.**
   - raw evidence store on disk
   - structured session state
   - a prompt slice containing only the subset needed for the current turn
3. **Summarize aggressively after each turn.**
   Persist `turn_summary`, `state_delta`, and `approved_facts_delta`; do not drag the whole backscroll forever.
4. **Retrieval beats stuffing.**
   If context files are large, index chunks locally and retrieve top-k snippets per unresolved item.
5. **Ask narrower questions when the budget is tight.**
   One metadata cluster or one semantic cluster at a time beats prompt bloat.

### Runtime and provider stance

Preferred runtime for the first real interactive implementation:
- **current R console first**
- use `cli` + `readline()` + lightweight commands
- make a terminal wrapper optional later, not the primary design center

Preferred provider boundary:
- one thin package-local adapter like `ms_chat(provider, messages, response_schema = NULL, temperature = 0.2)`
- adapters for OpenAI, Anthropic, Gemini, OpenRouter, and Ollama/local can sit behind that boundary
- do not let provider-specific API quirks leak through the whole package

### New-term escalation rule

Escalate to explicit new-term mode when one or more of these are true:
- deterministic candidate retrieval is weak
- LLM confidence stays low after clarification
- the user rejects all plausible candidates
- there is a stable concept description but no good existing term

At that point, stop pretending another generic chat turn will magically fix it. Ask the targeted new-term questions and emit a real request artifact.

### I-ADOPT mode should be first-class, not “semantics with different vibes”

Treat strict variable decomposition as its own sub-mode, with core slots like:
- property
- object of interest / entity
- matrix / medium
- context object
- constraint(s)

And keep adjacent-but-not-core slots separate:
- procedure
- unit
- statistic / aggregation
- derivation / estimation status
- temporal / spatial granularity

That slot separation is one of the main reasons `chat_iadopt()` / `chat_decomposition()` is worth doing at all. Without it, the model just makes conceptual soup faster.

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

### Follow-on slices — broader interactive curation engine (captured now, not required for Slice 1)

These are not prerequisites for the immediate routing slice, but they are now part of the plan so the decomposition path grows into a coherent system rather than a weird special case.

#### Slice 6 — session-engine scaffolding

Add the first package-internal state model for interactive curation sessions.

Acceptance:
- structured session state exists independently of the transcript
- the engine can save approved facts, unresolved items, turn summaries, and proposed patches
- decomposition mode is wired as one mode inside the shared engine, not as a fully separate architecture

#### Slice 7 — question planner + grouped turns

Add question planning that ranks unresolved items by information value and groups questions sensibly.

Acceptance:
- general metadata / semantics mode asks grouped 3–7 question rounds
- decomposition mode asks tighter 2–4 question rounds
- each question exposes target fields, why it matters, expected answer type, and current/default guess

#### Slice 8 — structured outputs + provenance bundle

Add machine-readable curation artifacts.

Acceptance:
- dataset metadata patch artifact exists
- column semantic patch artifact exists
- new-term request artifact exists
- provenance bundle records transcript, turn summaries, evidence refs, provider/model, timestamp, and approval status

#### Slice 9 — provider adapter + R-console UI

Implement one package-local chat adapter boundary and a minimal interactive R-console surface.

Acceptance:
- one `ms_chat(...)`-style package boundary exists
- at least 1–2 providers work behind the same interface
- R-console flow works via `cli` / `readline()` without needing a separate terminal launcher
- any terminal wrapper is explicitly layered on top later rather than baked into core logic

#### Slice 10 — retrieval / resume / narrowing controls

Add the context-budget protections needed to keep longer sessions sane.

Acceptance:
- context files are chunked / indexed once rather than resent every turn
- sessions can save / resume cleanly
- the engine can retrieve top-k relevant snippets per unresolved item
- the UI can narrow scope when token budget is tight instead of bloating prompts

## Concrete Implementation Notes

### Suggested internal shape

Possible helper set:
- `.ms_llm_should_route_to_decomposition(target_row)`
- `.ms_llm_messages_for_decomposition_target(target_row, candidate_rows, context_chunks)`
- `.ms_llm_messages_for_decomposition_batch(records)` only if batching remains clean
- `.ms_llm_chat_decomposition_request(...)` **or** a thin branch inside the existing request layer, depending on how `chat_decomposition()` must be invoked
- later, a shared session-engine layer for `chat_metadata()` / `chat_semantics()` / `chat_iadopt()` rather than duplicated provider loops
- later, a question planner that carries target fields, explanation, expected answer type, and current guess for each promptable item
- later, provenance writers for dataset patches, semantic patches, new-term requests, and approval state

For strict decomposition mode, useful future command affordances include things like:
- `/why <slot>`
- `/options <slot>`
- `/accept <slot> <candidate>`
- `/none <slot>`
- `/newterm <slot>`
- `/preview`
- `/approve`

Those do not belong in the first routing slice, but the exec plan should preserve them as part of the target UX.

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
- **Transcript-as-memory trap:** if later interactive work treats the raw chat transcript as the state store, context limits will quietly rot session quality.
- **Chat-toy drift:** a freeform conversational UX without structured patches, approval, and provenance will feel clever but produce lousy reproducibility.
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

- Decision: The broader interactive design must treat context windows as scratchpads, not memory.
  Rationale: Longer curation sessions will fall apart if state lives only in chat history rather than a structured session object.
  Date/Author: 2026-04-02 / Alan

- Decision: The first real interactive UX should run in the current R console, with any terminal wrapper layered on later.
  Rationale: That is the simplest portable path across RStudio, terminal R, and VS Code R, and it keeps the engine/UI boundary clean.
  Date/Author: 2026-04-02 / Alan

- Decision: I-ADOPT decomposition should be a first-class mode inside a shared curation engine, not a detached prompt hack.
  Rationale: Decomposition, dataset metadata, and column semantics need shared state, shared provenance, and a common approval model.
  Date/Author: 2026-04-02 / Alan

## Progress

- [x] (2026-04-02) Draft ExecPlan created in `notes/exec-plans/`.
- [x] (2026-04-02) Draft positioned to avoid package-build and publisher-doc side effects.
- [x] (2026-04-02) Reviewed `code/dfo-salmon-ontology/docs/CONVENTIONS.md` and tightened the plan around SKOS variable concepts, local annotation-style decomposition, and `usedProcedure` wording.
- [x] (2026-04-02) Expanded the draft to capture the broader interactive chat architecture: session-state model, question planner, structured outputs, provenance, provider boundary, context-budget rules, and R-console-first runtime stance.
- [ ] Implementation branch opened from clean `main` state.
- [ ] Routing predicate added.
- [ ] `chat_decomposition()` path wired.
- [ ] Regression tests added and passing.
- [ ] Short `NEWS.md` entry added if implementation lands.

## Outcomes & Retrospective

Current outcome: planning artifact only. No runtime behavior changed yet.

The draft now covers both:
- the immediate routing slice for `chat_decomposition()` on measurement / compound-variable targets, and
- the broader interactive curation architecture it should eventually belong to.

If this ships cleanly, the likely payoff is better measurement-variable term review with less generic “close enough” candidate selection, plus a clearer path toward structured interactive metadata / semantics curation that does not confuse prompt history with actual state. The thing to watch is whether the compound-variable heuristic stays sharp or starts hauling half the repo into I-ADOPT theater.
