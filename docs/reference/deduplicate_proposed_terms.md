# Deduplicate proposed ontology terms

Applies I-ADOPT compositional deduplication to a gpt_proposed_terms
dataframe. This prevents term proliferation by:

1.  Removing duplicates across tables (same term_label)

2.  Collapsing age-stratified variants (X Age 1..7) into one base term

3.  Collapsing phase-stratified variants (Ocean/Terminal/Mainstem X)
    into one base term

4.  Identifying terms that should use constraint_iri instead of new
    term_iri

## Usage

``` r
deduplicate_proposed_terms(proposed_terms, warn_threshold = 30L)
```

## Arguments

- proposed_terms:

  A data frame with columns: term_label, term_definition, term_type,
  suggested_parent_iri. Typically loaded from gpt_proposed_terms.csv.

- warn_threshold:

  Integer. If the input has more than this many rows, issue a warning
  about potential over-engineering. Default is 30.

## Value

A tibble with deduplicated terms and additional columns:

- `is_base_term`: TRUE if this is the canonical base term for a pattern

- `needs_age_facet`: TRUE if age variants should use constraint_iri

- `needs_phase_facet`: TRUE if phase variants should use constraint_iri

- `collapsed_from`: Count of how many variants were collapsed into this
  term

- `dedup_notes`: Explanation of deduplication applied

## Details

The function returns a deduplicated dataframe with added columns for
facet handling.

**Target ratio**: For a dictionary with N measurement columns, expect
~N/10 to N/5 distinct base terms, NOT N terms. If the output still has
\>30 rows, consider further manual review.

**Anti-patterns detected**:

- "Spawners Age 1", "Spawners Age 2", ... patterns → collapsed to
  "SpawnerCount"

- Duplicate term_labels across different tables → deduplicated

- Phase-stratified variants (Ocean X, Terminal X) → collapsed to base
  term

## Examples

``` r
if (FALSE) { # \dontrun{
# Load raw proposed terms
proposed <- readr::read_csv("work/semantics/gpt_proposed_terms.csv")

# Deduplicate
deduped <- deduplicate_proposed_terms(proposed)

# Review collapsed terms
deduped |> dplyr::filter(collapsed_from > 1)

# Write cleaned output
readr::write_csv(deduped, "work/semantics/gpt_proposed_terms_deduped.csv")
} # }
```
