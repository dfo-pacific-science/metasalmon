# Linking to Standard Vocabularies

## Overview

When you reuse published salmon data terms, your dictionary becomes
easier for other scientists and machines to understand. This guide
explains when to reuse an existing term, how to point to it, and how to
discover terms without drowning in jargon.

### Why reuse shared terms?

- **Consistency**: Everyone who links to the same term knows they are
  talking about the same thing. For example, connecting `SPAWN_EST` to a
  DFO term called “Natural spawner count” removes guesswork.
- **Automation**: Tools can read a standard `IRI` (Internationalized
  Resource Identifier, a kind of web address for a definition) and know
  what to expect without human explanation.
- **Future-proofing**: Standards, like the DFO Salmon Ontology, evolve
  alongside policy. By linking to them now, you can pick up improvements
  later.

### Choosing a term

1.  **Look for a term that matches the column**. Use
    `find_terms("spawner count")` to search DFO, OLS, and other
    vocabularies.

``` r

library(metasalmon)
devtools::load_all(".")
find_terms("spawner count",
           role = "property",
           sources = sources_for_role("property")) |>
  dplyr::select(label, source, ontology, score, alignment_only) |>
  head()
```

This example uses the role-aware source set and surfaces
`alignment_only` so you can down-weight Wikidata crosswalks when
reviewing candidates. The `score` column shows the computed ranking,
which factors in ontology preferences, cross-source agreement, and ZOOMA
confidence.

#### Available sources by role

[`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
can query multiple vocabulary sources. Use
[`sources_for_role()`](https://dfo-pacific-science.github.io/metasalmon/reference/sources_for_role.md)
to get the recommended sources for each I-ADOPT role:

``` r

sources_for_role("unit")
# Returns: c("qudt", "nvs", "ols")

sources_for_role("entity")
# Returns: c("gbif", "worms", "bioportal", "ols")
```

| Role | Recommended Sources | Notes |
|----|----|----|
| `unit` | QUDT, NVS P06, OLS | QUDT preferred for SI units |
| `property` | NVS P01, OLS, ZOOMA | Measurement ontologies like STATO/OBA |
| `entity` | GBIF, WoRMS, BioPortal, OLS | Taxon resolvers for species |
| `method` | BioPortal, OLS, ZOOMA | gcdfo SKOS methods preferred |
| `variable` | NVS, OLS, ZOOMA | Compound observables |

#### Searching for units with QUDT

For unit columns, QUDT provides authoritative unit IRIs:

``` r

find_terms("kilogram", role = "unit", sources = sources_for_role("unit")) |>
  dplyr::select(label, iri, source, score) |>
  head()
```

#### Searching for taxa with GBIF/WoRMS

For species or organism columns, use taxon resolvers:

``` r

find_terms("Oncorhynchus kisutch", role = "entity", sources = c("gbif", "worms")) |>
  dplyr::select(label, iri, source, ontology, score) |>
  head()
```

#### Interpreting results

The results include several columns for transparency:

- **score**: Computed ranking incorporating source preferences, role
  boosts, and cross-source agreement
- **alignment_only**: `TRUE` for Wikidata terms (useful for crosswalks,
  not canonical modeling)
- **agreement_sources**: How many sources returned this term (higher =
  more confidence)
- **zooma_confidence/zooma_annotator**: ZOOMA annotation confidence when
  applicable

Filter out alignment-only terms when selecting canonical IRIs:

``` r

results <- find_terms("salmon", sources = c("ols", "nvs"))
canonical <- results[!results$alignment_only, ]
```

#### Debugging slow or empty searches

If a search returns unexpected results, check the diagnostics:

``` r

results <- find_terms("temperature", sources = c("ols", "nvs", "zooma"))
diagnostics <- attr(results, "diagnostics")
print(diagnostics)
# Shows: source, query, status (success/error), count, elapsed_secs, error message
```

2.  **Decide what kind of term it is**:
    - Use a controlled vocabulary term (SKOS concept) when the column
      holds one of a set of codes (species, run type, etc.).
    - Use an ontology class (OWL) when the column names a category you
      would treat as a type in data (for example, what kind of unit or
      entity each row is about).
3.  **Capture the link**: place the chosen URI in `term_iri` for the
    dictionary row. Mention it once; you do not need to repeat the same
    URI in multiple columns unless they genuinely mean different things.

``` r

dict <- infer_dictionary(
  df,
  dataset_id = "my-dataset-2026",
  table_id = "main-table"
)

dict$term_iri[dict$column_name == "SPAWN_EST"] <- "https://w3id.org/gcdfo/salmon#NaturalSpawnerCount"
```

### Working with semantic web terms (plain language)

- **IRI**: think of it as the web address that points to a formal
  definition. You only need to copy-paste it; you do not need to
  understand the underlying formal logic.
- **term_iri**: attaches the chosen web address to a column so the
  column is self-explanatory.
- **entity_iri** and **property_iri**: optional links for measurement
  columns. Use `property_iri` to specify what characteristic was
  measured (e.g., “count”), and `entity_iri` to specify what was
  measured (e.g., “spawning salmon”).
- **constraint_iri**: an optional I-ADOPT component that qualifies the
  measurement (e.g., “maximum”, “annual average”). Use it only when it
  adds clarity.

### When to skip linking

- If a column is purely administrative or custom to your survey, it is
  fine to leave `term_iri` blank and rely on your own
  `column_description`.
- If you cannot find a fitting term, note the idea in
  `gpt_proposed_terms.csv` and move on. You can revisit it later when
  new terms are published.

### Building vocabulary-aware code lists

- If a categorical column such as `SPECIES` uses a published vocabulary,
  add a `code_value` row that matches the vocabulary notation and
  include the `term_iri` for the concept.
- If you do not have a matching vocabulary, describe the codes clearly
  in `code_label` and `code_description` so reviewers do not have to
  guess.

### Exploring suggestions with metasalmon

[`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
can look at your dictionary and offer `term_iri`, `entity_iri`, or
I-ADOPT components based on the bundled catalog.

``` r

dict_suggested <- suggest_semantics(df, dict, sources = c("ols", "nvs"))
suggestions <- attr(dict_suggested, "semantic_suggestions")
head(suggestions)
```

It is safe to accept suggestions as a starting point, then tweak the
term labels to match your domain-specific language.

### Next steps

- See the “How It Fits Together” section in the README for context on
  how the dictionary, ontology, and GPT assistant team up.
- Follow the [Publishing Data
  Packages](https://dfo-pacific-science.github.io/metasalmon/articles/data-dictionary-publication.md)
  guide when you finalize your metadata before publishing.
- Try the [Using AI to Document Your
  Data](https://dfo-pacific-science.github.io/metasalmon/articles/gpt-collaboration.md)
  workflow to draft column descriptions or propose new terms when the
  catalog does not yet cover your concept.
