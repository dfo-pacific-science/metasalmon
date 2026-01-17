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

find_terms("spawner count", sources = c("ols", "nvs"))
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
