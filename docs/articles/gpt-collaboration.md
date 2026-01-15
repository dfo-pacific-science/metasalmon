# AI Assistance for Salmon Data Packages

## Overview

This vignette shows how to collaborate with GPT (or other LLMs) to
propose semantic data dictionaries, IRIs, code lists, and metadata for
your Salmon Data Packages. This workflow combines AI assistance with the
deterministic R functions in `metasalmon` for a powerful, reproducible
pipeline.

**See also:** - The README’s “How It Fits Together” section for the full
picture - [Data Dictionary &
Publication](https://dfo-pacific-science.github.io/metasalmon/articles/articles/data-dictionary-publication.md)
for the metadata and publication workflow - [Reusing Standards for
Salmon Data
Terms](https://dfo-pacific-science.github.io/metasalmon/articles/articles/reusing-standards-salmon-data-terms.md)
for the plain-language ontology guidance

**Canonical output contract:** Use `smn-gpt/SYSTEM-PROMPT.md` as the
authoritative format rules (CSV only).

## Safe data checklist

- Share 50–500 representative rows, not the full dataset.
- Include column summaries (types, missingness, and unique values for
  categoricals).
- Remove or mask sensitive identifiers before sharing.
- Attach codebooks or methods docs when available.
- Treat `dfo-salmon.ttl` as schema-only (vocabulary source, not data).

## Workflow Overview

1.  **Bootstrap**: Use
    [`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
    to create a starter dictionary from your data.
2.  **Prompt GPT**: Send the dictionary and data sample to GPT with a
    focused prompt.
3.  **Extract**: Copy GPT’s suggested IRIs, descriptions, and codes into
    your dictionary.
4.  **Validate**: Use
    [`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md)
    to ensure everything is correct.
5.  **Package**: Create your Salmon Data Package with enriched
    semantics.

## Step-by-Step Guide

### Step 1: Prepare Your Data and Starter Dictionary

``` r


library(metasalmon)
library(readr)

# Load the example NuSEDS Fraser River Coho data included in the metasalmon package
data_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
df <- read_csv(data_path, show_col_types = FALSE)

# Create starter dictionary
dict <- infer_dictionary(
  df,
  dataset_id = "nuseds-fraser-coho-2024",
  table_id = "escapement"
)

# Show a sample for GPT
head(df, 5)
str(df)

# Optional: extra summaries that help GPT map fields more accurately
summary(df)
colSums(is.na(df))
dict

# Optional: write small files to upload to GPT (often more reliable than copy/paste)
# CAUTION: Do not include any sensitive data in the files you upload to GPT
readr::write_csv(head(df, 500), "data-sample.csv")
readr::write_csv(dict, "dictionary.csv")
```

### Optional: Build a GPT Context Pack (Recommended)

GPT will do better when you give it machine-readable files (and the
exact schemas you need back), rather than pasted console prints.

If your GPT interface supports file uploads, consider uploading:

- Your inferred dictionary (`dictionary.csv`)
- A small sample of your data (`data-sample.csv`)
- The `metasalmon` schema templates: `dataset.csv`, `tables.csv`,
  `column_dictionary.csv`, `codes.csv`
- The latest DFO Salmon Ontology file (e.g., `dfo-salmon.ttl`) as a
  schema-only vocabulary source (not data)
- Any other vocabularies you rely on (e.g., units)
- Any methods / codebook documents that define how fields were collected
  and what values mean
- The ontology repository’s term request template (optional, but helps
  the model draft issues in the expected format):  
  <https://github.com/dfo-pacific-science/salmon-ontology/blob/main/.github/ISSUE_TEMPLATE/new-term-request.md>

### Step 2: Craft a GPT Prompt

Create a prompt that asks GPT to:

- Propose column descriptions
- Suggest IRIs from the DFO Salmon Ontology (or appropriate
  vocabularies)
- Identify categorical fields that need code lists
- Propose controlled vocabulary values
- Return results in a strict, copy/pasteable CSV format matching the
  `metasalmon` schemas (see `column_dictionary.csv` and `codes.csv`)

**Example prompt:**

    I'm creating a Salmon Data Package for coho escapement data. Here's my data sample and starter dictionary:

    Data sample (first 5 rows):
    [Paste head(df) output from above code chunk]

    Data structure summary:
    [Paste str(df) output from above code chunk]

    Optional summaries:
    [Paste summary(df) and colSums(is.na(df)) output]

    Dictionary:
    [Paste dict output from above code chunk]

    Please help me enrich this dictionary by:
    1. Writing clear, biologist-friendly descriptions for each column
    2. Suggesting IRIs from the DFO Salmon Ontology (or appropriate vocabularies) for:
       - term_iri: the IRI for this column (use a SKOS concept IRI for compound variables)
       - term_type: set to `skos_concept` or `owl_class` (leave NA if unknown)
       - unit_label / unit_iri: if applicable
       - For measurement columns, also provide `property_iri`, `entity_iri`, and optional `constraint_iri` / `method_iri`
       - If there is no reasonable match, tell me how to request a new term be added to the DFO salmon ontology
    3. Identifying which columns should have code lists (categorical fields)
    4. Proposing code values and labels for categorical fields

    Output requirements:
    - Return a CSV named `column_dictionary_gpt.csv` with the same columns as `dict` (do not rename keys or columns).
    - Use only valid `value_type` values: string, integer, number, boolean, date, datetime.
    - Use only valid `column_role` values: identifier, attribute, measurement, temporal, categorical. Use `measurement` **only** for variables you would analyze (e.g., counts, rates, lengths), and keep IDs, dates, sample labels, and descriptive text as identifier/temporal/attribute.
    - If you cannot find an exact IRI in the provided ontology file, leave it blank (NA) and propose a new term in a separate CSV named `gpt_proposed_terms.csv` with:
      - term_label
      - term_definition
      - definition_source_url (optional)
      - term_type (`skos_concept` | `owl_class` | `owl_object_property`)
      - suggested_parent_iri
      - suggested_relationships
      - notes
    - If categorical columns exist, return `codes.csv`; otherwise omit it.
    - Do not invent IRIs.

### Step 3: Extract GPT’s Suggestions

GPT will return enriched dictionary rows and possibly code lists. Save
the CSVs and load them in R:

``` r


# Option A (more deterministic): GPT returns a full CSV (`column_dictionary_gpt.csv`)
# and you merge by keys, only filling blanks.
dict_gpt <- readr::read_csv("column_dictionary_gpt.csv", show_col_types = FALSE)
dict <- dict |>
  dplyr::left_join(
    dict_gpt,
    by = c("dataset_id", "table_id", "column_name"),
    suffix = c("", ".gpt")
  ) |>
  dplyr::mutate(
    column_label = dplyr::coalesce(column_label, `column_label.gpt`),
    column_description = dplyr::coalesce(column_description, `column_description.gpt`),
    column_role = dplyr::coalesce(column_role, `column_role.gpt`),
    value_type = dplyr::coalesce(value_type, `value_type.gpt`),
    unit_label = dplyr::coalesce(unit_label, `unit_label.gpt`),
    unit_iri = dplyr::coalesce(unit_iri, `unit_iri.gpt`),
    term_iri = dplyr::coalesce(term_iri, `term_iri.gpt`),
    term_type = dplyr::coalesce(term_type, `term_type.gpt`),
    required = required | dplyr::coalesce(`required.gpt`, FALSE)
  ) |>
  dplyr::select(-dplyr::ends_with(".gpt"))

# Example: GPT suggests descriptions and IRIs
# You would copy GPT's suggestions here

# Update dictionary with GPT's suggestions using DFO Salmon Ontology IRIs
dict$column_description[dict$column_name == "SPECIES"] <- "Salmon species common name/code"
dict$column_role[dict$column_name == "SPECIES"] <- "categorical"

# Example: Link an escapement measurement column
dict$column_description[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "Estimated total natural-origin spawners"
dict$term_iri[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "<DFO variable concept IRI>"
dict$term_type[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "skos_concept"
dict$property_iri[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "https://qudt.org/vocab/quantitykind/NumberOfOrganisms"
dict$entity_iri[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "https://w3id.org/gcdfo/salmon#Stock"
dict$unit_iri[dict$column_name == "NATURAL_SPAWNERS_TOTAL"] <- "http://qudt.org/vocab/unit/Each"

# GPT might also suggest code lists
codes <- readr::read_csv("codes.csv", show_col_types = FALSE)
```

### Step 3b: Pull role-aware suggestions locally (I-ADOPT catalogue)

Before (or alongside) GPT, you can ask `metasalmon` to propose
candidates using the bundled I-ADOPT terminology catalogue:

``` r

# Role-aware suggestions; no network keys needed for OLS/NVS
dict_suggested <- suggest_semantics(
  df = df,
  dict = dict,
  sources = c("ols", "nvs")
)

suggestions <- attr(dict_suggested, "semantic_suggestions")
# Inspect by role
dplyr::count(suggestions, dictionary_role)
head(suggestions)

# Manually accept the top-ranked picks per role for a measurement column
nat_row <- dict$column_name == "NATURAL_SPAWNERS_TOTAL"
dict$term_iri[nat_row]      <- suggestions$iri[suggestions$dictionary_role == "variable"][1]
dict$property_iri[nat_row]  <- suggestions$iri[suggestions$dictionary_role == "property"][1]
dict$entity_iri[nat_row]    <- suggestions$iri[suggestions$dictionary_role == "entity"][1]
dict$unit_iri[nat_row]      <- suggestions$iri[suggestions$dictionary_role == "unit"][1]
```

`smn-gpt` uses the same I-ADOPT catalogue (symlinked into the repo) and
follows the same rules: measurement columns require `term_iri`,
`property_iri`, `entity_iri`, `unit_iri`, with optional `constraint_iri`
and `method_iri`.

### Step 4: Validate and Refine

Always validate GPT’s suggestions:

``` r


# Validate dictionary
validate_dictionary(dict, require_iris = FALSE)

# Optional: check that codes cover observed values (otherwise factor conversion will introduce NAs)
col <- "SPECIES"
observed <- unique(df[[col]])
observed <- observed[!is.na(observed)]
covered <- codes$code_value[codes$column_name == col]
missing_codes <- setdiff(observed, covered)
if (length(missing_codes) > 0) missing_codes

# Optional: apply dictionary + codes to catch type/factor issues early
df_transformed <- apply_salmon_dictionary(df, dict, codes = codes)

# Check for any issues GPT might have introduced
# (e.g., invalid IRIs, wrong types)
```

### Step 5: Create Your Package

Use the enriched dictionary and codes:

``` r


# Prepare metadata (you can also ask GPT to help draft this)
dataset_meta <- tibble::tibble(
  dataset_id = "nuseds-fraser-coho-2024",
  title = "NuSEDS Fraser River Coho Escapement Data",
  description = "Escapement monitoring data for coho salmon in PFMA 29",
  creator = "DFO",
  contact_name = "DFO Data Stewardship Unit",
  contact_email = "data.stewardship@example.org",
  license = "Open Government License - Canada"
)

table_meta <- tibble::tibble(
  dataset_id = "nuseds-fraser-coho-2024",
  table_id = "escapement",
  file_name = "escapement.csv",
  table_label = "Escapement Data",
  description = "Coho escapement counts and metadata",
  observation_unit = NA_character_,      # or a short label like "CU-year record"
  observation_unit_iri = NA_character_,  # optional class for the table’s unit-of-observation
  primary_key = "POP_ID"
)

# Create package
resources <- list(escapement = df)
pkg_path <- create_salmon_datapackage(
  resources,
  dataset_meta,
  table_meta,
  dict,
  codes = codes, # include only when categorical columns exist
  path = tempdir(),
  format = "csv",
  overwrite = TRUE
)
```

## Advanced: GPT Prompt Templates

### Template 1: Dictionary Enrichment

    I have a salmon monitoring dataset with the following columns: [list columns].

    For each column, please:
    1. Write a clear description (1-2 sentences)
    2. Suggest the appropriate DFO Salmon Ontology IRI from https://w3id.org/gcdfo/salmon# and put it in the right field:
       - term_iri for the best-matching term (use a SKOS concept for compound variables)
       - term_type: `skos_concept` when the match is a SKOS concept (a controlled vocabulary term); use `owl_class` only when the column represents a class/type rather than a variable
       - If there is no reasonable match, leave the IRI blank and draft a proposed term (see below)
    3. For measurement columns, suggest unit_label and unit_iri (if available) and include `property_iri`, `entity_iri`, and optional `constraint_iri` / `method_iri`. For categorical columns, keep `vocabulary_iri` in the codes.csv output (not in the column dictionary).

    Return as a CSV named `column_dictionary_gpt.csv` matching the column_dictionary.csv schema. Also return `gpt_proposed_terms.csv` with:
    - term_label
    - term_definition
    - definition_source_url (optional)
    - term_type: "skos_concept", "owl_class", or "owl_object_property"
    - suggested_parent_iri: a broader concept (for SKOS) or a superclass (for OWL classes)
    - suggested_relationships: broader/narrower/closeMatch/related (for SKOS) or subclass/sameAs/seeAlso (for OWL)
    - notes

### Template 2: Code List Generation

    I have a categorical column "[COLUMN_NAME]". Here's the output of:
    - str(df)
    - sort(unique(df$[COLUMN_NAME])) (or the top ~50 values)
    - dplyr::count(df, [COLUMN_NAME], sort = TRUE)

    Please:
    1. Create a controlled vocabulary (code list) with proper labels
    2. Suggest an IRI for each code value:
       - term_iri when the match is known; set term_type to `skos_concept` or `owl_class`
    3. If `term_iri` is a DFO SKOS concept and it has a `skos:notation`, set `code_value` to that notation
    4. Identify the vocabulary IRI (`vocabulary_iri`) when these values belong to a controlled vocabulary (often a SKOS concept scheme; otherwise leave NA)
    5. If there is no reasonable match, tell me how to request a new term be added to the DFO salmon ontology and draft a github issue according to the github issue template in the dfo-salmon repository

    Return as a CSV named `codes.csv` matching the codes.csv schema.

### Template 3: Metadata Generation

    I'm creating a Salmon Data Package for [replace this section with a description of the who, what, what, where, and why of your dataset using the microphone transcription option to provide as much detail as possible. Additionally consider attaching supporting documentation like a pdf or word doc about the methods].

    Please help me draft metadata for the dataset, providing values for each of the required fields in the Salmon Data Package dataset.csv file:

    - dataset_id
    - title
    - description
    - creator
    - contact_name
    - contact_email
    - license
    - temporal_start
    - temporal_end
    - spatial_extent
    - dataset_type
    - source_citation

    For each required field, provide a realistic value if sufficient information is available. Otherwise, ask for clarification. Return the result as a CSV named `dataset.csv` matching the dataset.csv schema.

## Handling Ontology Gaps (Important)

The DFO Salmon Ontology is still evolving, so it’s normal that many
terms you want will not exist yet.

When a specific term isn’t available:

- Prefer a broader existing IRI (a more general concept or superclass)
  when it is still semantically correct.
- Leave `term_iri` blank (`NA`) rather than inventing an IRI.
- Capture what you *wanted* as a proposed new term in
  `gpt_proposed_terms.csv` and include its parent relationship:
  - For SKOS concepts, use a broader concept (broader means “more
    general”).
  - For OWL classes, use a superclass (superclass means “a more general
    type”).
- Include `definition_source_url` and `notes` when available.
- Draft a GitHub issue for the ontology repository using the new-term
  template:  
  <https://github.com/dfo-pacific-science/salmon-ontology/blob/main/.github/ISSUE_TEMPLATE/new-term-request.md>
- Periodically refresh the ontology file in your Custom GPT so
  suggestions are based on the latest terms.

## Tips for Effective GPT Collaboration

1.  **Provide context**: Include sample data rows so GPT understands
    your domain.
2.  **Attach files when possible**: Upload your `dictionary.csv` and a
    small `data-sample.csv` rather than pasting printed tibbles.
3.  **Minimize sensitive data**: Use a representative sample and remove
    any sensitive identifiers before uploading.
4.  **Be specific**: Ask for IRIs from specific ontologies (DFO Salmon
    Ontology).
5.  **Validate everything**: Always run manually review and edit gpt
    suggestions and then run
    [`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md)
    after GPT suggestions.
6.  **Iterate**: Refine prompts based on GPT’s responses.
7.  **Keep deterministic**: Keep prompts in version control, ask for
    strict output (CSV only), prefer “merge by keys” workflows, and (if
    using an API) use a low temperature.
8.  **Expect ontology gaps**: If an IRI doesn’t exist yet, leave it
    blank and draft a term request (don’t invent IRIs).

## Automated Semantic Suggestion

The
[`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
function provides role-aware IRI suggestions using the bundled I-ADOPT
terminology catalogue and external ontology services (OLS, NVS,
BioPortal):

``` r

dict_suggested <- suggest_semantics(df, dict, sources = c("ols", "nvs"))
suggestions <- attr(dict_suggested, "semantic_suggestions")
```

This complements the GPT workflow by providing deterministic,
reproducible suggestions that you can review and merge into your
dictionary. See [Reusing Standards for Salmon Data
Terms](https://dfo-pacific-science.github.io/metasalmon/articles/articles/reusing-standards-salmon-data-terms.md)
for details on
[`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
and the I-ADOPT catalogue.

## Example: Complete Workflow

``` r


# 1. Bootstrap
dict <- infer_dictionary(df, dataset_id = "my-dataset", table_id = "my-table")

# 2. (Manual step: prompt GPT with dict and sample data)

# 3. Extract GPT suggestions (example)
dict$column_description <- c("Species name", "Population count", ...)  # From GPT
dict$term_iri <- c("<DFO variable concept IRI>", ...)  # From GPT
dict$term_type <- c("skos_concept", ...)  # From GPT

# 4. Validate
validate_dictionary(dict)

# 5. Create package
create_salmon_datapackage(resources, dataset_meta, table_meta, dict, ...)
```

## Resources

- **DFO Salmon Ontology**: <https://w3id.org/gcdfo/salmon>
  - Key classes: `ConservationUnit`, `Stock`, `BroodYear`, `CatchYear`
  - Key schemes: `WSPMetricScheme`, `WSPBiologicalStatusZoneScheme`,
    `SalmonOriginScheme`, `EnumerationMethodScheme`
  - Repository: <https://github.com/dfo-pacific-science/salmon-ontology>
- **Frictionless Data Package spec**:
  <https://specs.frictionlessdata.io/data-package/>
- **Schemas to upload to GPT** (included with the package):
  `system.file("extdata", "dataset.csv", package = "metasalmon")`,
  `system.file("extdata", "tables.csv", package = "metasalmon")`,
  `system.file("extdata", "column_dictionary.csv", package = "metasalmon")`,
  `system.file("extdata", "codes.csv", package = "metasalmon")`
- **Custom GPT prompt template** (included with the package):
  `system.file("extdata", "custom-gpt-prompt.md", package = "metasalmon")`
