# metasalmon

<img src="man/figures/logo.png" alt="metasalmon logo" width="200"/>

## The Problem

You've spent years collecting salmon data. But when you try to share it:

- Colleagues ask "What does SPAWN_EST mean?"
- Combining datasets fails because everyone uses different column names
- Your future self opens old data and can't remember what the codes mean
- Other researchers can't use your data without emailing you for explanations

## The Solution

`metasalmon` wraps your salmon data with a **data dictionary** that travels with it—explaining every column, every code, and linking to standard scientific definitions. These definitions come from the [Salmon Domain Ontology](https://w3id.org/smn/) (shared layer) and the [DFO Salmon Ontology](https://w3id.org/gcdfo/salmon/) (DFO-specific layer), alongside other published controlled vocabularies, and the data is packaged according to the [Salmon Data Package Specification](https://github.com/dfo-pacific-science/smn-data-pkg/blob/main/SPECIFICATION.md). The preferred review workflow now happens **inside the R package**: `metasalmon` can retrieve candidate terms, optionally ask an LLM to review them, write draft REVIEW-prefixed IRIs into the package, and then send you back to the created package for manual cleanup in Excel.

**Integration context:** See the Salmon Data Integration System overview page (https://br-johnson.github.io/salmon-data-integration-system/) and walkthrough video (https://youtu.be/B0Zqac49zng?si=VmOjbfMDMd2xW9fH).

**Think of it like adding a detailed legend to your spreadsheet that never gets lost.**

## What You Get

| Your Data            | + metasalmon        | = Data Package            |
| -------------------- | ------------------- | ------------------------- |
| Raw CSV files        | Data dictionary     | Self-documenting dataset  |
| Cryptic column names | Clear descriptions  | Anyone can understand it  |
| Inconsistent codes   | Linked to standards | Works with other datasets |

## Quick Example

Install, run one function on the bundled Fraser Coho 2023-2024 example (173 rows), then review in Excel.

```r
# Install from GitHub (recommended)
# install.packages("remotes")
# remotes::install_github("dfo-pacific-science/metasalmon")

library(metasalmon)

data_path <- system.file("extdata", "nuseds-fraser-coho-2023-2024.csv", package = "metasalmon")
fraser_coho <- readr::read_csv(data_path, show_col_types = FALSE)

pkg_path <- create_sdp(
  fraser_coho,
  dataset_id = "fraser-coho-2023-2024",
  table_id = "escapement",
  overwrite = FALSE
)

# Open pkg_path and review:
# - metadata/dataset.csv
# - metadata/tables.csv
# - metadata/column_dictionary.csv
# - metadata/codes.csv (if present)
# - data/*.csv
# - semantic_suggestions.csv (if present)
# - README-review.txt
```

`create_sdp()` is the main path. It writes the canonical `metadata/*.csv` files plus your `data/*.csv` tables, adds a review checklist, auto-applies top column-level and table observation-unit suggestions into blank fields, and seeds code-level semantic suggestions conservatively by default for factor and low-cardinality character source columns. Before SPSR/EDH upload, run `validate_salmon_datapackage(pkg_path, require_iris = TRUE)` to catch package/data/codes mismatches in one pass. In interactive use `create_sdp()` can also mention an available package update; set `check_updates = FALSE` to skip that check.

## Bundled NuSEDS Example

The default quickstart and get-started flow use `nuseds-fraser-coho-2023-2024.csv`, a 173-row Fraser coho slice derived from the official Open Government Canada Fraser and BC Interior workbook.

Open Government Canada record: <https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6>

A smaller `nuseds-fraser-coho-sample.csv` file is still bundled for tiny smoke tests, but it is no longer the default walkthrough dataset.

The package also now ships a matching starter dictionary for the fuller example (`system.file("extdata", "nuseds-fraser-coho-2023-2024-column_dictionary.csv", package = "metasalmon")`), which is useful when you want a ready-made context file for the package-native LLM review path.

See `example-data-README.md` for the record/resource URLs, row counts, licensing note, and the `data-raw/` script that reproduces the 2023-2024 example.

To continue:

- [5-Minute Quickstart](articles/metasalmon.html) — create the full package with metadata and export it.
- [Publishing Data Packages](articles/data-dictionary-publication.html) — end-to-end publication checklist.
- [Linking to Standard Vocabularies](articles/reusing-standards-salmon-data-terms.html) — pick `term_iri`, `property_iri`, and `entity_iri` with confidence.

## Package-native LLM semantic review (optional)

If you want an LLM to judge the shortlisted semantic matches directly from R, keep the deterministic search path and add an opt-in review pass:

```r
context_files <- c(
  file.path(pkg_path, "metadata", "column_dictionary.csv"),
  "README.md",
  "methods-report.pdf"
)

suggested <- suggest_semantics(
  df = fraser_coho,
  dict = infer_dictionary(fraser_coho, dataset_id = "fraser-coho-2023-2024", table_id = "escapement"),
  llm_assess = TRUE,
  llm_provider = "openrouter",
  llm_context_files = context_files
)

suggestions <- attr(suggested, "semantic_suggestions")
assessments <- attr(suggested, "semantic_llm_assessments")

# In create_sdp(..., llm_assess = TRUE), selected LLM-reviewed IRIs are written
# back into the package as REVIEW-prefixed draft values for manual cleanup,
# including table-level observation-unit selections in metadata/tables.csv.
```

This keeps `find_terms()` as the canonical candidate generator. The LLM judges the retrieved shortlist, can use local README/markdown/PDF context to make better calls, and can mark selected draft IRIs directly in the created package as `REVIEW: <iri>` so you can confirm or replace them in Excel. That includes selected table-level observation-unit matches written into `metadata/tables.csv`. Validation should only pass after the REVIEW prefix is removed. When you use `llm_provider = "openrouter"` without specifying `llm_model`, `metasalmon` now defaults to `openrouter/free`.

## Recommended workflow

For the current package-native review path, use this order:

1. Run `create_sdp(...)` to create the Salmon Data Package.
2. If you want semantic review, set `llm_assess = TRUE`.
3. Open the created package folder and review:
   - `metadata/*.csv`
   - `semantic_suggestions.csv`
   - `README-review.txt`
4. In Excel (or another spreadsheet editor), resolve every `REVIEW:`-prefixed IRI in the metadata files.
5. If you are preparing EDH metadata, regenerate the XML from the reviewed package with `write_edh_xml_from_sdp(pkg_path)` (the reviewed-package wrapper around the canonical `edh_build_hnap_xml()` builder). It now refuses to rebuild while `REVIEW:` markers or unresolved dataset/table placeholder text remain.
6. Re-run validation with `validate_salmon_datapackage(pkg_path, require_iris = TRUE)`.
7. Publish/share only after the `REVIEW:` markers are gone and validation passes.

In other words: **create -> review in Excel -> remove `REVIEW:` markers -> rebuild EDH XML if needed -> validate -> publish**.

## Who Is This For?

| If you are...                           | Start here                                                                 |
| --------------------------------------- | -------------------------------------------------------------------------- |
| A biologist who wants to share data     | [5-Minute Quickstart](articles/metasalmon.html)                            |
| Curious how it works                    | [How It Fits Together](#how-it-fits-together)                              |
| A data steward standardizing datasets   | [Data Dictionary & Publication](articles/data-dictionary-publication.html) |
| Reading CSVs from private GitHub repos  | [GitHub CSV Access](articles/github-csv-access.html)                       |

## Video Walkthrough

[Watch: Creating Your First Data Package](https://youtu.be/B0Zqac49zng?si=VmOjbfMDMd2xW9fH)

## Installation

```r
# Install from GitHub
install.packages("remotes")
remotes::install_github("dfo-pacific-science/metasalmon")
```

## What's In a Data Package?

When you create a package, you get a folder containing:

```
my-data-package/
  +-- README-review.txt         # Step-by-step review checklist for manual Excel cleanup
  +-- semantic_suggestions.csv  # Detailed semantic evidence + LLM review trail (when present)
  +-- datapackage.json          # Machine-readable export
  +-- metadata/
  |   +-- dataset.csv           # Dataset-level metadata (canonical)
  |   +-- tables.csv            # Table-level metadata and file paths
  |   +-- column_dictionary.csv # What each column means
  |   +-- codes.csv             # What each code value means (if applicable)
  +-- data/
      +-- escapement.csv        # Your data table(s)
```

Anyone opening this folder - whether a colleague, a reviewer, or your future self - can immediately understand your data. The `metadata/*.csv` files are the canonical package metadata; `datapackage.json` is a derived interoperability export. When you share the package, send the whole folder (or a zip of the whole folder), not just `datapackage.json`.

## Key Features

**For everyday use:**

- Automatically generate data dictionaries from your data frames
- Validate that your dictionary is complete and correct
- Create shareable packages that work across R, Python, and other tools
- Read CSVs directly from private GitHub repositories

**For data stewards (optional):**

- Link columns to standard DFO Salmon Ontology terms
- Add I-ADOPT measurement metadata (property, entity, unit, constraint)
- Use AI assistance to help write descriptions
- Suggest Darwin Core Data Package table/field mappings for biodiversity data
- Opt in to DwC-DP export hints via `suggest_semantics(..., include_dwc = TRUE)` while keeping the Salmon Data Package as the canonical deliverable.
- Generate HNAP-aware EDH metadata XML for DFO Enterprise Data Hub upload workflows via the canonical `edh_build_hnap_xml()` builder, the reviewed-package helper `write_edh_xml_from_sdp()`, or `create_sdp(..., include_edh_xml = TRUE)`.
- Role-aware vocabulary search with `find_terms()` and `sources_for_role()`:
  - Units: QUDT preferred, then NVS P06
  - Salmon-domain roles: shared SMN terms first, then GCDFO DFO-specific terms where needed
  - Entities/taxa: SMN and GCDFO first, then GBIF and WoRMS taxon resolvers
  - Properties/variables/methods: shared salmon-domain terms first, then broader ontology fallbacks
  - Cross-source agreement boosting for high-confidence matches
- Per-source diagnostics, scoring, and optional rerank explain why `find_terms()` matches rank where they do and expose failures, so you can tune role-aware queries with confidence.
- End-to-end semantic QA loop with `fetch_salmon_ontology()` + `validate_semantics()`, plus `deduplicate_proposed_terms()` to prevent term proliferation before opening ontology issues.
- Optional package-native LLM review for semantic suggestions: `suggest_semantics(..., llm_assess = TRUE)` can judge retrieved candidates directly in R, include local README/report context files, and use OpenAI-compatible providers such as OpenRouter (including model ids ending in `:free`).
- NuSEDS method crosswalk helpers: `nuseds_enumeration_method_crosswalk()` and `nuseds_estimate_method_crosswalk()` for mapping legacy values to canonical method families.

## Getting Help

- [Frequently Asked Questions](articles/faq.html)
- [Glossary of Terms](articles/glossary.html)
- [Report a bug](https://github.com/dfo-pacific-science/metasalmon/issues)
- [Request a feature](https://github.com/dfo-pacific-science/metasalmon/issues)
- [Salmon Domain Ontology](https://w3id.org/smn/)
- [Salmon Data Package Specification](https://github.com/dfo-pacific-science/smn-data-pkg/blob/main/SPECIFICATION.md)

## How It Fits Together

`metasalmon` brings together four pieces: your raw data, the Salmon Data Package specification, the Salmon Domain Ontology (and other vocabularies), optional in-package LLM review, and the review files written into the package itself. When you finish the workflow, the dictionary, dataset/table metadata, and optional code lists are already aligned with the specification, which makes the package ready to publish. The ontology keeps the column meanings consistent, and the package-native review workflow helps draft descriptions and term choices without forcing you to bounce back out to a separate ChatGPT export loop.

The high-level flow is:

- **Start here:** `create_sdp()` takes raw tables, infers the package metadata, writes a review-ready package, gives you a checklist, auto-fills top column/table semantic suggestions only where fields are blank, and keeps default code-level semantic seeding conservative by limiting it to factor and low-cardinality character source columns.
- **Advanced/manual path:** `write_salmon_datapackage()` is for cases where you already assembled `dataset.csv`, `tables.csv`, `column_dictionary.csv`, and optional `codes.csv` yourself.
- **Raw tables** lead into `metadata/column_dictionary.csv` (and `metadata/codes.csv` when there are categorical columns).
- **Dataset/table metadata** fill the required specification fields (title, description, creator, contact, etc.), so the package folder can be shared or uploaded.
- **The Salmon Domain Ontology and published vocabularies** supply `term_iri`/`entity_iri` links that describe what each column and row represents.
- **`write_salmon_datapackage()`** consumes the metadata, dictionary, codes, and data to write the files in the Salmon Data Package format; the preferred review loop is now the package itself plus `README-review.txt` / `semantic_suggestions.csv`, not an external ChatGPT export.

<script>
// Prevent Mermaid from auto-rendering before we prepare the code blocks.
window.mermaid = { startOnLoad: false };
</script>
<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
<script>
document.addEventListener("DOMContentLoaded", function () {
  if (typeof mermaid === "undefined") return;

  mermaid.initialize({ startOnLoad: false });

  // Render the "How It Fits Together" diagram.
  var src = document.getElementById("how-it-fits-together-mermaid");
  var target = document.getElementById("how-it-fits-together-diagram");
  if (!src || !target || typeof mermaid.render !== "function") return;

  var code = src.value || src.textContent || "";
  Promise.resolve(mermaid.render("how-it-fits-together", code))
    .then(function (res) {
      target.innerHTML = res.svg || res;
    })
    .catch(function () {
      // If rendering fails, leave the source text hidden and skip the diagram.
    });
});
</script>
<div id="how-it-fits-together-diagram"></div>
<textarea id="how-it-fits-together-mermaid" style="display:none">
flowchart LR
  RawData["Raw data tables"] --> Dict["metadata/column_dictionary.csv + metadata/codes.csv"]
  Dict["metadata/column_dictionary.csv + metadata/codes.csv"] --> Dataset["metadata/dataset.csv + metadata/tables.csv"]
  Dataset["metadata/dataset.csv + metadata/tables.csv"] --> Package["Package root + data/ + datapackage.json"]
  Dataset["metadata/dataset.csv + metadata/tables.csv"] --> Spec["Salmon Data Package specification"]
  Spec["Salmon Data Package specification"] --> Package["Package root + data/ + datapackage.json"]
  Ontology["Salmon Domain Ontology and published vocabularies"] --> Dict["metadata/column_dictionary.csv + metadata/codes.csv"]
  Ontology["Salmon Domain Ontology and published vocabularies"] --> Package["Package root + data/ + datapackage.json"]
  GPT["Salmon Data Standardizer GPT"] --> Dict["metadata/column_dictionary.csv + metadata/codes.csv"]
  GPT["Salmon Data Standardizer GPT"] --> Package["Package root + data/ + datapackage.json"]
</textarea>

## For Developers

<details>
<summary>Development setup and package structure</summary>

### Installation for Development

```r
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown",
                   "tibble", "readr", "jsonlite", "cli", "rlang", "dplyr",
                   "tidyr", "purrr", "withr", "frictionless"))
```

### Build and Check

```r
devtools::document()
devtools::test()
devtools::check()
devtools::build_vignettes()
pkgdown::build_site()
```

```bash
# Canonical source-tarball build path (writes into the repo root, not ../)
./scripts/build-package.sh
```

### Package Structure

- `R/`: Core functions for dictionary and package operations
- `inst/extdata/`: Example data files and templates
- `tests/testthat/`: Automated tests
- `vignettes/`: Long-form documentation
- `docs/`: pkgdown site output

### Salmon Domain Ontology

This package can link your data to the [Salmon Domain Ontology](https://w3id.org/smn/) for shared terms and to the [DFO Salmon Ontology](https://w3id.org/gcdfo/salmon/) for DFO-specific terms. Canonical IRIs are explicit: SMN uses `https://w3id.org/smn/<Term>` and GCDFO uses `https://w3id.org/gcdfo/salmon#<Term>`. metasalmon does not silently rewrite legacy `salmon:` IRIs.

See the [Reusing Standards for Salmon Data Terms](articles/reusing-standards-salmon-data-terms.html) guide for details.

</details>
