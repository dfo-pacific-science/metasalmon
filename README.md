# metasalmon

<img src="man/figures/logo.png" alt="metasalmon logo" width="200"/>

## The Problem

You've spent years collecting salmon data. But when you try to share it:

- Colleagues ask "What does SPAWN_EST mean?"
- Combining datasets fails because everyone uses different column names
- Your future self opens old data and can't remember what the codes mean
- Other researchers can't use your data without emailing you for explanations

## The Solution

`metasalmon` wraps your salmon data with a **data dictionary** that travels with it—explaining every column, every code, and linking to standard scientific definitions. These definitions come from the [Salmon Domain Ontology](https://w3id.org/smn/) (shared layer) and the [DFO Salmon Ontology](https://w3id.org/gcdfo/salmon/) (DFO-specific layer), alongside other published controlled vocabularies, and the data is packaged according to the [Salmon Data Package Specification](https://github.com/dfo-pacific-science/smn-data-pkg/blob/main/SPECIFICATION.md). For extra help, our custom [Salmon Data Standardizer GPT](https://chatgpt.com/g/g-69375eab4f608191863e8c23313a6f9f-salmon-data-standardizer) can generate metadata drafts, salmon data packages, and guide your data dictionary creation in coordination with this R package.

**Integration context:** See the Salmon Data Integration System overview page (https://br-johnson.github.io/salmon-data-integration-system/) and walkthrough video (https://youtu.be/B0Zqac49zng?si=VmOjbfMDMd2xW9fH).

**Think of it like adding a detailed legend to your spreadsheet that never gets lost.**

## What You Get

| Your Data            | + metasalmon        | = Data Package            |
| -------------------- | ------------------- | ------------------------- |
| Raw CSV files        | Data dictionary     | Self-documenting dataset  |
| Cryptic column names | Clear descriptions  | Anyone can understand it  |
| Inconsistent codes   | Linked to standards | Works with other datasets |

## Quick Example

Install, run one function, then review in Excel.

```r
# Install from GitHub (recommended)
# install.packages("remotes")
# remotes::install_github("dfo-pacific-science/metasalmon")

library(metasalmon)

data_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
fraser_coho <- readr::read_csv(data_path, show_col_types = FALSE)

pkg_path <- create_sdp(
  fraser_coho,
  dataset_id = "fraser-coho-2024",
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

## Built-in NuSEDS Examples

`metasalmon` now ships two Fraser coho example tables so you can pick speed vs realism:

| File | Rows | Years | Use it when |
| --- | ---: | --- | --- |
| `nuseds-fraser-coho-sample.csv` | 30 | 1996-2024 | You want the smallest possible quickstart/demo dataset |
| `nuseds-fraser-coho-2023-2024.csv` | 173 | 2023-2024 | You want a fuller official multi-year slice for more realistic workflows |

```r
tiny_path <- system.file("extdata", "nuseds-fraser-coho-sample.csv", package = "metasalmon")
fuller_path <- system.file("extdata", "nuseds-fraser-coho-2023-2024.csv", package = "metasalmon")
provenance_path <- system.file("extdata", "example-data-README.md", package = "metasalmon")
```

The fuller example is derived from the Open Government Canada NuSEDS record using the Fraser and BC Interior workbook, filtered to `SPECIES == "Coho"` and `ANALYSIS_YR %in% c(2023, 2024)`. It keeps a compact analysis-friendly subset of columns and uses `NATURAL_ADULT_SPAWNERS` because `NATURAL_SPAWNERS_TOTAL` is blank for this official two-year slice. See `example-data-README.md` for the record/resource URLs, row counts, licensing note, and the `data-raw/` script that reproduces it.

To continue:

- [5-Minute Quickstart](articles/metasalmon.html) — create the full package with metadata and export it.
- [Publishing Data Packages](articles/data-dictionary-publication.html) — end-to-end publication checklist.
- [Linking to Standard Vocabularies](articles/reusing-standards-salmon-data-terms.html) — pick `term_iri`, `property_iri`, and `entity_iri` with confidence.

## Who Is This For?

| If you are...                           | Start here                                                                 |
| --------------------------------------- | -------------------------------------------------------------------------- |
| A biologist who wants to share data     | [5-Minute Quickstart](articles/metasalmon.html)                            |
| Curious how it works                    | [How It Fits Together](#how-it-fits-together)                              |
| A data steward standardizing datasets   | [Data Dictionary & Publication](articles/data-dictionary-publication.html) |
| Interested in AI-assisted documentation | [AI Assistance (Advanced)](articles/gpt-collaboration.html)                |
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
  +-- README-review.txt         # Step-by-step review checklist
  +-- semantic_suggestions.csv  # Review-only semantic suggestions (when present)
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
- Generate HNAP-aware EDH metadata XML for DFO Enterprise Data Hub upload workflows via `edh_build_iso19139_xml()` (with legacy ISO 19139 fallback still available).
- Role-aware vocabulary search with `find_terms()` and `sources_for_role()`:
  - Units: QUDT preferred, then NVS P06
  - Salmon-domain roles: shared SMN terms first, then GCDFO DFO-specific terms where needed
  - Entities/taxa: SMN and GCDFO first, then GBIF and WoRMS taxon resolvers
  - Properties/variables/methods: shared salmon-domain terms first, then broader ontology fallbacks
  - Cross-source agreement boosting for high-confidence matches
- Per-source diagnostics, scoring, and optional rerank explain why `find_terms()` matches rank where they do and expose failures, so you can tune role-aware queries with confidence.
- End-to-end semantic QA loop with `fetch_salmon_ontology()` + `validate_semantics()`, plus `deduplicate_proposed_terms()` to prevent term proliferation before opening ontology issues.
- NuSEDS method crosswalk helpers: `nuseds_enumeration_method_crosswalk()` and `nuseds_estimate_method_crosswalk()` for mapping legacy values to canonical method families.

## Getting Help

- [Frequently Asked Questions](articles/faq.html)
- [Glossary of Terms](articles/glossary.html)
- [Report a bug](https://github.com/dfo-pacific-science/metasalmon/issues)
- [Request a feature](https://github.com/dfo-pacific-science/metasalmon/issues)
- [Salmon Domain Ontology](https://w3id.org/smn/)
- [Salmon Data Package Specification](https://github.com/dfo-pacific-science/smn-data-pkg/blob/main/SPECIFICATION.md)

## How It Fits Together

`metasalmon` brings together four pieces: your raw data, the Salmon Data Package specification, the Salmon Domain Ontology (and other vocabularies), and the Salmon Data Standardizer GPT. When you finish the workflow, the dictionary, dataset/table metadata, and optional code lists are already aligned with the specification, which makes the package ready to publish. The ontology keeps the column meanings consistent, and the GPT assistant helps draft descriptions and term choices so you can close the loop without juggling multiple tools.

The high-level flow is:

- **Start here:** `create_sdp()` takes raw tables, infers the package metadata, writes a review-ready package, gives you a checklist, auto-fills top column/table semantic suggestions only where fields are blank, and keeps default code-level semantic seeding conservative by limiting it to factor and low-cardinality character source columns.
- **Advanced/manual path:** `write_salmon_datapackage()` is for cases where you already assembled `dataset.csv`, `tables.csv`, `column_dictionary.csv`, and optional `codes.csv` yourself.
- **Raw tables** lead into `metadata/column_dictionary.csv` (and `metadata/codes.csv` when there are categorical columns).
- **Dataset/table metadata** fill the required specification fields (title, description, creator, contact, etc.), so the package folder can be shared or uploaded.
- **The Salmon Domain Ontology and published vocabularies** supply `term_iri`/`entity_iri` links that describe what each column and row represents.
- **`write_salmon_datapackage()`** consumes the metadata, dictionary, codes, and data to write the files in the Salmon Data Package format, while the GPT assistant helps polish the metadata and suggests vocabulary links.

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
