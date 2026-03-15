# 5-Minute Quickstart

## Installation

First, install metasalmon from GitHub:

``` r

# Install from GitHub (recommended)
install.packages("remotes")
remotes::install_github("dfo-pacific-science/metasalmon")
```

## What You’ll Learn

By the end of this guide, you’ll be able to:

- Turn your salmon data spreadsheet into a shareable “data package”
- Create a data dictionary that explains what each column means
- Share data that colleagues can immediately understand

## What You’ll Need

- R installed (version 4.4 or higher)
- Your salmon data as a CSV file (or use our example data)
- About 5 minutes

## Video Version

Prefer video? [Watch the
walkthrough](https://youtu.be/B0Zqac49zng?si=VmOjbfMDMd2xW9fH)

------------------------------------------------------------------------

## Step 1: Load Your Data

For the one-shot workflow, keep your inputs in a **named list** of data
frames (one per table):

``` r

library(metasalmon)
library(readr)

# Replace with your own CSVs or tables
catches <- read_csv("catches.csv", show_col_types = FALSE)
stations <- read_csv("stations.csv", show_col_types = FALSE)

# Named resource list for the package
resources <- list(
  catches = catches,
  stations = stations
)

head(resources$catches)
head(resources$stations)
```

> **Using your own data?** Keep the same names for each table’s key, and add
> as many tables as you need.

------------------------------------------------------------------------

## Step 2: Infer all metadata artifacts in one pass

`metasalmon` can infer multiple package artifacts from your tables at once:

- `dict` (`column_dictionary.csv` rows)
- `table_meta` (`tables.csv` rows)
- `codes` (`codes.csv` rows)
- `dataset_meta` (`dataset.csv` row)
- Semantic suggestions (optional, via `seed_semantics`)

``` r

artifacts <- infer_salmon_datapackage_artifacts(
  resources,
  dataset_id = "fraser-coho-2024",
  seed_semantics = TRUE,
  seed_verbose = TRUE
)

dict <- artifacts$dict
table_meta <- artifacts$table_meta
codes <- artifacts$codes
dataset_meta <- artifacts$dataset_meta
suggestions <- artifacts$semantic_suggestions

# Review what it created
print(dict)
head(suggestions)
```

What you get:

- `dict`: one row per column, with inferred `column_name`, `value_type`, and
  `column_role`
- `table_meta`: one row per input table
- `codes`: categorical terms inferred from character/factor columns
- `dataset_meta`: dataset-level row (including temporal/spatial keyword candidates)

The dictionary is still a starting point - review and refine it before
publishing.

### Need Help Finding Standard Terms?

Not sure what the official salmon data standard term is for a column?
The
[`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
function can automatically suggest standard terminology from scientific
vocabularies:

``` r

# Get semantic suggestions for your dictionary
# For salmon-domain roles, SMN shared terms are queried first, then GCDFO as a
# distinct DFO-specific source before OLS/NVS fallback sources.
dict_suggested <- suggest_semantics(
  df = resources[["catches"]],
  dict = dict
)

# View the suggestions
suggestions <- attr(dict_suggested, "semantic_suggestions")
head(suggestions)
```

This searches standard ontologies and vocabularies to find matching
terms for your columns, helping you link your data to recognized
scientific standards without making OLS/NVS the first stop for
salmon-domain matches.

> **Want faster results?** Use the [Salmon Data Standardizer
> GPT](https://chatgpt.com/g/g-69375eab4f608191863e8c23313a6f9f-salmon-data-standardizer)
> to get AI-powered suggestions for terminology, descriptions, and
> metadata. Just upload your dictionary and data sample!

``` r

# Optional: include DwC-DP export mappings alongside ontology suggestions
sem <- suggest_semantics(df = resources[["catches"]], dict)
sem_with_dwc <- suggest_semantics(df = resources[["catches"]], dict, include_dwc = TRUE)
```

DwC-DP mappings stay optional; keep SDP columns as canonical and use the
DwC view only when exporting to biodiversity tooling.

------------------------------------------------------------------------

## Step 3: Check Your Dictionary

Before packaging, let’s make sure the dictionary is valid:

``` r

validate_dictionary(dict)
```

**What happens**:

- **Green checkmarks** = everything looks good
- **Warnings** = optional improvements you could make
- **Errors** = things you need to fix before proceeding

If you see errors, the message will tell you what’s wrong. Common fixes:

``` r

# Example: Fix a column type that was guessed incorrectly
dict$value_type[dict$column_name == "YEAR"] <- "integer"

# Example: Add a better description
dict$column_description[dict$column_name == "POP_ID"] <-
  "Unique population identifier from the NuSEDS database"

# Validate again
validate_dictionary(dict)
```

------------------------------------------------------------------------

## Step 4: Describe Your Dataset (and tables)

You already have a full starting metadata set in
`artifacts$dataset_meta` and `artifacts$table_meta`; review and refine
before packaging.

``` r

# Existing inferred metadata
print(dataset_meta)
print(table_meta)

# Edit fields that are still too vague or missing
# For example:

dataset_meta$title <- "Fraser River Coho Escapement Data"
dataset_meta$description <- "Sample escapement monitoring data for coho salmon in PFMA 29"
dataset_meta$contact_name <- "Your Name"
dataset_meta$contact_email <- "your.email@dfo-mpo.gc.ca"
dataset_meta$license <- "Open Government License - Canada"

# Table metadata is still one row per table id
# Set explicit labels or file names if needed.
table_meta$file_name <- c("catches.csv", "stations.csv")
table_meta$table_label <- c("Catches", "Station Lookups")
```

------------------------------------------------------------------------

## Step 5: Create Your Data Package

Bundle everything together into a shareable folder:

``` r

# Optional: true one-shot path (recommended for first pass)
pkg_path <- create_salmon_datapackage_from_data(
  resources,
  path = "my-first-package",
  dataset_id = "fraser-coho-2024",
  seed_semantics = TRUE,
  overwrite = TRUE
)

# If you prefer explicit control, keep manual two-step:
# pkg_path <- create_salmon_datapackage(
#   resources = resources,
#   dataset_meta = dataset_meta,
#   table_meta = table_meta,
#   dict = dict,
#   codes = codes,
#   path = "my-first-package",
#   overwrite = TRUE
# )

# See what was created
list.files(pkg_path)
```

**What you get**: A folder called `my-first-package/` containing:

| File                    | Purpose                   |
|-------------------------|---------------------------|
| `catches.csv`, `stations.csv` | Your source tables        |
| `column_dictionary.csv` | What each column means    |
| `tables.csv`            | Table-level metadata      |
| `codes.csv`             | Candidate code label mappings |
| `dataset.csv`           | Dataset-level information |
| `datapackage.json`      | Machine-readable metadata |



## Step 5b (Optional): Export EDH XML metadata

If your workflow includes DFO Enterprise Data Hub / GeoNetwork upload,
the default
[`edh_build_iso19139_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_iso19139_xml.md)
path now writes the richer HNAP-aware EDH XML export from
`dataset_meta`. The older compact ISO 19139 shape is still available as
an explicit fallback.

``` r

edh_hnap_xml_path <- file.path(pkg_path, "metadata-edh-hnap.xml")
edh_build_iso19139_xml(dataset_meta, output_path = edh_hnap_xml_path)

edh_iso_xml_path <- file.path(pkg_path, "metadata-iso19139.xml")
edh_build_iso19139_xml(
  dataset_meta,
  output_path = edh_iso_xml_path,
  profile = "iso19139"
)

file.exists(edh_hnap_xml_path)
file.exists(edh_iso_xml_path)
```

The default HNAP-aware path adds EDH-oriented structure like
maintenance/status, legal constraints, optional download/distribution
metadata, reference system info, bounding boxes, deterministic
identifiers, and bilingual locale scaffolding. Use
`profile = "iso19139"` only when you specifically need that smaller
fallback export.

Validate and enrich either XML output against your local EDH profile
before production upload.

------------------------------------------------------------------------

## Step 6: Share It!

Your data package is ready. You can:

- **Email the folder** to a colleague (zip it first)
- **Upload to a data repository** like Zenodo or CIOOS
- **Archive it** for your future self
- **Include it** in a research compendium

When someone opens your package, they’ll find not just data, but
complete documentation explaining what every column means.

------------------------------------------------------------------------

## Reading a Package Back

Later, you (or a colleague) can load the package back into R:

``` r

# Read the package
pkg <- read_salmon_datapackage(pkg_path)

# What's inside?
names(pkg)

# Access the components
pkg$dataset      # Dataset metadata
pkg$tables       # Table metadata
pkg$dictionary   # Column descriptions
pkg$resources    # Your actual data

# Get your data back as a tibble
head(pkg$resources$escapement)
```

------------------------------------------------------------------------

## What’s Next?

You’ve created your first Salmon Data Package! Here are some ways to go
deeper:

- **[Using AI to Document Your
  Data](https://dfo-pacific-science.github.io/metasalmon/articles/gpt-collaboration.md)** -
  Use the Salmon Data Standardizer GPT to get AI-powered suggestions for
  terminology, descriptions, and metadata
- **[Publishing Data
  Packages](https://dfo-pacific-science.github.io/metasalmon/articles/data-dictionary-publication.md)** -
  More control over metadata and publishing
- **[Linking to Standard
  Vocabularies](https://dfo-pacific-science.github.io/metasalmon/articles/reusing-standards-salmon-data-terms.md)** -
  Connect your data to scientific standards
- **[Accessing Data from
  GitHub](https://dfo-pacific-science.github.io/metasalmon/articles/github-csv-access.md)** -
  Read CSVs from private repositories
- **[Glossary of
  Terms](https://dfo-pacific-science.github.io/metasalmon/articles/glossary.md)** -
  Definitions of technical terms
- **[FAQ](https://dfo-pacific-science.github.io/metasalmon/articles/faq.md)** -
  Common questions and troubleshooting

------------------------------------------------------------------------

## Troubleshooting

### “validate_dictionary() shows errors”

This usually means a column type was guessed incorrectly. Check the
error message and fix:

``` r

# See what types are valid
# string, integer, number, boolean, date, datetime

# Fix a specific column
dict$value_type[dict$column_name == "PROBLEM_COLUMN"] <- "string"
```

### “Column not found in dictionary”

Make sure your `table_id` in `table_meta` matches the `table_id` you
used in
[`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md).

### “I don’t understand what a field means”

See the
[Glossary](https://dfo-pacific-science.github.io/metasalmon/articles/glossary.md)
for plain-English definitions of terms like “column_role”, “value_type”,
etc.

### “I want to add better descriptions”

Edit the dictionary directly before creating the package:

``` r

# View and edit in RStudio
View(dict)

# Or edit programmatically
dict$column_description[dict$column_name == "MAX_ESTIMATE"] <-
  "Maximum escapement estimate for the population in a given year"
dict$column_label[dict$column_name == "MAX_ESTIMATE"] <-
  "Maximum Estimate"
```

### Still stuck?

- [Report a
  bug](https://github.com/dfo-pacific-science/metasalmon/issues)
- [Ask a
  question](https://github.com/dfo-pacific-science/metasalmon/issues)
