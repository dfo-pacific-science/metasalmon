# Package index

## Dictionary Functions

Infer, validate, and apply semantic data dictionaries

- [`infer_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_dictionary.md)
  : Infer a starter dictionary from a data frame
- [`validate_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_dictionary.md)
  : Validate a salmon data dictionary
- [`apply_salmon_dictionary()`](https://dfo-pacific-science.github.io/metasalmon/reference/apply_salmon_dictionary.md)
  : Apply a salmon dictionary to a data frame
- [`apply_semantic_suggestions()`](https://dfo-pacific-science.github.io/metasalmon/reference/apply_semantic_suggestions.md)
  : Apply semantic suggestions into a dictionary

## Package Functions

Create and read Salmon Data Packages

- [`create_salmon_datapackage()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_salmon_datapackage.md)
  : Create a Salmon Data Package
- [`create_salmon_datapackage_from_data()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_salmon_datapackage_from_data.md)
  : Create a Salmon Data Package from raw tables
- [`infer_salmon_datapackage_artifacts()`](https://dfo-pacific-science.github.io/metasalmon/reference/infer_salmon_datapackage_artifacts.md)
  : Infer Salmon Data Package artifacts from resource tables
- [`read_salmon_datapackage()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_salmon_datapackage.md)
  : Read a Salmon Data Package

## Semantic Helpers

Semantic suggestion and vocabulary search capabilities

- [`suggest_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_semantics.md)
  : Suggest semantic annotations for a dictionary
- [`find_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/find_terms.md)
  : Find candidate terms across external vocabularies
- [`sources_for_role()`](https://dfo-pacific-science.github.io/metasalmon/reference/sources_for_role.md)
  : Get recommended sources for a given role
- [`deduplicate_proposed_terms()`](https://dfo-pacific-science.github.io/metasalmon/reference/deduplicate_proposed_terms.md)
  : Deduplicate proposed ontology terms

## Ontology + Validation

Fetch the ontology and validate semantic coverage

- [`fetch_salmon_ontology()`](https://dfo-pacific-science.github.io/metasalmon/reference/fetch_salmon_ontology.md)
  : Fetch the Salmon Domain Ontology with caching
- [`validate_semantics()`](https://dfo-pacific-science.github.io/metasalmon/reference/validate_semantics.md)
  : Validate semantics with graceful gap reporting
- [`suggest_facet_schemes()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_facet_schemes.md)
  : Suggest facet schemes for proposed terms

## Term Request Workflow

Detect missing SMN terms, route shared vs profile requests, and draft
GitHub issues

- [`detect_semantic_term_gaps()`](https://dfo-pacific-science.github.io/metasalmon/reference/detect_semantic_term_gaps.md)
  : Detect missing semantic terms that are not covered by SMN
- [`render_ontology_term_request()`](https://dfo-pacific-science.github.io/metasalmon/reference/render_ontology_term_request.md)
  : Render GitHub-ready ontology term request payloads
- [`submit_term_request_issues()`](https://dfo-pacific-science.github.io/metasalmon/reference/submit_term_request_issues.md)
  : Submit rendered ontology term requests as GitHub issues

## NuSEDS Helpers

Crosswalk legacy NuSEDS method labels to canonical method families

- [`nuseds_enumeration_method_crosswalk()`](https://dfo-pacific-science.github.io/metasalmon/reference/nuseds_enumeration_method_crosswalk.md)
  : NuSEDS enumeration method crosswalk
- [`nuseds_estimate_method_crosswalk()`](https://dfo-pacific-science.github.io/metasalmon/reference/nuseds_estimate_method_crosswalk.md)
  : NuSEDS estimate method crosswalk

## Darwin Core (DwC-DP)

Darwin Core Data Package mapping and export helpers

- [`suggest_dwc_mappings()`](https://dfo-pacific-science.github.io/metasalmon/reference/suggest_dwc_mappings.md)
  : Suggest Darwin Core Data Package mappings for dictionary columns
- [`dwc_dp_build_descriptor()`](https://dfo-pacific-science.github.io/metasalmon/reference/dwc_dp_build_descriptor.md)
  : Build a DwC-DP datapackage descriptor (export helper)

## Enterprise Data Hub (EDH)

HNAP-aware EDH XML export helper with legacy ISO 19139 fallback

- [`edh_build_iso19139_xml()`](https://dfo-pacific-science.github.io/metasalmon/reference/edh_build_iso19139_xml.md)
  : Build ISO 19139 or HNAP-aware metadata XML for DFO Enterprise Data
  Hub export

## GitHub Access

Authenticate once and read CSVs from private GitHub repositories

- [`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
  : Set up GitHub access for private repositories
- [`github_raw_url()`](https://dfo-pacific-science.github.io/metasalmon/reference/github_raw_url.md)
  : Build a stable raw GitHub URL
- [`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md)
  : Read a CSV from a GitHub repository
- [`read_github_csv_dir()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv_dir.md)
  : Read all CSV files from a GitHub directory

## ICES Vocabulary

Access ICES reference codes and vocabulary

- [`ices_vocab`](https://dfo-pacific-science.github.io/metasalmon/reference/ices_vocab.md)
  : ICES controlled vocabularies (code lists)
- [`ices_code_types()`](https://dfo-pacific-science.github.io/metasalmon/reference/ices_code_types.md)
  : List ICES code types
- [`ices_codes()`](https://dfo-pacific-science.github.io/metasalmon/reference/ices_codes.md)
  : List ICES codes for a code type
- [`ices_find_code_types()`](https://dfo-pacific-science.github.io/metasalmon/reference/ices_find_code_types.md)
  : Find ICES code types by text match
- [`ices_find_codes()`](https://dfo-pacific-science.github.io/metasalmon/reference/ices_find_codes.md)
  : Find ICES codes within a code type by text match
