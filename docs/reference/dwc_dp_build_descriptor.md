# Build a DwC-DP datapackage descriptor (export helper)

This is an opt-in export helper for DwC-DP (Darwin Core Data Package).
SDP remains canonical; DwC-DP is a derived/interoperability view.

## Usage

``` r
dwc_dp_build_descriptor(
  resources,
  profile_version = "master",
  profile_url = "http://rs.tdwg.org/dwc/dwc-dp",
  output_path = NULL,
  validate = FALSE,
  python = "python3"
)
```

## Arguments

- resources:

  Data frame with columns `name`, `path`, `schema` (schema is the DwC-DP
  table schema name, e.g., "occurrence", "event").

- profile_version:

  Git ref for DwC-DP schemas (default "master").

- profile_url:

  DwC-DP profile URL.

- output_path:

  Optional path to write the descriptor JSON.

- validate:

  If TRUE, attempt frictionless validation via python.

- python:

  Path to python executable (default "python3").

## Value

A list representing the descriptor (invisible); writes to `output_path`
when provided.
