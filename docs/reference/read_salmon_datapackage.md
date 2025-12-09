# Read a Salmon Data Package

Loads a Salmon Data Package from disk, reading `datapackage.json` and
associated resource files, returning tibbles and metadata for analysis.

## Usage

``` r
read_salmon_datapackage(path)
```

## Arguments

- path:

  Character; path to directory containing `datapackage.json`

## Value

A list with components:

- `dataset`: Dataset metadata tibble

- `tables`: Table metadata tibble

- `dictionary`: Dictionary tibble (reconstructed from schema)

- `codes`: Codes tibble (if available)

- `resources`: Named list of data tibbles

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a package
pkg <- read_salmon_datapackage("path/to/package")
pkg$resources$main_table
} # }
```
