# Read a CSV from a GitHub repository

Builds a stable raw URL (no token embedded), sends your GitHub PAT via
the `Authorization` header, and returns the CSV as a tibble. Accepts
either a path inside the repository or a full
GitHub/`raw.githubusercontent.com` URL.

## Usage

``` r
read_github_csv(path, ref = "main", repo = NULL, token = NULL, ...)
```

## Arguments

- path:

  Path inside the repository, or a full GitHub/raw URL.

- ref:

  Git reference (branch, tag, or commit SHA). Ignored when `path` is
  already a full URL.

- repo:

  Repository slug in `"owner/name"` form. Required unless `path` is a
  full URL.

- token:

  Optional GitHub PAT override. If `NULL`, falls back to
  [`gh::gh_token()`](https://gh.r-lib.org/reference/gh_token.html).

- ...:

  Passed through to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html).

## Value

Tibble read from the remote CSV.

## Examples

``` r
if (FALSE) { # \dontrun{
ms_setup_github()
dim_date <- read_github_csv("data/gold/dimension_tables/dim_date.csv", repo = "owner/repo")
dim_date_pinned <- read_github_csv(
  "data/gold/dimension_tables/dim_date.csv",
  ref = "v0.3.0",
  repo = "owner/repo"
)
} # }
```
