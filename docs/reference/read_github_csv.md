# Read a CSV from a GitHub repository

Reads a CSV file directly from a GitHub repository (public or private)
and returns it as a tibble. Authentication is handled via the GitHub PAT
stored by
[`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md);
the token is sent via HTTP headers, not embedded in the URL.

## Usage

``` r
read_github_csv(path, ref = "main", repo = NULL, token = NULL, ...)
```

## Arguments

- path:

  Path to the CSV file inside the repository (e.g.,
  `"data/observations.csv"`), or a full GitHub URL (blob or raw format).

- ref:

  Git reference: branch name, tag, or commit SHA. Defaults to `"main"`.
  For reproducible analyses, prefer tags or commit SHAs. Ignored when
  `path` is already a full URL with a ref embedded.

- repo:

  Repository slug in `"owner/name"` form. Required when `path` is a
  relative path; optional when `path` is a full URL.

- token:

  Optional GitHub PAT override. If `NULL` (default), uses the token from
  [`gh::gh_token()`](https://gh.r-lib.org/reference/gh_token.html),
  which is typically set by
  [`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md).

- ...:

  Additional arguments passed to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html),
  such as `col_types`, `skip`, `n_max`, etc.

## Value

A tibble containing the CSV data.

## Details

This function supports automatic retries with exponential backoff for
transient network errors.

Before using this function, run
[`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
once to configure authentication. For private repositories, your PAT
must have the `repo` scope.

For reproducible analyses, pin to a specific tag or commit SHA rather
than a branch name like `"main"`, since branch contents can change over
time.

## See also

[`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
for authentication setup,
[`github_raw_url()`](https://dfo-pacific-science.github.io/metasalmon/reference/github_raw_url.md)
for getting the raw URL without fetching data.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, set up authentication (run once)
ms_setup_github(repo = "myorg/myrepo")

# Read a CSV from the main branch
data <- read_github_csv("data/observations.csv", repo = "myorg/myrepo")

# Pin to a release tag for reproducibility
data_v1 <- read_github_csv(
  "data/observations.csv",
  ref = "v1.0.0",
  repo = "myorg/myrepo"
)

# Pin to a specific commit
data_exact <- read_github_csv(
  "data/observations.csv",
  ref = "a1b2c3d",
  repo = "myorg/myrepo"
)

# Pass arguments to read_csv
data_typed <- read_github_csv(
  "data/observations.csv",
  repo = "myorg/myrepo",
  col_types = "ccin"
)

# Read from a full GitHub URL
data_url <- read_github_csv(
  "https://github.com/myorg/myrepo/blob/main/data/observations.csv"
)
} # }
```
