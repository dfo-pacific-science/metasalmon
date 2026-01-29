# Read all CSV files from a GitHub directory

Lists all CSV files in a GitHub repository directory and reads them into
a named list of tibbles. Similar to using
[`dir()`](https://rdrr.io/r/base/list.files.html) with
[`lapply()`](https://rdrr.io/r/base/lapply.html) to read multiple local
CSV files.

## Usage

``` r
read_github_csv_dir(
  path,
  ref = "main",
  repo = NULL,
  token = NULL,
  pattern = "\\.csv$",
  ...
)
```

## Arguments

- path:

  Path to the directory inside the repository (e.g.,
  `"data/observations"`), or a full GitHub URL pointing to a directory.
  Trailing slashes are optional.

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

- pattern:

  Optional regular expression to filter CSV file names. Defaults to
  `"\\.csv$"` (files ending in `.csv`). Set to `NULL` to match all files
  in the directory (not just CSVs).

- ...:

  Additional arguments passed to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
  for each file, such as `col_types`, `skip`, `n_max`, etc.

## Value

A named list of tibbles, where names are the CSV file names (without the
`.csv` extension). Returns an empty list if no CSV files are found.

## Details

This function uses the GitHub API to list directory contents, filters
for CSV files, then reads each file using
[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md).
Authentication is required even for public repositories when using the
API.

Before using this function, run
[`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
once to configure authentication. For private repositories, your PAT
must have the `repo` scope.

For reproducible analyses, pin to a specific tag or commit SHA rather
than a branch name like `"main"`, since branch contents can change over
time.

**Manual alternative**: You can achieve the same result by using
[`gh::gh()`](https://gh.r-lib.org/reference/gh.html) to list directory
contents, filtering for CSV files, then looping through them with
[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md).
See the vignette for an example.

## See also

[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md)
for reading a single CSV file,
[`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
for authentication setup.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, set up authentication (run once)
ms_setup_github(repo = "myorg/myrepo")

# Read all CSV files from a directory
data_list <- read_github_csv_dir("data/observations", repo = "myorg/myrepo")

# Access individual data frames by name
observations <- data_list$observations
metadata <- data_list$metadata

# Pin to a release tag for reproducibility
data_v1 <- read_github_csv_dir(
  "data/observations",
  ref = "v1.0.0",
  repo = "myorg/myrepo"
)

# Custom pattern to match specific files
subset <- read_github_csv_dir(
  "data",
  repo = "myorg/myrepo",
  pattern = "^obs_.*\\.csv$"
)

# Pass arguments to read_csv for all files
data_typed <- read_github_csv_dir(
  "data/observations",
  repo = "myorg/myrepo",
  col_types = "ccin"
)
} # }
```
