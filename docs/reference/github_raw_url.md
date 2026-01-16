# Build a stable raw GitHub URL

Constructs a `raw.githubusercontent.com` URL for a file in a GitHub
repository. This URL format is suitable for programmatic access and can
be used to document data sources. Note that the URL does not contain
authentication credentials; tokens are passed via HTTP headers by
[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md).

## Usage

``` r
github_raw_url(path, ref = "main", repo = NULL)
```

## Arguments

- path:

  Character scalar path inside the repository (e.g.,
  `"data/myfile.csv"`), or a full GitHub URL (blob or raw) which will be
  normalized.

- ref:

  Git reference: branch name, tag, or commit SHA. Defaults to `"main"`.
  For reproducible analyses, prefer tags or commit SHAs over branch
  names.

- repo:

  Repository slug in `"owner/name"` form. Required when `path` is a
  relative path; optional when `path` is already a full URL (the repo
  will be extracted from the URL).

## Value

Character scalar containing the raw GitHub URL.

## See also

[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md)
for reading the CSV content directly,
[`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
for authentication setup.

## Examples

``` r
# Build a raw URL for a file on main branch
github_raw_url("data/observations.csv", repo = "myorg/myrepo")
#> [1] "https://raw.githubusercontent.com/myorg/myrepo/main/data/observations.csv"

# Pin to a specific release tag for reproducibility
github_raw_url("data/observations.csv", ref = "v1.2.0", repo = "myorg/myrepo")
#> [1] "https://raw.githubusercontent.com/myorg/myrepo/v1.2.0/data/observations.csv"

# Pin to a specific commit SHA
github_raw_url("data/observations.csv", ref = "abc1234def", repo = "myorg/myrepo")
#> [1] "https://raw.githubusercontent.com/myorg/myrepo/abc1234def/data/observations.csv"
```
