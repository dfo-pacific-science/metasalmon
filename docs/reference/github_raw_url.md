# Build a stable raw GitHub URL

Treats `path` as a path inside the target repository and returns the
corresponding `raw.githubusercontent.com` URL. If you already have a
full `http(s)` URL, it is returned unchanged after stripping any query
string.

## Usage

``` r
github_raw_url(path, ref = "main", repo = NULL)
```

## Arguments

- path:

  Character scalar path inside the repository, or a full URL.

- ref:

  Git reference (branch, tag, or commit SHA). Defaults to `"main"`.

- repo:

  Repository slug in `"owner/name"` form when `path` is not already a
  full URL. Required unless `path` is a full URL.

## Value

Character scalar raw URL.

## Examples

``` r
github_raw_url("data/gold/dimension_tables/dim_date.csv", repo = "owner/repo")
#> [1] "https://raw.githubusercontent.com/owner/repo/main/data/gold/dimension_tables/dim_date.csv"
github_raw_url("data/gold/dimension_tables/dim_date.csv", ref = "v0.3.0", repo = "owner/repo")
#> [1] "https://raw.githubusercontent.com/owner/repo/v0.3.0/data/gold/dimension_tables/dim_date.csv"
```
