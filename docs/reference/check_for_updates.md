# Check whether a newer metasalmon release is available

Compares the installed package version with the latest GitHub release
for `dfo-pacific-science/metasalmon`.

## Usage

``` r
check_for_updates(
  repo = "dfo-pacific-science/metasalmon",
  current = utils::packageVersion("metasalmon"),
  timeout = 2,
  quiet = FALSE
)
```

## Arguments

- repo:

  GitHub repository in `"owner/name"` form. Defaults to the canonical
  `metasalmon` repository.

- current:

  Installed version to compare. Defaults to
  `utils::packageVersion("metasalmon")`.

- timeout:

  Number of seconds to wait for GitHub before giving up. Defaults to
  `2`.

- quiet:

  Logical; if `TRUE`, suppresses cli messages and only returns the
  result object.

## Value

Invisibly returns a list with class `"metasalmon_update_check"`.
Elements include `status`, `current_version`, `latest_version`,
`update_available`, `repo`, `release_tag`, `release_url`,
`install_command`, and `message`.

## Details

This function performs a network request only when you call it.
`metasalmon` does not check for updates automatically when the package
is attached.
[`create_sdp()`](https://dfo-pacific-science.github.io/metasalmon/reference/create_sdp.md)
can call it optionally when `check_updates = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
check_for_updates()
} # }
```
