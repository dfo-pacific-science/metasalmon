# Set up GitHub access for private GitHub data

Checks for git, guides creation of a GitHub personal access token (PAT)
with `repo` scope, stores it via `gitcreds`, and optionally verifies
access to a target repository (defaults to the Qualark data repo). PAT
stands for personal access token, which is the GitHub credential used
for API requests.

## Usage

``` r
ms_setup_github(repo = "dfo-pacific-science/qualark-data")
```

## Arguments

- repo:

  Repository slug in `"owner/name"` form to verify access.

## Value

Invisibly returns the detected PAT.

## Examples

``` r
if (FALSE) { # \dontrun{
ms_setup_github()
} # }
```
