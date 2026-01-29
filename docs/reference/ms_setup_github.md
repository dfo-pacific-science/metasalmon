# Set up GitHub access for private repositories

Interactive setup wizard that configures authentication for reading CSV
files from private GitHub repositories. This function:

## Usage

``` r
ms_setup_github(repo = "dfo-pacific-science/qualark-data")
```

## Arguments

- repo:

  Repository slug in `"owner/name"` form to verify access. Specify the
  private repository you intend to work with to confirm your PAT has the
  necessary permissions. Default is a test repository, but you should
  specify your target repository for verification.

## Value

Invisibly returns the detected PAT.

## Details

1.  Checks that git is installed and available

2.  Guides creation of a GitHub Personal Access Token (PAT) with `repo`
    scope if one is not already stored

3.  Stores the PAT securely via `gitcreds` for future use

4.  Verifies that authentication works by testing access to a repository

Run this function once before using
[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md)
to access private repositories. The stored PAT will be used
automatically for subsequent requests.

A Personal Access Token (PAT) is a GitHub credential that allows API
access. The `repo` scope is required to read from private repositories.
Tokens are stored locally by the `gitcreds` package in your system's
credential manager.

If your organization uses Single Sign-On (SSO), you may need to
authorize your PAT for that organization at
https://github.com/settings/tokens after creating it.

## See also

[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md)
for reading CSV files from GitHub,
[`github_raw_url()`](https://dfo-pacific-science.github.io/metasalmon/reference/github_raw_url.md)
for building raw GitHub URLs.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic setup (verifies against default test repository)
ms_setup_github()

# Verify access to a specific private repository
ms_setup_github(repo = "your-org/your-private-repo")

# After setup, you can read CSVs from private repos
data <- read_github_csv("path/to/file.csv", repo = "your-org/your-repo")
} # }
```
