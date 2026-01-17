# Reading CSVs from Private GitHub Repositories

## Overview

metasalmon includes helper functions for reading CSV files directly from
private GitHub repositories. This is useful when your data lives in a
private repo and you want to:

- Access data without manually downloading files
- Pin to specific versions (tags or commits) for reproducibility
- Share analysis scripts that automatically fetch the latest data

## One-Time Setup

Before reading from private repositories, you need to authenticate with
GitHub. The
[`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
function guides you through this process:

``` r

library(metasalmon)

# Run once to set up authentication
ms_setup_github()
```

This function will:

1.  **Check for git** - Verify that git is installed on your system
2.  **Create a PAT** - Open a browser to create a GitHub Personal Access
    Token (PAT) with `repo` scope if you don’t have one
3.  **Store the PAT** - Save your token securely using `gitcreds` so you
    don’t need to enter it repeatedly
4.  **Verify access** - Confirm that authentication works by testing
    against a repository

### Verifying Access to a Specific Repository

By default,
[`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
verifies access to a test repository. You can specify your own
repository to verify:

``` r

# Verify access to a specific private repository
ms_setup_github(repo = "your-org/your-private-repo")
```

### Troubleshooting Authentication

If you encounter authentication issues:

``` r

# Check if you have a stored PAT
gh::gh_token()

# Re-set your credentials if needed
gitcreds::gitcreds_set()

# Then run setup again
ms_setup_github(repo = "your-org/your-repo")
```

**Common issues:**

| Problem | Solution |
|----|----|
| “No GitHub PAT found” | Run [`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md) to create and store a token |
| “Access blocked by org SSO” | Re-authorize your PAT for the organization at <https://github.com/settings/tokens> |
| “Unable to reach repository” | Check the repo slug is correct (`owner/name` format) |

## Reading CSV Files

Once authenticated, use
[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md)
to read CSV files directly into R:

``` r

# Read a CSV from a private repository
my_data <- read_github_csv(
  path = "data/my-dataset.csv",
  repo = "your-org/your-repo"
)

# View the data
head(my_data)
```

### Specifying Branches, Tags, or Commits

By default, files are read from the `main` branch. You can specify a
different reference:

``` r

# Read from a specific branch
dev_data <- read_github_csv(
  path = "data/my-dataset.csv",
  ref = "develop",
  repo = "your-org/your-repo"
)

# Pin to a release tag for reproducibility
stable_data <- read_github_csv(
  path = "data/my-dataset.csv",
  ref = "v1.0.0",
  repo = "your-org/your-repo"
)

# Pin to a specific commit SHA for exact reproducibility
exact_data <- read_github_csv(
  path = "data/my-dataset.csv",
  ref = "abc1234",
  repo = "your-org/your-repo"
)
```

**Best practice**: For reproducible analyses, pin to a tag or commit
rather than a branch name. Branch names like `main` can change over
time.

### Passing Options to read_csv()

Additional arguments are passed through to
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html):

``` r

# Specify column types
typed_data <- read_github_csv(
  path = "data/my-dataset.csv",
  repo = "your-org/your-repo",
  col_types = "ccin"  # character, character, integer, number
)

# Skip rows or limit reading
partial_data <- read_github_csv(
  path = "data/my-dataset.csv",
  repo = "your-org/your-repo",
  skip = 1,
  n_max = 100
)
```

## Getting Raw URLs

Sometimes you need the raw GitHub URL rather than the data itself. Use
[`github_raw_url()`](https://dfo-pacific-science.github.io/metasalmon/reference/github_raw_url.md):

``` r

# Get the stable raw URL for a file
url <- github_raw_url(
  path = "data/my-dataset.csv",
  repo = "your-org/your-repo"
)

# Pin to a specific version
versioned_url <- github_raw_url(
  path = "data/my-dataset.csv",
  ref = "v1.0.0",
  repo = "your-org/your-repo"
)

# The URL can be used in documentation or shared with others
# (they'll still need authentication to access private repos)
print(versioned_url)
```

**Note**: The raw URL does not contain your token - authentication is
handled separately via headers when you use
[`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md).

## Working with Full URLs

If you already have a GitHub URL (blob or raw), you can pass it
directly:

``` r

# From a GitHub blob URL (the kind you see in the browser)
data1 <- read_github_csv(
  path = "https://github.com/your-org/your-repo/blob/main/data/file.csv"
)

# From a raw.githubusercontent.com URL
data2 <- read_github_csv(
  path = "https://raw.githubusercontent.com/your-org/your-repo/main/data/file.csv"
)
```

When using full URLs, the `repo` and `ref` parameters are extracted
automatically and don’t need to be specified.

## Example Workflow

Here’s a complete example workflow for a reproducible analysis:

``` r

library(metasalmon)

# First time only: set up authentication
# ms_setup_github(repo = "dfo-pacific-science/my-data-repo")

# Read the latest data from main branch
current_data <- read_github_csv(
  path = "data/escapement-2024.csv",
  repo = "dfo-pacific-science/my-data-repo"
)

# For a published analysis, pin to a specific version
# This ensures anyone running your script gets the exact same data
archived_data <- read_github_csv(
  path = "data/escapement-2024.csv",
  ref = "v2.1.0",  # Use a release tag
  repo = "dfo-pacific-science/my-data-repo"
)

# Document the data source in your analysis
cat("Data source:", github_raw_url(
  "data/escapement-2024.csv",
  ref = "v2.1.0",
  repo = "dfo-pacific-science/my-data-repo"
))
```

## Security Notes

- **Tokens are not embedded in URLs** - Your PAT is sent via HTTP
  headers, not in the URL itself
- **Tokens are stored locally** - The `gitcreds` package stores your
  token in your system’s credential manager
- **Don’t commit tokens** - Never include your PAT in scripts or
  version-controlled files
- **Review token scopes** - The `repo` scope is required for private
  repositories; consider using fine-grained PATs for additional security

## Function Reference

| Function | Purpose |
|----|----|
| [`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md) | One-time authentication setup |
| [`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md) | Read a CSV file from GitHub into R |
| [`github_raw_url()`](https://dfo-pacific-science.github.io/metasalmon/reference/github_raw_url.md) | Get the raw URL for a file (no data fetched) |

For detailed documentation, see:

- [`ms_setup_github()`](https://dfo-pacific-science.github.io/metasalmon/reference/ms_setup_github.md)
- [`read_github_csv()`](https://dfo-pacific-science.github.io/metasalmon/reference/read_github_csv.md)
- [`github_raw_url()`](https://dfo-pacific-science.github.io/metasalmon/reference/github_raw_url.md)
