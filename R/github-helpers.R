#' Build a stable raw GitHub URL
#'
#' Constructs a `raw.githubusercontent.com` URL for a file in a GitHub
#' repository. This URL format is suitable for programmatic access and can be
#' used to document data sources. Note that the URL does not contain
#' authentication credentials; tokens are passed via HTTP headers by
#' `read_github_csv()`.
#'
#' @param path Character scalar path inside the repository (e.g.,
#'   `"data/myfile.csv"`), or a full GitHub URL (blob or raw) which will be
#'   normalized.
#' @param ref Git reference: branch name, tag, or commit SHA. Defaults to
#'   `"main"`. For reproducible analyses, prefer tags or commit SHAs over
#'   branch names.
#' @param repo Repository slug in `"owner/name"` form. Required when `path` is
#'   a relative path; optional when `path` is already a full URL (the repo
#'   will be extracted from the URL).
#'
#' @return Character scalar containing the raw GitHub URL.
#'
#' @seealso [read_github_csv()] for reading the CSV content directly,
#'   [ms_setup_github()] for authentication setup.
#'
#' @export
#'
#' @examples
#' # Build a raw URL for a file on main branch
#' github_raw_url("data/observations.csv", repo = "myorg/myrepo")
#'
#' # Pin to a specific release tag for reproducibility
#' github_raw_url("data/observations.csv", ref = "v1.2.0", repo = "myorg/myrepo")
#'
#' # Pin to a specific commit SHA
#' github_raw_url("data/observations.csv", ref = "abc1234def", repo = "myorg/myrepo")
github_raw_url <- function(path, ref = "main", repo = NULL) {
  target <- ms_resolve_path(path, ref = ref, repo = repo)
  target$url
}

#' Set up GitHub access for private repositories
#'
#' Interactive setup wizard that configures authentication for reading CSV files
#' from private GitHub repositories. This function:
#'
#' 1. Checks that git is installed and available
#' 2. Guides creation of a GitHub Personal Access Token (PAT) with `repo` scope
#'    if one is not already stored
#' 3. Stores the PAT securely via `gitcreds` for future use
#' 4. Verifies that authentication works by testing access to a repository
#'
#' Run this function once before using `read_github_csv()` to access private
#' repositories. The stored PAT will be used automatically for subsequent
#' requests.
#'
#' @param repo Repository slug in `"owner/name"` form to verify access. Specify
#'   the private repository you intend to work with to confirm your PAT has
#'   the necessary permissions. Default is a test repository, but you should
#'   specify your target repository for verification.
#'
#' @return Invisibly returns the detected PAT.
#'
#' @details
#' A Personal Access Token (PAT) is a GitHub credential that allows API access.
#' The `repo` scope is required to read from private repositories. Tokens are
#' stored locally by the `gitcreds` package in your system's credential manager.
#'
#' If your organization uses Single Sign-On (SSO), you may need to authorize
#' your PAT for that organization at https://github.com/settings/tokens after
#' creating it.
#'
#' @seealso [read_github_csv()] for reading CSV files from GitHub,
#'   [github_raw_url()] for building raw GitHub URLs.
#'
#' @export
#'
#' @examples
#' \dontrun
#' # Basic setup (verifies against default test repository)
#' ms_setup_github()
#'
#' # Verify access to a specific private repository
#' ms_setup_github(repo = "your-org/your-private-repo")
#'
#' # After setup, you can read CSVs from private repos
#' data <- read_github_csv("path/to/file.csv", repo = "your-org/your-repo")
#' }
ms_setup_github <- function(repo = "dfo-pacific-science/qualark-data") {
  repo <- ms_normalize_repo(repo)

  cli::cli_h1("Setting up GitHub access")
  git <- Sys.which("git")
  if (!nzchar(git)) {
    cli::cli_abort("git is not installed or not on PATH. Install git, then retry.")
  }
  cli::cli_alert_success("git found at {.path {git}}")

  token <- ms_current_token()
  if (!nzchar(token)) {
    cli::cli_alert_info("No GitHub PAT detected; opening a browser to create one with {.code repo} scope.")
    usethis::create_github_token(scopes = "repo")
    cli::cli_alert_info("Storing the PAT in your git credential helper...")
    gitcreds::gitcreds_set()
    token <- ms_current_token()
  } else {
    cli::cli_alert_success("Found an existing GitHub PAT.")
  }

  if (!nzchar(token)) {
    cli::cli_abort("No GitHub PAT available. Run {.code gitcreds::gitcreds_set()} with a PAT, then rerun this setup.")
  }

  cli::cli_alert_info("Verifying access to {.val {repo}} ...")
  tryCatch(
    {
      gh::gh(sprintf("/repos/%s", repo), .token = token)
      cli::cli_alert_success("GitHub access verified and PAT stored.")
    },
    error = function(e) {
      headers <- tryCatch(e$response$headers, error = function(...) list())
      sso <- headers[["x-github-sso"]]
      if (!is.null(sso)) {
        cli::cli_abort(
          "Access blocked by org SSO. Re-authorize your PAT for this org at {.url https://github.com/settings/tokens}."
        )
      }
      cli::cli_abort("Unable to reach {.val {repo}}: {conditionMessage(e)}")
    }
  )

  invisible(token)
}

#' Read a CSV from a GitHub repository
#'
#' Reads a CSV file directly from a GitHub repository (public or private) and
#' returns it as a tibble. Authentication is handled via the GitHub PAT stored
#' by `ms_setup_github()`; the token is sent via HTTP headers, not embedded in
#' the URL.
#'
#' This function supports automatic retries with exponential backoff for
#' transient network errors.
#'
#' @param path Path to the CSV file inside the repository (e.g.,
#'   `"data/observations.csv"`), or a full GitHub URL (blob or raw format).
#' @param ref Git reference: branch name, tag, or commit SHA. Defaults to
#'   `"main"`. For reproducible analyses, prefer tags or commit SHAs.
#'   Ignored when `path` is already a full URL with a ref embedded.
#' @param repo Repository slug in `"owner/name"` form. Required when `path` is
#'   a relative path; optional when `path` is a full URL.
#' @param token Optional GitHub PAT override. If `NULL` (default), uses the
#'   token from `gh::gh_token()`, which is typically set by `ms_setup_github()`.
#' @param ... Additional arguments passed to `readr::read_csv()`, such as
#'   `col_types`, `skip`, `n_max`, etc.
#'
#' @return A tibble containing the CSV data.
#'
#' @details
#' Before using this function, run `ms_setup_github()` once to configure
#' authentication. For private repositories, your PAT must have the `repo`
#' scope.
#'
#' For reproducible analyses, pin to a specific tag or commit SHA rather than
#' a branch name like `"main"`, since branch contents can change over time.
#'
#' @seealso [ms_setup_github()] for authentication setup,
#'   [github_raw_url()] for getting the raw URL without fetching data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First, set up authentication (run once)
#' ms_setup_github(repo = "myorg/myrepo")
#'
#' # Read a CSV from the main branch
#' data <- read_github_csv("data/observations.csv", repo = "myorg/myrepo")
#'
#' # Pin to a release tag for reproducibility
#' data_v1 <- read_github_csv(
#'   "data/observations.csv",
#'   ref = "v1.0.0",
#'   repo = "myorg/myrepo"
#' )
#'
#' # Pin to a specific commit
#' data_exact <- read_github_csv(
#'   "data/observations.csv",
#'   ref = "a1b2c3d",
#'   repo = "myorg/myrepo"
#' )
#'
#' # Pass arguments to read_csv
#' data_typed <- read_github_csv(
#'   "data/observations.csv",
#'   repo = "myorg/myrepo",
#'   col_types = "ccin"
#' )
#'
#' # Read from a full GitHub URL
#' data_url <- read_github_csv(
#'   "https://github.com/myorg/myrepo/blob/main/data/observations.csv"
#' )
#' }
read_github_csv <- function(
    path,
    ref = "main",
    repo = NULL,
    token = NULL,
    ...
) {
  target <- ms_resolve_path(path, ref = ref, repo = repo)
  token <- if (is.null(token)) ms_current_token() else token

  if (!nzchar(token)) {
    cli::cli_abort("No GitHub PAT found. Run {.code metasalmon::ms_setup_github()} first.")
  }

  resp <- httr2::request(target$url) |>
    httr2::req_headers(Authorization = paste("token", token)) |>
    httr2::req_user_agent(ms_user_agent()) |>
    httr2::req_retry(backoff = ~2^.x, max_tries = 4) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status == 401) {
    cli::cli_abort("GitHub authentication failed. Run {.code metasalmon::ms_setup_github()} to refresh your PAT.")
  }

  if (status == 403) {
    sso <- httr2::resp_header(resp, "x-github-sso")
    if (!is.null(sso)) {
      cli::cli_abort(
        "Access blocked by org SSO. Re-authorize your PAT for this org at {.url https://github.com/settings/tokens}."
      )
    }
    cli::cli_abort("Access to {.val {target$repo}} was denied (status 403).")
  }

  if (status == 404) {
    cli::cli_abort(
      "Path {.path {target$path}} not found at ref {.val {target$ref}} in {.val {target$repo}}."
    )
  }

  httr2::resp_check_status(resp)
  readr::read_csv(I(httr2::resp_body_string(resp)), show_col_types = FALSE, ...)
}

ms_current_token <- function() {
  token <- suppressWarnings(
    tryCatch(gh::gh_token(), error = function(...) NA_character_)
  )
  if (is.null(token) || is.na(token)) "" else token
}

ms_normalize_repo <- function(repo) {
  if (!is.character(repo) || length(repo) != 1 || is.na(repo) || !grepl(".+/.+", repo)) {
    cli::cli_abort("{.arg repo} must be like {.code owner/name}.")
  }
  sub("^/", "", repo)
}

ms_resolve_path <- function(path, ref, repo) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || path == "") {
    cli::cli_abort("{.arg path} must be a non-empty string path or URL.")
  }

  if (!is.character(ref) || length(ref) != 1 || is.na(ref) || !nzchar(ref)) {
    cli::cli_abort("{.arg ref} must be a non-empty string reference.")
  }
  clean_ref <- ref
  clean_repo <- if (!is.null(repo)) ms_normalize_repo(repo) else NULL

  if (grepl("^https?://", path)) {
    clean_url <- sub("\\?.*$", "", path)

    blob_match <- regexec("^https?://github\\.com/([^/]+)/([^/]+)/blob/([^/]+)/(.+)$", clean_url)
    blob_parts <- regmatches(clean_url, blob_match)[[1]]
    if (length(blob_parts) == 5) {
      return(list(
        url = sprintf(
          "https://raw.githubusercontent.com/%s/%s/%s/%s",
          blob_parts[2],
          blob_parts[3],
          blob_parts[4],
          blob_parts[5]
        ),
        repo = paste(blob_parts[2], blob_parts[3], sep = "/"),
        ref = blob_parts[4],
        path = blob_parts[5]
      ))
    }

    raw_match <- regexec("^https?://raw\\.githubusercontent\\.com/([^/]+)/([^/]+)/([^/]+)/(.+)$", clean_url)
    raw_parts <- regmatches(clean_url, raw_match)[[1]]
    if (length(raw_parts) == 5) {
      return(list(
        url = clean_url,
        repo = paste(raw_parts[2], raw_parts[3], sep = "/"),
        ref = raw_parts[4],
        path = raw_parts[5]
      ))
    }

    return(list(url = clean_url, repo = clean_repo %||% "", ref = clean_ref, path = path))
  }

  if (is.null(clean_repo)) {
    cli::cli_abort("{.arg repo} is required when {.arg path} is not a full URL.")
  }

  clean_path <- sub("^/", "", path)
  list(
    url = sprintf("https://raw.githubusercontent.com/%s/%s/%s", clean_repo, clean_ref, clean_path),
    repo = clean_repo,
    ref = clean_ref,
    path = clean_path
  )
}

ms_user_agent <- function() {
  version <- tryCatch(as.character(utils::packageVersion("metasalmon")), error = function(...) "unknown")
  sprintf("metasalmon/%s", version)
}
