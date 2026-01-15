#' Build a stable raw GitHub URL
#'
#' Treats `path` as a path inside the target repository and returns the
#' corresponding `raw.githubusercontent.com` URL. If you already have a full
#' `http(s)` URL, it is returned unchanged after stripping any query string.
#'
#' @param path Character scalar path inside the repository, or a full URL.
#' @param ref Git reference (branch, tag, or commit SHA). Defaults to `"main"`.
#' @param repo Repository slug in `"owner/name"` form when `path` is not already
#'   a full URL. Required unless `path` is a full URL.
#'
#' @return Character scalar raw URL.
#' @export
#'
#' @examples
#' github_raw_url("data/gold/dimension_tables/dim_date.csv", repo = "owner/repo")
#' github_raw_url("data/gold/dimension_tables/dim_date.csv", ref = "v0.3.0", repo = "owner/repo")
github_raw_url <- function(path, ref = "main", repo = NULL) {
  target <- ms_resolve_path(path, ref = ref, repo = repo)
  target$url
}

#' Set up GitHub access for private GitHub data
#'
#' Checks for git, guides creation of a GitHub personal access token (PAT) with
#' `repo` scope, stores it via `gitcreds`, and optionally verifies access to a
#' target repository (defaults to the Qualark data repo). PAT stands for
#' personal access token, which is the GitHub credential used for API requests.
#'
#' @param repo Repository slug in `"owner/name"` form to verify access.
#'
#' @return Invisibly returns the detected PAT.
#' @export
#'
#' @examples
#' \dontrun{
#' ms_setup_github()
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
#' Builds a stable raw URL (no token embedded), sends your GitHub PAT via the
#' `Authorization` header, and returns the CSV as a tibble. Accepts either a
#' path inside the repository or a full GitHub/`raw.githubusercontent.com` URL.
#'
#' @param path Path inside the repository, or a full GitHub/raw URL.
#' @param ref Git reference (branch, tag, or commit SHA). Ignored when `path` is
#'   already a full URL.
#' @param repo Repository slug in `"owner/name"` form. Required unless `path` is
#'   a full URL.
#' @param token Optional GitHub PAT override. If `NULL`, falls back to
#'   `gh::gh_token()`.
#' @param ... Passed through to `readr::read_csv()`.
#'
#' @return Tibble read from the remote CSV.
#' @export
#'
#' @examples
#' \dontrun{
#' ms_setup_github()
#' dim_date <- read_github_csv("data/gold/dimension_tables/dim_date.csv", repo = "owner/repo")
#' dim_date_pinned <- read_github_csv(
#'   "data/gold/dimension_tables/dim_date.csv",
#'   ref = "v0.3.0",
#'   repo = "owner/repo"
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
