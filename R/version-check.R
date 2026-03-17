#' Check whether a newer metasalmon release is available
#'
#' Compares the installed package version with the latest GitHub release for
#' `dfo-pacific-science/metasalmon`.
#'
#' This function performs a network request only when you call it. `metasalmon`
#' does not check for updates automatically when the package is attached.
#' `create_sdp()` can call it optionally when `check_updates = TRUE`.
#'
#' @param repo GitHub repository in `"owner/name"` form. Defaults to the
#'   canonical `metasalmon` repository.
#' @param current Installed version to compare. Defaults to
#'   `utils::packageVersion("metasalmon")`.
#' @param timeout Number of seconds to wait for GitHub before giving up.
#'   Defaults to `2`.
#' @param quiet Logical; if `TRUE`, suppresses cli messages and only returns the
#'   result object.
#'
#' @return Invisibly returns a list with class `"metasalmon_update_check"`.
#'   Elements include `status`, `current_version`, `latest_version`,
#'   `update_available`, `repo`, `release_tag`, `release_url`,
#'   `install_command`, and `message`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_for_updates()
#' }
check_for_updates <- function(
    repo = "dfo-pacific-science/metasalmon",
    current = utils::packageVersion("metasalmon"),
    timeout = 2,
    quiet = FALSE
) {
  repo <- ms_normalize_repo(repo)
  current_version <- ms_normalize_current_version(current)
  timeout <- ms_validate_update_timeout(timeout)
  install_command <- sprintf("remotes::install_github(%s)", shQuote(repo))

  latest <- ms_fetch_latest_release(repo = repo, timeout = timeout)
  if (!isTRUE(latest$ok)) {
    result <- structure(
      list(
        status = "unavailable",
        current_version = current_version,
        latest_version = NA_character_,
        update_available = NA,
        repo = repo,
        release_tag = NA_character_,
        release_url = NA_character_,
        install_command = install_command,
        message = latest$message %||% "Update check failed."
      ),
      class = "metasalmon_update_check"
    )

    if (!quiet) {
      cli::cli_alert_info(c(
        "Couldn't check for a newer {.pkg metasalmon} release right now.",
        "i" = result$message
      ))
    }

    return(invisible(result))
  }

  compare <- utils::compareVersion(current_version, latest$version)
  status <- if (compare < 0) {
    "update_available"
  } else if (compare == 0) {
    "up_to_date"
  } else {
    "development_ahead"
  }

  result <- structure(
    list(
      status = status,
      current_version = current_version,
      latest_version = latest$version,
      update_available = compare < 0,
      repo = repo,
      release_tag = latest$tag_name,
      release_url = latest$html_url,
      install_command = install_command,
      message = latest$message %||% NA_character_
    ),
    class = "metasalmon_update_check"
  )

  if (!quiet) {
    if (identical(status, "update_available")) {
      cli::cli_alert_warning(
        "A newer {.pkg metasalmon} release is available: {.val {current_version}} -> {.val {latest$version}}."
      )
      cli::cli_alert_info("Update with {.code {install_command}}.")
      cli::cli_alert_info("Release notes: {.url {latest$html_url}}")
    } else if (identical(status, "up_to_date")) {
      cli::cli_alert_success(
        "{.pkg metasalmon} {.val {current_version}} matches the latest GitHub release {.val {latest$version}}."
      )
    } else {
      cli::cli_alert_info(
        "Installed {.pkg metasalmon} {.val {current_version}} is newer than the latest GitHub release {.val {latest$version}}."
      )
      cli::cli_alert_info("You're probably on an unreleased development build.")
    }
  }

  invisible(result)
}

ms_fetch_latest_release <- function(repo, timeout = 2) {
  req <- httr2::request(sprintf("https://api.github.com/repos/%s/releases/latest", repo)) |>
    httr2::req_headers(
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28"
    ) |>
    httr2::req_user_agent(ms_user_agent()) |>
    httr2::req_timeout(seconds = timeout)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    return(list(ok = FALSE, message = conditionMessage(resp)))
  }

  status <- httr2::resp_status(resp)
  if (status == 404) {
    return(list(ok = FALSE, message = sprintf("No GitHub release metadata found for %s.", repo)))
  }

  if (status == 403) {
    remaining <- httr2::resp_header(resp, "x-ratelimit-remaining") %||% NA_character_
    if (!is.na(remaining) && identical(remaining, "0")) {
      return(list(ok = FALSE, message = "GitHub API rate limit reached. Try again later."))
    }
    return(list(ok = FALSE, message = "GitHub denied the update check request (status 403)."))
  }

  if (status >= 400) {
    return(list(ok = FALSE, message = sprintf("GitHub returned status %s.", status)))
  }

  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    error = function(e) e
  )

  if (inherits(body, "error")) {
    return(list(ok = FALSE, message = "Couldn't parse GitHub release metadata."))
  }

  tag_name <- body$tag_name %||% ""
  version <- ms_normalize_release_version(tag_name)
  if (!nzchar(version)) {
    return(list(ok = FALSE, message = "Latest GitHub release did not include a usable version tag."))
  }

  list(
    ok = TRUE,
    version = version,
    tag_name = tag_name,
    html_url = body$html_url %||% NA_character_,
    message = body$name %||% NA_character_
  )
}

ms_normalize_release_version <- function(tag_name) {
  if (!is.character(tag_name) || length(tag_name) != 1 || is.na(tag_name)) {
    return("")
  }

  sub("^[Vv]", "", trimws(tag_name))
}

ms_normalize_current_version <- function(current) {
  if (inherits(current, "package_version")) {
    return(as.character(current))
  }

  if (!is.character(current) || length(current) != 1 || is.na(current) || !nzchar(trimws(current))) {
    cli::cli_abort("{.arg current} must be a single package version string.")
  }

  trimws(current)
}

ms_validate_update_timeout <- function(timeout) {
  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || timeout <= 0) {
    cli::cli_abort("{.arg timeout} must be a single positive number of seconds.")
  }

  as.numeric(timeout)
}
