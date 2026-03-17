test_that("check_for_updates reports when a newer release is available", {
  result <- with_mocked_bindings(
    ms_fetch_latest_release = function(repo, timeout = 2) {
      list(
        ok = TRUE,
        version = "0.0.14",
        tag_name = "v0.0.14",
        html_url = "https://github.com/dfo-pacific-science/metasalmon/releases/tag/v0.0.14",
        message = "metasalmon 0.0.14"
      )
    },
    check_for_updates(current = "0.0.13", quiet = TRUE)
  )

  expect_s3_class(result, "metasalmon_update_check")
  expect_identical(result$status, "update_available")
  expect_true(result$update_available)
  expect_identical(result$current_version, "0.0.13")
  expect_identical(result$latest_version, "0.0.14")
  expect_identical(result$install_command, "remotes::install_github('dfo-pacific-science/metasalmon')")
})

test_that("check_for_updates reports when installed version is current", {
  result <- with_mocked_bindings(
    ms_fetch_latest_release = function(repo, timeout = 2) {
      list(
        ok = TRUE,
        version = "0.0.13",
        tag_name = "v0.0.13",
        html_url = "https://github.com/dfo-pacific-science/metasalmon/releases/tag/v0.0.13",
        message = "metasalmon 0.0.13"
      )
    },
    check_for_updates(current = "0.0.13", quiet = TRUE)
  )

  expect_identical(result$status, "up_to_date")
  expect_false(result$update_available)
})

test_that("check_for_updates reports when installed version is ahead of release", {
  result <- with_mocked_bindings(
    ms_fetch_latest_release = function(repo, timeout = 2) {
      list(
        ok = TRUE,
        version = "0.0.13",
        tag_name = "v0.0.13",
        html_url = "https://github.com/dfo-pacific-science/metasalmon/releases/tag/v0.0.13",
        message = "metasalmon 0.0.13"
      )
    },
    check_for_updates(current = "0.0.13.9000", quiet = TRUE)
  )

  expect_identical(result$status, "development_ahead")
  expect_false(result$update_available)
})

test_that("check_for_updates returns unavailable result when release lookup fails", {
  result <- with_mocked_bindings(
    ms_fetch_latest_release = function(repo, timeout = 2) {
      list(ok = FALSE, message = "GitHub API rate limit reached. Try again later.")
    },
    check_for_updates(current = "0.0.13", quiet = TRUE)
  )

  expect_identical(result$status, "unavailable")
  expect_true(is.na(result$update_available))
  expect_true(is.na(result$latest_version))
  expect_match(result$message, "rate limit", ignore.case = TRUE)
})

test_that("version check helpers normalize release tags and validate inputs", {
  expect_identical(metasalmon:::ms_normalize_release_version("v0.0.13"), "0.0.13")
  expect_identical(metasalmon:::ms_normalize_release_version(" V0.0.14 "), "0.0.14")
  expect_identical(metasalmon:::ms_normalize_current_version(package_version("0.0.13")), "0.0.13")
  expect_error(check_for_updates(timeout = 0, quiet = TRUE), "positive number")
  expect_error(metasalmon:::ms_normalize_current_version(character()), "single package version")
})
