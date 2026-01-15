test_that("github_raw_url builds stable raw URLs", {
  url <- github_raw_url("path/to/file.csv", repo = "owner/repo")
  expect_equal(
    url,
    "https://raw.githubusercontent.com/owner/repo/main/path/to/file.csv"
  )

  custom <- github_raw_url("path/to/file.csv", ref = "v1.0.0", repo = "owner/repo")
  expect_equal(
    custom,
    "https://raw.githubusercontent.com/owner/repo/v1.0.0/path/to/file.csv"
  )

  token_url <- "https://raw.githubusercontent.com/owner/repo/main/path/to/file.csv?token=SECRET"
  sanitized <- github_raw_url(token_url)
  expect_equal(sanitized, "https://raw.githubusercontent.com/owner/repo/main/path/to/file.csv")
})

test_that("GitHub path resolution handles blob and raw URLs", {
  blob <- metasalmon:::ms_resolve_path(
    "https://github.com/owner/repo/blob/main/path/to/file.csv",
    ref = "ignored",
    repo = NULL
  )
  expect_equal(
    blob$url,
    "https://raw.githubusercontent.com/owner/repo/main/path/to/file.csv"
  )
  expect_equal(blob$repo, "owner/repo")
  expect_equal(blob$ref, "main")

  raw <- metasalmon:::ms_resolve_path(
    "https://raw.githubusercontent.com/owner/repo/main/path/to/file.csv",
    ref = "ignored",
    repo = NULL
  )
  expect_equal(raw$url, "https://raw.githubusercontent.com/owner/repo/main/path/to/file.csv")
  expect_equal(raw$path, "path/to/file.csv")
})

test_that("read_github_csv errors early without a token", {
  expect_error(
    read_github_csv("data/gold/dimension_tables/dim_date.csv", repo = "owner/repo", token = ""),
    "No GitHub PAT found"
  )
})

test_that("read_github_csv can fetch when a token is configured", {
  token <- metasalmon:::ms_current_token()
  skip_if(!nzchar(token), "No GitHub token configured; skipping Qualark fetch test.")

  repo <- Sys.getenv("METASALMON_QUALARK_TEST_REPO", "dfo-pacific-science/qualark-data")
  path <- Sys.getenv("METASALMON_QUALARK_TEST_PATH", "data/gold/dimension_tables/dim_date.csv")
  ref <- Sys.getenv("METASALMON_QUALARK_TEST_REF", "main")

  tryCatch(
    gh::gh(sprintf("/repos/%s", repo), .token = token),
    error = function(e) {
      testthat::skip(paste("Cannot access", repo, "with current token:", conditionMessage(e)))
    }
  )

  tryCatch(
    gh::gh(
      sprintf("/repos/%s/contents/%s", repo, path),
      .token = token,
      ref = ref
    ),
    error = function(e) {
      testthat::skip(paste("Test CSV path not reachable:", conditionMessage(e)))
    }
  )

  df <- read_github_csv(path, ref = ref, repo = repo, token = token, progress = FALSE)

  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_gt(ncol(df), 0)
})
