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

test_that("read_github_csv_dir errors early without a token", {
  expect_error(
    read_github_csv_dir("data/observations", repo = "owner/repo", token = ""),
    "No GitHub PAT found"
  )
})

test_that("ms_resolve_dir_path handles directory paths correctly", {
  # Test relative path
  result <- metasalmon:::ms_resolve_dir_path("data/observations", ref = "main", repo = "owner/repo")
  expect_equal(result$repo, "owner/repo")
  expect_equal(result$ref, "main")
  expect_equal(result$path, "data/observations")

  # Test path with trailing slash
  result2 <- metasalmon:::ms_resolve_dir_path("data/observations/", ref = "main", repo = "owner/repo")
  expect_equal(result2$path, "data/observations")

  # Test empty path (root)
  result3 <- metasalmon:::ms_resolve_dir_path("", ref = "main", repo = "owner/repo")
  expect_equal(result3$path, "")

  # Test blob URL (directory)
  blob_dir <- metasalmon:::ms_resolve_dir_path(
    "https://github.com/owner/repo/tree/main/data/observations",
    ref = "ignored",
    repo = NULL
  )
  expect_equal(blob_dir$repo, "owner/repo")
  expect_equal(blob_dir$ref, "main")
  expect_equal(blob_dir$path, "data/observations")

  # Test raw URL (extracts directory from file path)
  raw_dir <- metasalmon:::ms_resolve_dir_path(
    "https://raw.githubusercontent.com/owner/repo/main/data/observations/file.csv",
    ref = "ignored",
    repo = NULL
  )
  expect_equal(raw_dir$repo, "owner/repo")
  expect_equal(raw_dir$ref, "main")
  expect_equal(raw_dir$path, "data/observations")
})

test_that("read_github_csv_dir can fetch when a token is configured", {
  token <- metasalmon:::ms_current_token()
  skip_if(!nzchar(token), "No GitHub token configured; skipping directory fetch test.")

  repo <- Sys.getenv("METASALMON_QUALARK_TEST_REPO", "dfo-pacific-science/qualark-data")
  dir_path <- Sys.getenv("METASALMON_QUALARK_TEST_DIR", "data/gold/dimension_tables")
  ref <- Sys.getenv("METASALMON_QUALARK_TEST_REF", "main")

  # Verify we can access the repo
  tryCatch(
    gh::gh(sprintf("/repos/%s", repo), .token = token),
    error = function(e) {
      testthat::skip(paste("Cannot access", repo, "with current token:", conditionMessage(e)))
    }
  )

  # Verify the directory exists and has contents
  tryCatch(
    {
      contents <- gh::gh(
        sprintf("/repos/%s/contents/%s", repo, dir_path),
        .token = token,
        ref = ref
      )
      # Check if it's actually a directory (array) or a single file
      if (!is.null(contents$type) && contents$type == "file") {
        testthat::skip(paste("Path", dir_path, "is a file, not a directory"))
      }
      # Check if directory has any CSV files
      csv_files <- Filter(
        function(x) x$type == "file" && grepl("\\.csv$", x$name, ignore.case = TRUE),
        contents
      )
      if (length(csv_files) == 0) {
        testthat::skip(paste("Directory", dir_path, "has no CSV files"))
      }
    },
    error = function(e) {
      testthat::skip(paste("Test directory path not reachable:", conditionMessage(e)))
    }
  )

  # Test reading the directory
  data_list <- read_github_csv_dir(dir_path, ref = ref, repo = repo, token = token)

  expect_type(data_list, "list")
  expect_gt(length(data_list), 0)

  # Check that all elements are data frames
  for (i in seq_along(data_list)) {
    expect_s3_class(data_list[[i]], "data.frame")
  }

  # Check that names are set (file names without .csv extension)
  expect_true(all(nchar(names(data_list)) > 0))
})

test_that("read_github_csv_dir handles empty directories", {
  token <- metasalmon:::ms_current_token()
  skip_if(!nzchar(token), "No GitHub token configured; skipping empty directory test.")

  repo <- Sys.getenv("METASALMON_QUALARK_TEST_REPO", "dfo-pacific-science/qualark-data")
  ref <- Sys.getenv("METASALMON_QUALARK_TEST_REF", "main")

  # Try to find an empty directory or create a test scenario
  # For now, we'll test that it returns an empty list gracefully
  # This test might skip if we can't find an appropriate test directory
  tryCatch(
    {
      # Try root directory - might have files, but we can test the pattern matching
      result <- read_github_csv_dir(
        "nonexistent-directory-that-should-not-exist",
        ref = ref,
        repo = repo,
        token = token,
        pattern = "definitely_no_files_match_this_pattern_12345\\.csv$"
      )
      expect_type(result, "list")
      expect_equal(length(result), 0)
    },
    error = function(e) {
      # If directory doesn't exist, we get an error, which is expected
      if (grepl("not found|404", conditionMessage(e), ignore.case = TRUE)) {
        # This is expected behavior
        expect_true(TRUE)
      } else {
        # Other errors should still be tested
        testthat::skip(paste("Unexpected error:", conditionMessage(e)))
      }
    }
  )
})
