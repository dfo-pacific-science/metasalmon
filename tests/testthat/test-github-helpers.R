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

test_that("GitHub helper URL parsing rejects non-GitHub remote URLs", {
  expect_error(
    metasalmon:::ms_resolve_path(
      "https://example.com/data.csv",
      ref = "main",
      repo = NULL
    ),
    "URL inputs must use"
  )

  expect_error(
    metasalmon:::ms_resolve_dir_path(
      "https://example.com/data",
      ref = "main",
      repo = NULL
    ),
    "URL inputs must use"
  )
})

test_that("read_github_csv rejects non-GitHub URLs before any fetch", {
  expect_error(
    with_mocked_bindings(
      ms_github_get = function(url, token = NULL) {
        stop("network call should not happen")
      },
      read_github_csv("https://example.com/data.csv", token = "secret"),
      .package = "metasalmon"
    ),
    "URL inputs must use"
  )
})

test_that("read_github_csv can read public content without a token", {
  out <- with_mocked_bindings(
    ms_resolve_path = function(path, ref, repo) {
      list(
        url = "https://raw.githubusercontent.com/owner/repo/main/path/to/file.csv",
        repo = "owner/repo",
        ref = "main",
        path = "path/to/file.csv"
      )
    },
    ms_current_token = function() "",
    ms_github_get = function(url, token = NULL) {
      expect_equal(token, "")
      httr2::response(
        status_code = 200,
        url = url,
        body = charToRaw("a,b\n1,2\n")
      )
    },
    read_github_csv("path/to/file.csv", repo = "owner/repo"),
    .package = "metasalmon"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_equal(ncol(out), 2)
})

test_that("read_github_csv_dir can list and read public content without a token", {
  used_tokens <- character()
  out <- with_mocked_bindings(
    ms_resolve_dir_path = function(path, ref, repo) {
      list(repo = "owner/repo", ref = "main", path = "data")
    },
    ms_current_token = function() "",
    ms_github_list_contents = function(repo, path, ref, token = NULL) {
      expect_equal(token, "")
      list(
        list(type = "file", name = "a.csv"),
        list(type = "file", name = "b.txt"),
        list(type = "file", name = "c.csv")
      )
    },
    read_github_csv = function(path, ref = "main", repo = NULL, token = NULL, ...) {
      used_tokens <<- c(used_tokens, token)
      data.frame(x = 1, stringsAsFactors = FALSE)
    },
    read_github_csv_dir("data", repo = "owner/repo"),
    .package = "metasalmon"
  )

  expect_type(out, "list")
  expect_equal(names(out), c("a", "c"))
  expect_equal(used_tokens, c("", ""))
})

test_that("read_github_csv can read remote content with a token", {
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

test_that("read_github_csv without token can read a known public GitHub raw CSV", {
  skip_if_offline()

  df <- read_github_csv(
    "https://raw.githubusercontent.com/dfo-pacific-science/metasalmon/main/inst/extdata/nuseds-fraser-coho-sample.csv",
    token = "",
    progress = FALSE
  )

  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_gt(ncol(df), 0)
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

  tryCatch(
    gh::gh(sprintf("/repos/%s", repo), .token = token),
    error = function(e) {
      testthat::skip(paste("Cannot access", repo, "with current token:", conditionMessage(e)))
    }
  )

  tryCatch(
    {
      contents <- gh::gh(
        sprintf("/repos/%s/contents/%s", repo, dir_path),
        .token = token,
        ref = ref
      )
      if (!is.null(contents$type) && contents$type == "file") {
        testthat::skip(paste("Path", dir_path, "is a file, not a directory"))
      }
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

  data_list <- read_github_csv_dir(dir_path, ref = ref, repo = repo, token = token)

  expect_type(data_list, "list")
  expect_gt(length(data_list), 0)
  for (i in seq_along(data_list)) {
    expect_s3_class(data_list[[i]], "data.frame")
  }
  expect_true(all(nchar(names(data_list)) > 0))
})

test_that("read_github_csv_dir handles empty directories", {
  token <- metasalmon:::ms_current_token()
  skip_if(!nzchar(token), "No GitHub token configured; skipping empty directory test.")

  repo <- Sys.getenv("METASALMON_QUALARK_TEST_REPO", "dfo-pacific-science/qualark-data")
  ref <- Sys.getenv("METASALMON_QUALARK_TEST_REF", "main")

  expect_error(
    read_github_csv_dir(
      "nonexistent-directory-that-should-not-exist",
      ref = ref,
      repo = repo,
      token = token
    ),
    "not found|404",
    ignore.case = TRUE
  )
})
