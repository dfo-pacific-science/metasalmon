test_that("github_raw_url builds raw URL from repo/path/ref", {
  expect_equal(
    github_raw_url("data/observations.csv", ref = "v1.2.0", repo = "myorg/myrepo"),
    "https://raw.githubusercontent.com/myorg/myrepo/v1.2.0/data/observations.csv"
  )
})

test_that("github_raw_url normalizes blob GitHub URL", {
  expect_equal(
    github_raw_url("https://github.com/myorg/myrepo/blob/main/data/observations.csv"),
    "https://raw.githubusercontent.com/myorg/myrepo/main/data/observations.csv"
  )
})

test_that("github_raw_url rejects non-GitHub URLs", {
  expect_error(
    github_raw_url("https://example.com/data.csv"),
    "URL inputs must use"
  )
})
