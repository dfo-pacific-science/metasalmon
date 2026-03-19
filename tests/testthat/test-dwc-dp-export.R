test_that("dwc_dp_build_descriptor builds minimal descriptor", {
  resources <- tibble::tibble(
    name = c("occurrence", "event"),
    path = c("occurrence.csv", "event.csv"),
    schema = c("occurrence", "event")
  )

  desc <- dwc_dp_build_descriptor(resources, profile_version = "master", validate = FALSE)
  expect_equal(desc$profile, "http://rs.tdwg.org/dwc/dwc-dp")
  expect_equal(length(desc$resources), 2)
  expect_true(grepl("occurrence.json", desc$resources[[1]]$schema))
})

test_that("dwc frictionless validator runs without heredoc shell syntax", {
  py <- Sys.which("python3")
  testthat::skip_if(py == "", "python3 not available")

  descriptor <- list(
    profile = "http://rs.tdwg.org/dwc/dwc-dp",
    name = "dwc-dp-export",
    resources = list()
  )

  status <- .dwc_dp_validate_with_frictionless(descriptor, python = "python3")
  expect_true(is.numeric(status))
  expect_false(any(grepl("<<'PY'", deparse(body(.dwc_dp_validate_with_frictionless)), fixed = TRUE)))
})
