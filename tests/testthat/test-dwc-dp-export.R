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
