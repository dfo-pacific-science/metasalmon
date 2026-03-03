test_that("nuseds_enumeration_method_crosswalk has required structure and key rows", {
  cross <- nuseds_enumeration_method_crosswalk()

  expect_true(is.data.frame(cross))
  expect_true(all(c(
    "nuseds_value",
    "method_family",
    "ontology_term",
    "notes"
  ) %in% names(cross)))

  expect_gt(nrow(cross), 0)
  expect_true(any(cross$nuseds_value == "Bank Walk"))
  expect_true(any(cross$method_family == "unknown"))
  expect_true(any(cross$method_family == "FS"))
})

test_that("nuseds_estimate_method_crosswalk has required structure and key rows", {
  cross <- nuseds_estimate_method_crosswalk()

  expect_true(is.data.frame(cross))
  expect_true(all(c(
    "nuseds_value",
    "method_family",
    "guidance_interpretation",
    "ontology_term",
    "notes"
  ) %in% names(cross)))

  expect_gt(nrow(cross), 0)
  expect_true(any(cross$nuseds_value == "Sonar-ARIS"))
  expect_true(any(cross$method_family == "depends"))
  expect_true(any(cross$method_family == "M"))
})
