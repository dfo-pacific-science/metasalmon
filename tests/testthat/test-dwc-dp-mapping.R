test_that("suggest_dwc_mappings returns DwC-DP field suggestions", {
  dict <- tibble::tibble(
    column_name = c("event_date", "decimal_latitude", "scientific_name"),
    column_label = c("Event Date", "Decimal Latitude", "Scientific Name"),
    column_description = c(
      "Date the event occurred",
      "Latitude in decimal degrees",
      "Scientific name of the organism"
    ),
    column_role = c("temporal", "attribute", "attribute")
  )

  res <- suggest_dwc_mappings(dict, max_per_column = 2)
  suggestions <- attr(res, "dwc_mappings")

  expect_true(is.data.frame(suggestions))
  expect_true(nrow(suggestions) > 0)
  expect_true("field_name" %in% names(suggestions))

  expect_true(any(suggestions$field_name == "eventDate"))
  expect_true(any(suggestions$field_name == "decimalLatitude"))
  expect_true(any(suggestions$field_name == "scientificName"))

  # New tables: material and material-assertion (MeasurementOrFact pattern)
  dict_material <- tibble::tibble(
    column_name = c("material_entity_id", "assertion_value_numeric"),
    column_label = c("Material Entity ID", "Assertion Value Numeric"),
    column_description = c("Identifier for the material entity", "Numeric assertion value")
  )
  res_material <- suggest_dwc_mappings(dict_material, max_per_column = 2)
  suggestions2 <- attr(res_material, "dwc_mappings")
  expect_true(any(suggestions2$field_name == "materialEntityID"))
  expect_true(any(suggestions2$field_name == "assertionValueNumeric"))
})
