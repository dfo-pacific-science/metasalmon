fill_measurement_components <- function(dict) {
  measurement_rows <- !is.na(dict$column_role) & dict$column_role == "measurement"
  if (!any(measurement_rows)) {
    return(dict)
  }

  required_cols <- c("term_iri", "property_iri", "entity_iri", "unit_iri")
  for (col in required_cols) {
    if (!col %in% names(dict)) {
      dict[[col]] <- NA_character_
    }
  }

  dict$term_iri[measurement_rows & (is.na(dict$term_iri) | dict$term_iri == "")] <-
    "https://example.org/variable"
  dict$property_iri[measurement_rows & (is.na(dict$property_iri) | dict$property_iri == "")] <-
    "https://example.org/property"
  dict$entity_iri[measurement_rows & (is.na(dict$entity_iri) | dict$entity_iri == "")] <-
    "https://example.org/entity"
  dict$unit_iri[measurement_rows & (is.na(dict$unit_iri) | dict$unit_iri == "")] <-
    "https://example.org/unit"

  dict
}
