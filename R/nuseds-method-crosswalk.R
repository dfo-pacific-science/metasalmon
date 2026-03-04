#' NuSEDS enumeration method crosswalk
#'
#' Return a static crosswalk of NuSEDS `ENUMERATION_METHODS` values to the
#' canonical enumeration method-family labels used by the Type 1--6 guidance.
#'
#' The returned table tracks the legacy NuSEDS term, the canonical family code,
#' and linked ontology identifiers used in the current implementation.
#'
#' @return A tibble with columns `nuseds_value`, `method_family`,
#'   `ontology_term`, and `notes`.
#' @export
#' @examples
#' nuseds_enumeration_method_crosswalk()
#'
#' @seealso [nuseds_estimate_method_crosswalk()]
nuseds_enumeration_method_crosswalk <- function() {
  term_family <- c(
    "Bank Walk" = "V",
    "Stream Walk" = "V",
    "Walk" = "V",
    "Boat" = "V",
    "Float" = "V",
    "Snorkel" = "V",
    "Snorkel Swim" = "V",
    "Strip Counts" = "V",
    "Spot Checks" = "V",
    "Dead Pitch" = "V",
    "Peak Live and Dead Count" = "V",
    "Fence" = "FS",
    "Electronic Counters" = "FS",
    "Enumeration by Hatchery" = "FS",
    "Broodstock Removal" = "FS",
    "Fixed Wing Aircraft" = "A",
    "Helicopter" = "A",
    "Hydroacoustic Station" = "S",
    "Trap" = "T",
    "Redd Counts" = "R",
    "Electroshocking" = "P",
    "Tag Recovery" = "M",
    "Based on Angling Catch" = "P",
    "Biologist/Working Group" = "unknown",
    "Other" = "unknown"
  )

  ontology_term <- c(
    "Bank Walk" = "gcdfo:VisualGroundCount",
    "Stream Walk" = "gcdfo:VisualGroundCount",
    "Walk" = "gcdfo:VisualGroundCount",
    "Boat" = "gcdfo:VisualGroundCount",
    "Float" = "gcdfo:VisualGroundCount",
    "Snorkel" = "gcdfo:VisualSnorkelCount",
    "Snorkel Swim" = "gcdfo:VisualSnorkelCount",
    "Strip Counts" = "gcdfo:VisualGroundCount",
    "Spot Checks" = "gcdfo:VisualGroundCount",
    "Dead Pitch" = "gcdfo:VisualGroundCount",
    "Peak Live and Dead Count" = "gcdfo:VisualGroundCount",
    "Fence" = "gcdfo:FixedSiteCensusManual",
    "Electronic Counters" = "gcdfo:FixedSiteCensusElectronic",
    "Enumeration by Hatchery" = "gcdfo:FixedSiteCensusManual",
    "Broodstock Removal" = "gcdfo:FixedSiteCensusManual",
    "Fixed Wing Aircraft" = "gcdfo:AerialSurveyCount",
    "Helicopter" = "gcdfo:AerialSurveyCount",
    "Hydroacoustic Station" = "gcdfo:HydroacousticSonarCount",
    "Trap" = "gcdfo:TrapCount",
    "Redd Counts" = "gcdfo:ReddCount",
    "Electroshocking" = "gcdfo:ElectrofishingCount",
    "Tag Recovery" = "gcdfo:MarkRecaptureFieldProgram",
    "Based on Angling Catch" = "gcdfo:EnumerationMethod",
    "Biologist/Working Group" = NA_character_,
    "Other" = NA_character_
  )

  note <- c(
    "Bank Walk" = "",
    "Stream Walk" = "",
    "Walk" = "",
    "Boat" = "",
    "Float" = "",
    "Snorkel" = "",
    "Snorkel Swim" = "",
    "Strip Counts" = "",
    "Spot Checks" = "",
    "Dead Pitch" = "Carcass-based visual surveys; often paired with peak/cumulative dead estimation methods.",
    "Peak Live and Dead Count" = "Value is analysis-like; prefer capturing peak/cumulative variants under ESTIMATE_METHOD.",
    "Fence" = "",
    "Electronic Counters" = "",
    "Enumeration by Hatchery" = "",
    "Broodstock Removal" = "",
    "Fixed Wing Aircraft" = "",
    "Helicopter" = "",
    "Hydroacoustic Station" = "",
    "Trap" = "If trap is non-spanning or efficiency-corrected, use T; fully constraining traps may behave more like fixed-site counting.",
    "Redd Counts" = "",
    "Electroshocking" = "",
    "Tag Recovery" = "",
    "Based on Angling Catch" = "Catch-based index; treat as a CPUE-style index unless more detail is provided.",
    "Biologist/Working Group" = "Not a method. Treat as method-unknown unless a specific field/analysis method is documented elsewhere.",
    "Other" = "Treat as method-unknown unless a specific field/analysis method is documented elsewhere."
  )

  methods <- names(term_family)
  data <- data.frame(
    nuseds_value = methods,
    method_family = unname(term_family[methods]),
    ontology_term = unname(ontology_term[methods]),
    notes = unname(note[methods]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  tibble::as_tibble(data[order(data$method_family, data$nuseds_value), ])
}


#' NuSEDS estimate method crosswalk
#'
#' Return a static crosswalk of NuSEDS `ESTIMATE_METHOD` values to the
#' canonical estimate-method families used by the Type 1--6 guidance.
#'
#' The returned table tracks the legacy NuSEDS term, the canonical family label,
#' and linked ontology identifiers used in the current implementation.
#'
#' @return A tibble with columns `nuseds_value`, `method_family`, `guidance_interpretation`,
#'   `ontology_term`, and `notes`.
#' @export
#' @examples
#' nuseds_estimate_method_crosswalk()
nuseds_estimate_method_crosswalk <- function() {
  term_family <- c(
    "Fixed Site Census" = "FS",
    "Resistivity Counter" = "FS",
    "Video Counter" = "FS",
    "Sonar-ARIS" = "S",
    "Sonar-DIDSON" = "S",
    "Mark & Recapture: Petersen" = "M",
    "Mark & Recapture: Jolly-Seber" = "M",
    "Mark & Recapture: Bayesian" = "M",
    "Mark & Recapture: Open Model" = "M",
    "Area Under the Curve" = "V",
    "Peak Live + Dead" = "V",
    "Peak Live + Cumulative Dead" = "V",
    "(Peak Live+Cum Dead)*Expansion" = "V",
    "Peak Live * Expansion" = "V",
    "Redd Count" = "R",
    "Cumulative CPUE" = "P",
    "Addition/Subtraction" = "depends",
    "Multiplication/Division" = "depends",
    "Lake Expansion" = "depends",
    "Calibrated Time Series" = "depends",
    "Combined Methods" = "depends",
    "Insufficient Information" = "unknown",
    "Unknown Estimate Method" = "unknown",
    "Other Estimate Method" = "unknown",
    "Not Applicable" = "unknown",
    "Expert Opinion" = "unknown",
    "Cumulative New" = "V"
  )

  guidance <- c(
    "Fixed Site Census" = "Enumeration device/mode (often stored as estimate method)",
    "Resistivity Counter" = "Enumeration device/mode (often stored as estimate method)",
    "Video Counter" = "Enumeration device/mode (often stored as estimate method)",
    "Sonar-ARIS" = "Hydroacoustic modelling pipeline",
    "Sonar-DIDSON" = "Hydroacoustic modelling pipeline",
    "Mark & Recapture: Petersen" = "Mark-recapture estimation",
    "Mark & Recapture: Jolly-Seber" = "Mark-recapture estimation",
    "Mark & Recapture: Bayesian" = "Mark-recapture estimation",
    "Mark & Recapture: Open Model" = "Mark-recapture estimation",
    "Area Under the Curve" = "Visual-series estimation (AUC/peak variants and expansions)",
    "Peak Live + Dead" = "Visual-series estimation (AUC/peak variants and expansions)",
    "Peak Live + Cumulative Dead" = "Visual-series estimation (AUC/peak variants and expansions)",
    "(Peak Live+Cum Dead)*Expansion" = "Visual-series estimation (AUC/peak variants and expansions)",
    "Peak Live * Expansion" = "Visual-series estimation (AUC/peak variants and expansions)",
    "Redd Count" = "Redd-based estimation (requires spawners-per-redd conversion)",
    "Cumulative CPUE" = "CPUE index",
    "Addition/Subtraction" = "Math/expansion operations (depends on base method)",
    "Multiplication/Division" = "Math/expansion operations (depends on base method)",
    "Lake Expansion" = "Math/expansion operations (depends on base method)",
    "Calibrated Time Series" = "Calibrated time series (requires calibration source + diagnostics)",
    "Combined Methods" = "Combined-method workflow (requires explicit component listing)",
    "Insufficient Information" = "Method unknown/administrative label",
    "Unknown Estimate Method" = "Method unknown/administrative label",
    "Other Estimate Method" = "Method unknown/administrative label",
    "Not Applicable" = "Method unknown/administrative label",
    "Expert Opinion" = "Method unknown/administrative label",
    "Cumulative New" = "Visual-series estimation (AUC/peak variants and expansions)"
  )

  ontology_term <- c(
    "Fixed Site Census" = "gcdfo:FixedStationTally",
    "Resistivity Counter" = "gcdfo:FixedStationTally",
    "Video Counter" = "gcdfo:FixedStationTally",
    "Sonar-ARIS" = "gcdfo:HydroacousticModelling",
    "Sonar-DIDSON" = "gcdfo:HydroacousticModelling",
    "Mark & Recapture: Petersen" = "gcdfo:MarkRecaptureAnalysis",
    "Mark & Recapture: Jolly-Seber" = "gcdfo:MarkRecaptureAnalysis",
    "Mark & Recapture: Bayesian" = "gcdfo:MarkRecaptureAnalysis",
    "Mark & Recapture: Open Model" = "gcdfo:MarkRecaptureAnalysis",
    "Area Under the Curve" = "gcdfo:AreaUnderTheCurve",
    "Peak Live + Dead" = "gcdfo:PeakCountAnalysis",
    "Peak Live + Cumulative Dead" = "gcdfo:PeakCountAnalysis",
    "(Peak Live+Cum Dead)*Expansion" = "gcdfo:ExpansionMathematicalOperations",
    "Peak Live * Expansion" = "gcdfo:ExpansionMathematicalOperations",
    "Redd Count" = "gcdfo:ReddExpansionAnalysis",
    "Cumulative CPUE" = "gcdfo:EstimateMethod",
    "Addition/Subtraction" = "gcdfo:ExpansionMathematicalOperations",
    "Multiplication/Division" = "gcdfo:ExpansionMathematicalOperations",
    "Lake Expansion" = "gcdfo:ExpansionMathematicalOperations",
    "Calibrated Time Series" = "gcdfo:CalibratedTimeSeries",
    "Combined Methods" = "gcdfo:EstimateMethod",
    "Insufficient Information" = NA_character_,
    "Unknown Estimate Method" = NA_character_,
    "Other Estimate Method" = NA_character_,
    "Not Applicable" = NA_character_,
    "Expert Opinion" = NA_character_,
    "Cumulative New" = "gcdfo:PeakCountAnalysis"
  )

  note <- c(
    "Fixed Site Census" = "",
    "Resistivity Counter" = "Enumerated as a device/mode; ensure bypass/coverage/QA metadata are captured.",
    "Video Counter" = "Enumerated as a device/mode; ensure QA review rate and uptime/coverage metadata are captured.",
    "Sonar-ARIS" = "",
    "Sonar-DIDSON" = "",
    "Mark & Recapture: Petersen" = "",
    "Mark & Recapture: Jolly-Seber" = "",
    "Mark & Recapture: Bayesian" = "",
    "Mark & Recapture: Open Model" = "",
    "Area Under the Curve" = "",
    "Peak Live + Dead" = "",
    "Peak Live + Cumulative Dead" = "",
    "(Peak Live+Cum Dead)*Expansion" = "Legacy label with known ambiguity in operator precedence; confirm component order before interpretation.",
    "Peak Live * Expansion" = "Legacy label with known ambiguity in operator precedence; confirm component order before interpretation.",
    "Redd Count" = "",
    "Cumulative CPUE" = "No specific CPUE estimate concept is currently defined in this scheme; linked at EstimateMethod scheme level.",
    "Addition/Subtraction" = "Use explicit companion logic when combining methods; requires base-method context.",
    "Multiplication/Division" = "Use explicit companion logic when combining methods; requires base-method context.",
    "Lake Expansion" = "Use explicit companion logic when combining methods; requires base-method context.",
    "Calibrated Time Series" = "Record calibration source years, diagnostics, and revision history.",
    "Combined Methods" = "Decompose into components (e.g., sonar + visual apportionment) and apply conservative classification.",
    "Insufficient Information" = "",
    "Unknown Estimate Method" = "",
    "Other Estimate Method" = "",
    "Not Applicable" = "",
    "Expert Opinion" = "",
    "Cumulative New" = "Legacy dictionary label; mapping is provisional pending local confirmation."
  )

  methods <- names(term_family)
  data <- data.frame(
    nuseds_value = methods,
    method_family = unname(term_family[methods]),
    guidance_interpretation = unname(guidance[methods]),
    ontology_term = unname(ontology_term[methods]),
    notes = unname(note[methods]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  tibble::as_tibble(data[order(data$method_family, data$nuseds_value), ])
}
