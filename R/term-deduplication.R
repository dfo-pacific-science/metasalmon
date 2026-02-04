#' Deduplicate proposed ontology terms
#'
#' Applies I-ADOPT compositional deduplication to a gpt_proposed_terms dataframe.
#' This prevents term proliferation by:
#' 1. Removing duplicates across tables (same term_label)
#' 2. Collapsing age-stratified variants (X Age 1..7) into one base term
#' 3. Collapsing phase-stratified variants (Ocean/Terminal/Mainstem X) into one base term
#' 4. Identifying terms that should use constraint_iri instead of new term_iri
#'
#' The function returns a deduplicated dataframe with added columns for facet handling.
#'
#' @param proposed_terms A data frame with columns: term_label, term_definition,
#'   term_type, suggested_parent_iri. Typically loaded from gpt_proposed_terms.csv.
#' @param warn_threshold Integer. If the input has more than this many rows,
#'   issue a warning about potential over-engineering. Default is 30.
#'
#' @return A tibble with deduplicated terms and additional columns:
#'   - `is_base_term`: TRUE if this is the canonical base term for a pattern
#'   - `needs_age_facet`: TRUE if age variants should use constraint_iri
#'   - `needs_phase_facet`: TRUE if phase variants should use constraint_iri
#'   - `collapsed_from`: Count of how many variants were collapsed into this term
#'   - `dedup_notes`: Explanation of deduplication applied
#'
#' @details
#' **Target ratio**: For a dictionary with N measurement columns, expect ~N/10 to N/5

#' distinct base terms, NOT N terms. If the output still has >30 rows, consider
#' further manual review.
#'
#' **Anti-patterns detected**:
#' - "Spawners Age 1", "Spawners Age 2", ... patterns → collapsed to "SpawnerCount"
#' - Duplicate term_labels across different tables → deduplicated
#' - Phase-stratified variants (Ocean X, Terminal X) → collapsed to base term
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load raw proposed terms
#' proposed <- readr::read_csv("work/semantics/gpt_proposed_terms.csv")
#'
#' # Deduplicate
#' deduped <- deduplicate_proposed_terms(proposed)
#'
#' # Review collapsed terms
#' deduped |> dplyr::filter(collapsed_from > 1)
#'
#' # Write cleaned output
#' readr::write_csv(deduped, "work/semantics/gpt_proposed_terms_deduped.csv")
#' }
deduplicate_proposed_terms <- function(proposed_terms, warn_threshold = 30L) {
  if (!is.data.frame(proposed_terms) || nrow(proposed_terms) == 0) {
    return(tibble::tibble(
      term_label = character(),
      term_definition = character(),
      term_type = character(),
      suggested_parent_iri = character(),
      is_base_term = logical(),
      needs_age_facet = logical(),
      needs_phase_facet = logical(),
      collapsed_from = integer(),
      dedup_notes = character()
    ))
  }

  # Warn if input is suspiciously large

if (nrow(proposed_terms) > warn_threshold) {
    cli::cli_warn(c(
      "!" = "gpt_proposed_terms has {nrow(proposed_terms)} rows (threshold: {warn_threshold}).",
      "i" = "This may indicate over-engineering. Expected: 15-25 base terms for a typical dataset.",
      "i" = "Review for: duplicate terms across tables, age/phase variants that should use constraint_iri."
    ))
  }

  df <- tibble::as_tibble(proposed_terms)

  # Normalize term_label for comparison
  df$label_normalized <- tolower(trimws(df$term_label))
  # A pattern-friendly normalization (underscores/punctuation -> spaces) for regex detection.
  df$label_pattern <- trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", df$label_normalized)))

  # Detect age-stratified patterns (e.g., "Spawners Age 1", "Catch Age 3")
  df$is_age_variant <- grepl("\\bage\\s*\\d+\\b", df$label_pattern, ignore.case = TRUE)
  df$age_base_label <- gsub("\\s*age\\s*\\d+\\s*", " ", df$label_pattern, ignore.case = TRUE)
  df$age_base_label <- trimws(gsub("\\s+", " ", df$age_base_label))

  # Detect phase-stratified patterns (e.g., "Ocean Catch", "Terminal Run")
  phase_prefixes <- c("ocean", "terminal", "mainstem", "marine", "freshwater", "in[- ]?river")
  phase_pattern <- paste0("^(", paste(phase_prefixes, collapse = "|"), ")\\s+")
  df$is_phase_variant <- grepl(phase_pattern, df$label_pattern, ignore.case = TRUE)
  df$phase_base_label <- gsub(phase_pattern, "", df$label_pattern, ignore.case = TRUE)
  df$phase_base_label <- trimws(df$phase_base_label)

  # Step 1: Remove exact duplicates by term_label (keep first occurrence)
  df <- df[!duplicated(df$label_normalized), ]
  n_after_exact_dedup <- nrow(df)

  # Step 2: Collapse age-stratified variants
  # Group by age_base_label and keep only the base term
  age_groups <- df |>
    dplyr::filter(.data$is_age_variant) |>
    dplyr::group_by(.data$age_base_label) |>
    dplyr::summarise(
      count = dplyr::n(),
      representative_label = .data$term_label[1],
      representative_def = .data$term_definition[1],
      .groups = "drop"
    ) |>
    dplyr::filter(.data$count > 1)

  # Mark age variants for removal (keep one representative per group)
  df$should_collapse_age <- FALSE
  for (i in seq_len(nrow(age_groups))) {
    base <- age_groups$age_base_label[i]
    matching_rows <- which(df$age_base_label == base & df$is_age_variant)
    if (length(matching_rows) > 1) {
      # Keep first, mark rest for removal
      df$should_collapse_age[matching_rows[-1]] <- TRUE
    }
  }

  # Step 3: Collapse phase-stratified variants (similar logic)
  phase_groups <- df |>
    dplyr::filter(.data$is_phase_variant & !.data$should_collapse_age) |>
    dplyr::group_by(.data$phase_base_label) |>
    dplyr::summarise(
      count = dplyr::n(),
      representative_label = .data$term_label[1],
      .groups = "drop"
    ) |>
    dplyr::filter(.data$count > 1)

  df$should_collapse_phase <- FALSE
  for (i in seq_len(nrow(phase_groups))) {
    base <- phase_groups$phase_base_label[i]
    matching_rows <- which(df$phase_base_label == base & df$is_phase_variant & !df$should_collapse_age)
    if (length(matching_rows) > 1) {
      df$should_collapse_phase[matching_rows[-1]] <- TRUE
    }
  }

  # Build output
  df$is_base_term <- !df$should_collapse_age & !df$should_collapse_phase
  df$needs_age_facet <- df$is_age_variant
  df$needs_phase_facet <- df$is_phase_variant

  # Calculate collapsed_from counts
  df$collapsed_from <- 1L
  for (i in seq_len(nrow(age_groups))) {
    base <- age_groups$age_base_label[i]
    first_row <- which(df$age_base_label == base & df$is_age_variant & df$is_base_term)[1]
    if (!is.na(first_row)) {
      df$collapsed_from[first_row] <- age_groups$count[i]
    }
  }
  for (i in seq_len(nrow(phase_groups))) {
    base <- phase_groups$phase_base_label[i]
    first_row <- which(df$phase_base_label == base & df$is_phase_variant & df$is_base_term)[1]
    if (!is.na(first_row)) {
      # Add to existing count (may have both age and phase collapsing)
      df$collapsed_from[first_row] <- df$collapsed_from[first_row] + phase_groups$count[i] - 1L
    }
  }

  # Add deduplication notes
  df$dedup_notes <- ""
  df$dedup_notes[df$needs_age_facet & df$is_base_term] <-
    "Base term for age variants; use an age-class constraint scheme (propose one if missing)"
  df$dedup_notes[df$needs_phase_facet & df$is_base_term & df$dedup_notes == ""] <-
    "Base term for phase variants; use a life-phase constraint scheme (propose one if missing)"
  df$dedup_notes[!df$is_base_term] <- "Collapsed into base term"

  # Filter to base terms only and clean up
  result <- df |>
    dplyr::filter(.data$is_base_term) |>
    dplyr::select(
      term_label = .data$term_label,
      term_definition = .data$term_definition,
      term_type = .data$term_type,
      suggested_parent_iri = .data$suggested_parent_iri,
      is_base_term = .data$is_base_term,
      needs_age_facet = .data$needs_age_facet,
      needs_phase_facet = .data$needs_phase_facet,
      collapsed_from = .data$collapsed_from,
      dedup_notes = .data$dedup_notes,
      dplyr::everything(),
      -dplyr::any_of(c(
        "label_normalized", "is_age_variant", "age_base_label",
        "is_phase_variant", "phase_base_label", "should_collapse_age",
        "should_collapse_phase"
      ))
    )

  # Report results
  n_removed <- nrow(proposed_terms) - nrow(result)
  if (n_removed > 0) {
    cli::cli_inform(c(
      "v" = "Deduplicated {nrow(proposed_terms)} -> {nrow(result)} terms ({n_removed} collapsed/removed).",
      "i" = "Review 'dedup_notes' column for facet handling guidance."
    ))
  }

  if (nrow(result) > warn_threshold) {
    cli::cli_warn(c(
      "!" = "After deduplication, still have {nrow(result)} terms (threshold: {warn_threshold}).",
      "i" = "Consider manual review for additional consolidation opportunities."
    ))
  }

  result
}

#' Suggest facet schemes for proposed terms
#'
#' Analyzes a proposed terms dataframe and suggests which facet schemes
#' (age class, life phase, etc.) should be created instead of proliferating
#' individual terms.
#'
#' @param proposed_terms A data frame with term_label column
#' @return A tibble with suggested facet schemes and their member concepts
#' @export
#'
#' @examples
#' \dontrun{
#' proposed <- readr::read_csv("work/semantics/gpt_proposed_terms.csv")
#' facets <- suggest_facet_schemes(proposed)
#' print(facets)
#' }
suggest_facet_schemes <- function(proposed_terms) {
  if (!is.data.frame(proposed_terms) || nrow(proposed_terms) == 0) {
    return(tibble::tibble(
      scheme_name = character(),
      scheme_definition = character(),
      suggested_concepts = list()
    ))
  }

  labels <- tolower(proposed_terms$term_label)

  schemes <- list()

  # Check for age patterns
  age_matches <- grep("(\\bage\\s*\\d+\\b|\\bage\\d+class\\b|\\bage\\d+\\b)", labels, ignore.case = TRUE, value = TRUE)
  if (length(age_matches) >= 3) {
    age_num_tokens <- unlist(regmatches(
      age_matches,
      gregexpr("age\\s*\\d+", age_matches, ignore.case = TRUE, perl = TRUE)
    ))
    ages_found <- sort(unique(as.integer(gsub("^age\\s*", "", age_num_tokens, ignore.case = TRUE))))
    schemes$AgeClassScheme <- list(
      scheme_name = "AgeClassScheme",
      scheme_definition = "Proposed age-class facets for age-stratified salmon measurements (e.g., Age1Class through Age7Class)",
      suggested_concepts = paste0("Age", ages_found, "Class")
    )
  }

  # Check for phase patterns
  phase_patterns <- c("ocean", "terminal", "mainstem", "marine", "freshwater")
  phases_found <- character()
  for (phase in phase_patterns) {
    # Accept both "ocean" and "oceanphase" style tokens
    if (any(grepl(paste0("\\b", phase, "(phase)?\\b"), labels, ignore.case = TRUE))) {
      phases_found <- c(phases_found, paste0(tools::toTitleCase(phase), "Phase"))
    }
  }
  if (length(phases_found) >= 2) {
    schemes$LifePhaseScheme <- list(
      scheme_name = "LifePhaseScheme",
      scheme_definition = "Proposed life-phase facets for location/phase-stratified salmon measurements",
      suggested_concepts = unique(phases_found)
    )
  }

  # Check for benchmark level patterns (lower/upper).
  # Prefer a generic facet scheme rather than CU-specific terms like CULowerBenchmark.
  benchmark_levels <- character()
  if (any(grepl("\\blowerbenchmark\\b|\\blower\\s+benchmark\\b", labels, ignore.case = TRUE))) {
    benchmark_levels <- c(benchmark_levels, "LowerBenchmark")
  }
  if (any(grepl("\\bupperbenchmark\\b|\\bupper\\s+benchmark\\b", labels, ignore.case = TRUE))) {
    benchmark_levels <- c(benchmark_levels, "UpperBenchmark")
  }
  if (length(unique(benchmark_levels)) >= 2) {
    schemes$BenchmarkLevelScheme <- list(
      scheme_name = "BenchmarkLevelScheme",
      scheme_definition = "Proposed benchmark-level facets (lower/upper) used to qualify benchmark values",
      suggested_concepts = unique(benchmark_levels)
    )
  }

  if (length(schemes) == 0) {
    return(tibble::tibble(
      scheme_name = character(),
      scheme_definition = character(),
      suggested_concepts = list()
    ))
  }

  tibble::tibble(
    scheme_name = vapply(schemes, `[[`, character(1), "scheme_name"),
    scheme_definition = vapply(schemes, `[[`, character(1), "scheme_definition"),
    suggested_concepts = lapply(schemes, `[[`, "suggested_concepts")
  )
}

# Global variable bindings for R CMD check
utils::globalVariables(c(
  "is_age_variant", "age_base_label", "is_phase_variant", "phase_base_label",
  "should_collapse_age", "should_collapse_phase", "is_base_term",
  "needs_age_facet", "needs_phase_facet", "collapsed_from", "dedup_notes",
  "label_normalized", "label_pattern"
))
