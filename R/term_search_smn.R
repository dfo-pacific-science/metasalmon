# Helpers for Salmon Domain Ontology (SMN) module indexing
#
# The shared SMN root (`https://w3id.org/smn/`) is the canonical entrypoint for
# the latest ontology. For lightweight lexical search we index the canonical
# module IRIs under `https://w3id.org/smn/modules/...`, which currently remain
# Turtle-first on W3ID.

.smn_module_urls <- function() {
  base <- "https://w3id.org/smn/modules"
  c(
    paste0(base, "/01-entity-systematics"),
    paste0(base, "/02-observation-measurement"),
    paste0(base, "/03-assessment-benchmarks"),
    paste0(base, "/04-management-governance"),
    paste0(base, "/05-provenance-quality"),
    paste0(base, "/06-data-interoperability"),
    paste0(base, "/07-controlled-vocabularies"),
    paste0(base, "/08-rda-case-study-profile-bridges"),
    paste0(base, "/09-rda-neville-decomposition-profile-bridges"),
    paste0(base, "/alignment-main"),
    paste0(base, "/alignment-research")
  )
}

.smn_cache_slug <- function(url) {
  slug <- sub("^https?://", "", url)
  slug <- gsub("[^A-Za-z0-9]+", "-", slug)
  slug <- gsub("(^-+|-+$)", "", slug)
  slug
}

.smn_fetch_module_path <- function(url, cache_dir) {
  fetch_salmon_ontology(
    url = url,
    accept = "text/turtle, text/plain;q=0.9",
    cache_dir = file.path(cache_dir, .smn_cache_slug(url)),
    fallback_urls = character()
  )
}

.smn_module_index_bundle <- function(cache_dir) {
  paths <- vapply(.smn_module_urls(), .smn_fetch_module_path, character(1), cache_dir = cache_dir)
  info <- file.info(paths)
  stamp <- paste(
    paste(paths, as.numeric(info$mtime), info$size, sep = "::"),
    collapse = "|"
  )

  list(
    paths = paths,
    stamp = stamp,
    index = .parse_smn_ttl_modules(paths)
  )
}

.smn_ttl_prefixes <- function(text) {
  lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  prefix_lines <- grep("^\\s*@prefix\\s+", lines, value = TRUE)
  if (length(prefix_lines) == 0) {
    return(character())
  }

  out <- stats::setNames(character(length(prefix_lines)), character(length(prefix_lines)))
  idx <- 0L
  for (line in prefix_lines) {
    m <- regexec("^\\s*@prefix\\s+([A-Za-z][A-Za-z0-9_-]*):\\s*<([^>]+)>\\s*\\.", line, perl = TRUE)
    hits <- regmatches(line, m)[[1]]
    if (length(hits) == 3) {
      idx <- idx + 1L
      out[[idx]] <- hits[[3]]
      names(out)[[idx]] <- hits[[2]]
    }
  }

  out[seq_len(idx)]
}

.smn_expand_curie <- function(term, prefixes) {
  term <- trimws(term)
  if (!nzchar(term)) {
    return(NA_character_)
  }
  if (startsWith(term, "<") && endsWith(term, ">")) {
    return(substring(term, 2L, nchar(term) - 1L))
  }
  if (!grepl(":", term, fixed = TRUE)) {
    return(term)
  }

  prefix <- sub(":.*$", "", term)
  local <- sub("^[^:]+:", "", term)
  base <- prefixes[[prefix]]
  if (is.null(base) || !nzchar(base)) {
    return(term)
  }
  paste0(base, local)
}

.smn_literal_values <- function(text) {
  pieces <- gregexpr('"[^"]+"(?:@[A-Za-z-]+)?', text, perl = TRUE)
  vals <- regmatches(text, pieces)[[1]]
  if (length(vals) == 0) {
    return(character())
  }
  vals <- sub('^"', "", vals)
  vals <- sub('"(@[A-Za-z-]+)?$', "", vals)
  vals
}

.smn_term_values <- function(text, prefixes) {
  pieces <- gregexpr('(<[^>]+>|[A-Za-z][A-Za-z0-9_-]*:[^\\s,;]+)', text, perl = TRUE)
  vals <- regmatches(text, pieces)[[1]]
  if (length(vals) == 0) {
    return(character())
  }
  vals <- vapply(vals, .smn_expand_curie, character(1), prefixes = prefixes)
  vals[!is.na(vals) & nzchar(vals)]
}

.smn_predicate_chunks <- function(rest, predicate) {
  pred <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", predicate, perl = TRUE)
  pattern <- paste0("(?:^|;)\\s*", pred, "\\s+([^;]+)")
  pieces <- gregexpr(pattern, rest, perl = TRUE)
  vals <- regmatches(rest, pieces)[[1]]
  if (length(vals) == 0) {
    return(character())
  }
  vals <- sub(paste0("^(?:;)?\\s*", pred, "\\s+"), "", trimws(vals), perl = TRUE)
  trimws(vals)
}

.smn_subject_local_name <- function(iri) {
  if (is.na(iri) || !nzchar(iri)) {
    return(NA_character_)
  }
  sub("^.*/", "", iri)
}

.smn_resource_kind <- function(type_iris) {
  types <- tolower(type_iris)
  if (any(grepl("skos/core#conceptscheme$", types))) return("ConceptScheme")
  if (any(grepl("skos/core#concept$", types))) return("Concept")
  if (any(grepl("owl#namedindividual$", types))) return("NamedIndividual")
  if (any(grepl("owl#objectproperty$", types))) return("ObjectProperty")
  if (any(grepl("owl#dataproperty$", types))) return("DataProperty")
  if (any(grepl("owl#annotationproperty$", types))) return("AnnotationProperty")
  if (any(grepl("owl#class$", types))) return("Class")
  NA_character_
}

.smn_role_flags <- function(label, definition, resource_kind, module_name, in_scheme, parent_iris, type_iris, iri) {
  txt <- tolower(paste(
    label,
    definition,
    resource_kind,
    module_name,
    in_scheme,
    parent_iris,
    type_iris,
    .smn_subject_local_name(iri),
    collapse = " "
  ))

  is_scheme <- grepl("scheme", txt)
  is_entity <- (
    grepl("entity-systematics", module_name) |
      grepl("entity|population|stock|river|habitat|taxon|organism|individual|group|stratum|species", txt)
  ) &&
    !is_scheme &&
    !grepl("measurement|assessment|benchmark|reference point|procedure|method|characteristic|property", txt)
  is_property <- grepl("property|characteristic|length|weight|size|status|confidence|phase", txt)
  if (grepl("sosa/property", txt)) {
    is_property <- TRUE
  }
  is_method <- grepl("method|procedure|protocol|enumeration", txt)
  if (grepl("sosa/procedure", txt)) {
    is_method <- TRUE
  }
  is_constraint <- (
    grepl("assessment-benchmarks|controlled-vocabularies", module_name) |
      grepl("constraint|context|phase|origin|benchmark|reference point|target|limit|status zone", txt)
  )
  is_variable <- grepl("measurement|abundance|count|rate|escapement|recruit|indicator|benchmark|reference point", txt) &&
    !grepl("context|scheme", txt)

  list(
    is_variable = is_variable,
    is_property = is_property,
    is_entity = is_entity,
    is_constraint = is_constraint,
    is_method = is_method
  )
}

.parse_smn_ttl_modules <- function(paths) {
  rows <- list()
  idx <- 0L

  for (path in paths) {
    if (!file.exists(path)) {
      next
    }

    text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    prefixes <- .smn_ttl_prefixes(text)
    stripped <- gsub("(?m)^\\s*#.*$", "", text, perl = TRUE)
    stripped <- gsub("(?m)^\\s*@prefix\\s+.*$", "", stripped, perl = TRUE)
    blocks <- strsplit(stripped, "\\n\\s*\\n+", perl = TRUE)[[1]]
    blocks <- trimws(blocks)
    blocks <- blocks[nzchar(blocks)]
    module_name <- basename(path)

    for (block in blocks) {
      collapsed <- gsub("\\s+", " ", trimws(block), perl = TRUE)
      if (!nzchar(collapsed)) {
        next
      }

      subject <- sub("\\s.*$", "", collapsed)
      if (!grepl("^smn:", subject)) {
        next
      }

      iri <- .smn_expand_curie(subject, prefixes)
      if (!grepl("^https?://w3id\\.org/smn/", iri)) {
        next
      }

      rest <- trimws(sub("^\\S+\\s+", "", collapsed, perl = TRUE))
      type_iris <- unique(unlist(lapply(.smn_predicate_chunks(rest, "a"), .smn_term_values, prefixes = prefixes), use.names = FALSE))
      labels <- unique(c(
        unlist(lapply(.smn_predicate_chunks(rest, "rdfs:label"), .smn_literal_values), use.names = FALSE),
        unlist(lapply(.smn_predicate_chunks(rest, "skos:prefLabel"), .smn_literal_values), use.names = FALSE)
      ))
      alt_labels <- unique(unlist(lapply(.smn_predicate_chunks(rest, "skos:altLabel"), .smn_literal_values), use.names = FALSE))
      definition <- unique(c(
        unlist(lapply(.smn_predicate_chunks(rest, "iao:0000115"), .smn_literal_values), use.names = FALSE),
        unlist(lapply(.smn_predicate_chunks(rest, "skos:definition"), .smn_literal_values), use.names = FALSE),
        unlist(lapply(.smn_predicate_chunks(rest, "rdfs:comment"), .smn_literal_values), use.names = FALSE)
      ))
      in_scheme <- unique(unlist(lapply(.smn_predicate_chunks(rest, "skos:inScheme"), .smn_term_values, prefixes = prefixes), use.names = FALSE))
      parents <- unique(c(
        unlist(lapply(.smn_predicate_chunks(rest, "rdfs:subClassOf"), .smn_term_values, prefixes = prefixes), use.names = FALSE),
        unlist(lapply(.smn_predicate_chunks(rest, "skos:broader"), .smn_term_values, prefixes = prefixes), use.names = FALSE),
        unlist(lapply(.smn_predicate_chunks(rest, "owl:equivalentClass"), .smn_term_values, prefixes = prefixes), use.names = FALSE),
        unlist(lapply(.smn_predicate_chunks(rest, "rdfs:subPropertyOf"), .smn_term_values, prefixes = prefixes), use.names = FALSE)
      ))

      label <- if (length(labels)) labels[[1]] else .smn_subject_local_name(iri)
      definition_text <- if (length(definition)) paste(definition, collapse = " | ") else ""
      resource_kind <- .smn_resource_kind(type_iris)
      role_flags <- .smn_role_flags(
        label = label,
        definition = definition_text,
        resource_kind = resource_kind,
        module_name = module_name,
        in_scheme = paste(in_scheme, collapse = " | "),
        parent_iris = paste(parents, collapse = " | "),
        type_iris = paste(type_iris, collapse = " | "),
        iri = iri
      )
      search_text <- tolower(paste(
        label,
        paste(alt_labels, collapse = " "),
        definition_text,
        paste(in_scheme, collapse = " "),
        paste(parents, collapse = " "),
        .smn_subject_local_name(iri),
        module_name,
        collapse = " "
      ))
      role_hints <- paste(
        c(
          if (isTRUE(role_flags$is_variable)) "variable",
          if (isTRUE(role_flags$is_property)) "property",
          if (isTRUE(role_flags$is_entity)) "entity",
          if (isTRUE(role_flags$is_constraint)) "constraint",
          if (isTRUE(role_flags$is_method)) "method"
        ),
        collapse = "|"
      )

      idx <- idx + 1L
      rows[[idx]] <- tibble::tibble(
        iri = iri,
        label = label,
        alt_labels = paste(alt_labels, collapse = " | "),
        definition = definition_text,
        resource_kind = resource_kind %||% "Resource",
        in_scheme = paste(in_scheme, collapse = " | "),
        parent_iris = paste(parents, collapse = " | "),
        type_iris = paste(type_iris, collapse = " | "),
        search_text = search_text,
        is_variable = isTRUE(role_flags$is_variable),
        is_property = isTRUE(role_flags$is_property),
        is_entity = isTRUE(role_flags$is_entity),
        is_constraint = isTRUE(role_flags$is_constraint),
        is_method = isTRUE(role_flags$is_method),
        role_hints = role_hints
      )
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      iri = character(),
      label = character(),
      alt_labels = character(),
      definition = character(),
      resource_kind = character(),
      in_scheme = character(),
      parent_iris = character(),
      type_iris = character(),
      search_text = character(),
      is_variable = logical(),
      is_property = logical(),
      is_entity = logical(),
      is_constraint = logical(),
      is_method = logical(),
      role_hints = character()
    ))
  }

  dplyr::bind_rows(rows) %>%
    dplyr::distinct(.data$iri, .keep_all = TRUE)
}
