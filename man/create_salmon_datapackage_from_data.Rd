\name{create_salmon_datapackage_from_data}
\alias{create_salmon_datapackage_from_data}
\title{Create a Salmon Data Package from raw tables}
\description{
Convenience one-shot helper that infers package artifacts from one or more raw
tables and immediately writes a Salmon Data Package.
}

\usage{
create_salmon_datapackage_from_data(
  resources,
  path,
  dataset_id = "dataset-1",
  table_id = "table-1",
  guess_types = TRUE,
  seed_semantics = TRUE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
  semantic_max_per_role = 1,
  seed_verbose = TRUE,
  seed_codes = NULL,
  seed_table_meta = NULL,
  seed_dataset_meta = NULL,
  format = "csv",
  overwrite = FALSE,
  include_edh_xml = FALSE,
  edh_profile = c("dfo_edh_hnap", "iso19139"),
  edh_xml_path = NULL
)
}

\arguments{
\item{resources}{Either a single data frame or a named list of data frames.}

\item{path}{Character; directory path where the package will be written.}

\item{dataset_id}{Dataset identifier applied to all inferred metadata rows.}

\item{table_id}{Fallback table identifier when \code{resources} is a single data frame.}

\item{guess_types}{Logical; if TRUE (default), infer \code{value_type} for each
dictionary column.}

\item{seed_semantics}{Logical; if TRUE, run \code{suggest_semantics()} during
inference and attach semantic suggestions to the returned dictionary (review-only, by design).}

\item{semantic_sources}{Vector of vocabulary sources passed to
\code{suggest_semantics()}.}

\item{semantic_max_per_role}{Maximum number of suggestions retained per I-ADOPT
role.}

\item{seed_verbose}{Logical; if TRUE, emit progress messages while seeding
semantic suggestions.}

\item{seed_codes}{Optional prebuilt \code{codes.csv}-style seed metadata used
to seed semantic suggestion target generation.}

\item{seed_table_meta}{Optional prebuilt \code{tables.csv}-style seed metadata used
to seed semantic suggestion target generation.}

\item{seed_dataset_meta}{Optional prebuilt \code{dataset.csv}-style seed metadata
used to seed semantic suggestion target generation.}

\item{format}{Character; resource format. Only \code{"csv"} is supported.}

\item{overwrite}{Logical; if FALSE (default), errors when the target path exists.}
\item{include_edh_xml}{Logical; if TRUE, writes EDH XML metadata to the package path.}
\item{edh_profile}{Profile for EDH XML export, one of `dfo_edh_hnap` or `iso19139`.}
\item{edh_xml_path}{Optional output file path for the EDH XML. Defaults to `metadata-edh-hnap.xml` for `dfo_edh_hnap` and `metadata-iso19139.xml` for `iso19139`.}
}

\value{
Invisibly returns the path to the created package.
}

\details{
This is a one-shot bootstrap flow intended for fast first-pass package creation, not automatic publication-ready review.

Use this function for demos and initial assembly, then run an explicit iterative loop:
\enumerate{
  \item{Inspect inferred artifacts with `infer_salmon_datapackage_artifacts(..., seed_semantics = TRUE)`.}
  \item{Validate and clean with `validate_dictionary()` and `validate_semantics()`.}
  \item{Use `suggest_semantics()` and `apply_semantic_suggestions()` deliberately (review-first by design).}
  \item{Rebuild with `create_salmon_datapackage()` using reviewed metadata.}
}

See \url{https://dfo-pacific-science.github.io/metasalmon/articles/data-dictionary-publication.html} and
\url{https://dfo-pacific-science.github.io/metasalmon/articles/reusing-standards-salmon-data-terms.html} for full guidance.
}

\seealso{
\code{\link{infer_salmon_datapackage_artifacts}}, \code{\link{create_salmon_datapackage}}, \code{\link{suggest_semantics}}
}

\examples{
\dontrun{
resources <- list(
  catches = data.frame(
    station_id = c("A", "B"),
    species = c("Coho", "Chinook"),
    count = c(10L, 20L)
  ),
  stations = data.frame(
    station_id = c("A", "B"),
    lat = c(49.8, 49.9),
    lon = c(-124.4, -124.5)
  )
)

pkg_path <- create_salmon_datapackage_from_data(
  resources,
  path = "my-package",
  dataset_id = "demo-1",
  seed_semantics = TRUE,
  overwrite = TRUE
)

pkg_path
}
}
