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
  overwrite = FALSE
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
}

\value{
Invisibly returns the path to the created package.
}

\details{
Inferred artifacts are produced by
\code{infer_salmon_datapackage_artifacts()} and written with
\code{create_salmon_datapackage()}.
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
