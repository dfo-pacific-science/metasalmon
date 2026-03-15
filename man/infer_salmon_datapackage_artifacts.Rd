\name{infer_salmon_datapackage_artifacts}
\alias{infer_salmon_datapackage_artifacts}
\title{Infer Salmon Data Package artifacts from resource tables}
\description{
Infers package-building metadata artifacts from one or more input tables in a single
step.
}

\usage{
infer_salmon_datapackage_artifacts(
  resources,
  dataset_id = "dataset-1",
  table_id = "table-1",
  guess_types = TRUE,
  seed_semantics = TRUE,
  semantic_sources = c("smn", "gcdfo", "ols", "nvs"),
  semantic_max_per_role = 1,
  seed_verbose = TRUE,
  seed_codes = NULL,
  seed_table_meta = NULL,
  seed_dataset_meta = NULL
)
}

\arguments{
\item{resources}{Either a single data frame or a named list of data frames.}

\item{dataset_id}{Dataset identifier applied to all inferred metadata rows.}

\item{table_id}{Fallback table identifier when \code{resources} is a single data frame.}

\item{guess_types}{Logical; if TRUE (default), infer \code{value_type} for each
column in the dictionary.}

\item{seed_semantics}{Logical; if TRUE, run \code{suggest_semantics()} and attach
semantic suggestions.}

\item{semantic_sources}{Vector of vocabulary sources forwarded to
\code{suggest_semantics()}.}

\item{semantic_max_per_role}{Maximum number of suggestions retained per I-ADOPT
role.}

\item{seed_verbose}{Logical; if TRUE, report inference and seeding progress.}

\item{seed_codes}{Optional prebuilt codes metadata used to seed semantic suggestion target generation.}

\item{seed_table_meta}{Optional prebuilt tables metadata used to seed semantic suggestion target generation.}

\item{seed_dataset_meta}{Optional prebuilt dataset metadata used to seed semantic suggestion target generation.}
}

\value{
A list with elements:
\describe{
  \item{\code{resources}}{Named list of input tables.}
  \item{\code{dataset_id}}{Dataset identifier used during inference.}
  \item{\code{dict}}{Inferred dictionary tibble.}
  \item{\code{table_meta}}{Inferred table metadata tibble.}
  \item{\code{codes}}{Inferred candidate codes tibble.}
  \item{\code{dataset_meta}}{Inferred dataset metadata tibble.}
  \item{\code{semantic_suggestions}}{The semantic suggestion tibble attached during seeding, or \code{NULL} when \code{seed_semantics = FALSE}.}
}
}

\details{
The return object is intentionally shaped for common biologist workflows:
use it with \code{create_salmon_datapackage()} once you are happy with the inferred
metadata.
}

\seealso{
\code{\link{infer_dictionary}}, \code{\link{create_salmon_datapackage}}, \code{\link{suggest_semantics}}
}

\examples{
\dontrun{
resources <- list(
  catches = data.frame(
    station_id = c("A", "B"),
    species = c("Coho", "Chinook"),
    count = c(10L, 20L),
    sample_date = as.Date(c("2024-01-01", "2024-01-02"))
  ),
  stations = data.frame(
    station_id = c("A", "B"),
    latitude = c(49.8, 49.9),
    longitude = c(-124.4, -124.5)
  )
)

artifacts <- infer_salmon_datapackage_artifacts(
  resources,
  dataset_id = "demo-1",
  seed_semantics = TRUE,
  seed_verbose = TRUE
)

artifacts$dict
artifacts$table_meta
artifacts$codes
artifacts$dataset_meta
}
}
