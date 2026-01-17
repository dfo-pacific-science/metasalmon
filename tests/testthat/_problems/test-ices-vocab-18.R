# Extracted from test-ices-vocab.R:18

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "metasalmon", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
fake <- data.frame(
    id = c(1, 2),
    guid = c("g1", "g2"),
    key = c("Gear", "TS_Sex"),
    description = c("Gear Type Codes", "Sex Codes (Fisheries)"),
    longDescription = c("", ""),
    modified = c("2025-01-01T00:00:00", "2025-01-01T00:00:00"),
    stringsAsFactors = FALSE
  )
res_all <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      if (grepl("/CodeType", url, fixed = TRUE)) return(fake)
      NULL
    },
    ices_code_types()
  )
