# Extracted from test-ices-vocab.R:51

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "metasalmon", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
fake <- data.frame(
    id = c(10, 11),
    guid = c("c1", "c2"),
    key = c("BOT", "BMT"),
    description = c("Bottom Trawl", "Beam trawl"),
    longDescription = c("", ""),
    modified = c("2025-01-01T00:00:00", "2025-01-01T00:00:00"),
    deprecated = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
res_all <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      if (grepl("/Code/Gear", url, fixed = TRUE)) return(fake)
      NULL
    },
    ices_codes("Gear")
  )
