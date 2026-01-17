test_that("ices_code_types returns code types and supports text filtering", {
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
  expect_s3_class(res_all, "tbl_df")
  expect_equal(nrow(res_all), 2)

  res_filtered <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      if (grepl("/CodeType", url, fixed = TRUE)) return(fake)
      NULL
    },
    ices_find_code_types("gear")
  )
  expect_equal(nrow(res_filtered), 1)
  expect_equal(res_filtered$key[[1]], "Gear")
})

test_that("ices_codes returns codes with a detail URL and supports filtering", {
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
  expect_s3_class(res_all, "tbl_df")
  expect_true(all(c("code_type", "url") %in% names(res_all)))
  expect_match(res_all$url[[1]], "CodeDetail/Gear/BOT", fixed = TRUE)

  res_filtered <- with_mocked_bindings(
    .safe_json = function(url, headers = NULL, timeout_secs = 30) {
      if (grepl("/Code/Gear", url, fixed = TRUE)) return(fake)
      NULL
    },
    ices_find_codes("beam", "Gear")
  )
  expect_equal(nrow(res_filtered), 1)
  expect_equal(res_filtered$key[[1]], "BMT")
})

