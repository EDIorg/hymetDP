library(hymetDP)

# search index ----------------------------------------------------------------

testthat::test_that("Search index is saved for future calls in session", {
  testthat::skip_on_cran()
  unlink(paste0(tempdir(), "/hymetDP_search_index.rda"))
  # Doesn't exist locally at time of first call
  is_local <- "hymetDP_search_index.rda" %in% dir(tempdir())
  expect_false(is_local)
  # Is created during first call
  r <- search_data()
  is_local <- "hymetDP_search_index.rda" %in% dir(tempdir())
  expect_true(is_local)
})

# result attributes -----------------------------------------------------------

testthat::test_that("Search results have a general format", {
  testthat::skip_on_cran()
  r <- search_data()
  # Is a table with expected columns and classes
  expect_true(is.data.frame(r))
  cols <- c("source", "id", "title", "abstract", "years",
            "url", "source_id", "source_id_url")
  expect_true(all(colnames(r) %in% cols))
  # A NULL search returns the full list
  expect_true(nrow(r) > 2)
  # Sources are EDI
  expect_true(all(unique(r$source) %in% c("EDI")))
})


testthat::test_that("Some info is expected by all sources", {
  testthat::skip_on_cran()
  r <- search_data()
  expect_true(all(!is.na(r$source)))
  expect_true(all(!is.na(r$id)))
  expect_true(all(!is.na(r$title)))
  expect_true(all(!is.na(r$abstract)))
  expect_true(all(!is.na(r$source_id)))
  expect_true(all(!is.na(r$source_id_url)))
})
