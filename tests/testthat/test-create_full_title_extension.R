library(hymetDP)

test_that("no extension is assigned if not specified or required", {
  test_table <- data.frame(title = c("My hydro package"))

  ext <- create_full_title_extension(L0_title = "My hydro package",
                              proposed_extension = "",
                              all_titles = test_table)

  expect_equal(ext, "(Reformatted to the hymetDP Design Pattern)")
})

test_that("extension is auto-assigned if not specified", {
  test_table <- data.frame(title = c("My hydro package",
                                     "My hydro package (Reformatted to the hymetDP Design Pattern)"))

  ext <- create_full_title_extension(L0_title = "My hydro package",
                                     proposed_extension = "",
                                     all_titles = test_table)

  expect_equal(ext, "(Reformatted to the hymetDP Design Pattern; Package A)")
})

test_that("next extension is auto-assigned if not specified", {
  test_table <- data.frame(title = c("My hydro package",
                                      "My hydro package (Reformatted to the hymetDP Design Pattern; Package D)"))

  ext <- create_full_title_extension(L0_title = "My hydro package",
                                     proposed_extension = "",
                                     all_titles = test_table)

  expect_equal(ext, "(Reformatted to the hymetDP Design Pattern; Package E)")
})


test_that("accepts assigned extension if no conflict", {
  test_table <- data.frame(title = c("My hydro package",
                                     "My hydro package (Reformatted to the hymetDP Design Pattern; Package A)"))

  ext <- create_full_title_extension(L0_title = "My hydro package",
                                     proposed_extension = "Sites 1, 2, 3",
                                     all_titles = test_table)

  expect_equal(ext, "(Reformatted to the hymetDP Design Pattern; Sites 1, 2, 3)")
})



test_that("rejects assigned extension and auto-assigns if there is a conflict", {
  test_table <- data.frame(title = c("My hydro package",
                                     "My hydro package (Reformatted to the hymetDP Design Pattern; Sites 1, 2, 3)",
                                     "My hydro package (Reformatted to the hymetDP Design Pattern; Package A)"))

  ext <- create_full_title_extension(L0_title = "My hydro package",
                                     proposed_extension = "Sites 1, 2, 3",
                                     all_titles = test_table)

  expect_equal(ext, "(Reformatted to the hymetDP Design Pattern; Package B)")
})
