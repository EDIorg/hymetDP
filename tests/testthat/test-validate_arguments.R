# Tests are organized around function calls (e.g. all tests listed under
# search_data() are relevant to the argument inputs to that function).

#context("validate_arguments()")

library(hymetDP)


# define_*() --------------------------------------------------------------

testthat::test_that("define_variable()", {

  # flat table must exist

  inputs <- as.list(
    list(
      L0_flat = 'non-existent-flat',
      local_variable = "air_temp",
      variable_units = 'celsius'))
  expect_error(validate_arguments("define_variable", inputs),
               regexp = "Please specify or create the \"flat\" table.")

  # variable is required

  flat <- data.frame()
  on.exit(flat, add = TRUE)
  inputs <- as.list(
    list(
      L0_flat = flat,
      local_variable = "air_temp",
      variable_units = 'celsius'))
  expect_null(validate_arguments("define_variable", inputs))

  inputs <- as.list(
    list(
      L0_flat = flat,
      local_variable = NULL,
      variable_units = 'celsius'))
  expect_error(validate_arguments("define_variable", inputs),
               regexp = "Please specify at least one variable.")

  # unit is either specified or there is a unit column

  inputs <- as.list(
    list(
      L0_flat = flat,
      local_variable = "air_temp",
      variable_units = NULL))
  expect_error(validate_arguments("define_variable", inputs),
               regexp = "A unit must be given for this variable if a \"unit\" column does not exist.")

  inputs <- as.list(
    list(
      L0_flat = flat,
      local_variable = "air_temp",
      variable_units = 'celsius'))
  expect_null(validate_arguments("define_variable", inputs))

  flat <- data.frame("unit" = NA_character_)
  on.exit(flat, add = TRUE)
  inputs <- as.list(
    list(
      L0_flat = flat,
      local_variable = "air_temp",
      variable_units = NULL))
  expect_null(validate_arguments("define_variable", inputs))





})





