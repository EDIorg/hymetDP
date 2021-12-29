# Test the define_variable() function
# See test-validate_arguments.R and test-validate_odm_terms.R for other tests
# this function is involved in.

library(hymetDP)

testthat::test_that("Single variable is added correctly", {

  flat <- data.frame(
    "datetime" = as.Date('2021-12-21'),
    "variable_name" = 'test_var',
    "value" = 8.2693,
    "unit" = "degree celsius")

  on.exit(flat, add = TRUE)

  res <- define_variable(
    L0_flat = flat,
    local_variable = 'test_var',
    variable_name = 'Temperature')

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue")

  expect_true(all(expected_cols %in% names(res)))

  expect_equal(unique(res$VariableCode), 1)

})

testthat::test_that("Multiple variables are added correctly", {

  flat <- data.frame(
    "datetime" = as.Date('2021-12-21'),
    "variable_name" = c('test_var', 'another_test_var'),
    "value" = c(8.2693, 3.9628),
    "unit" = c("degree celsius", "millimeter"))

  on.exit(flat, add = TRUE)

  flat <- define_variable(
    L0_flat = flat,
    local_variable = 'test_var',
    variable_name = 'Temperature')

  on.exit(flat, add = TRUE)

  res <- define_variable(
    L0_flat = flat,
    local_variable = 'another_test_var',
    variable_name = 'Precipitation')

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue")

  expect_true(all(expected_cols %in% names(res)))

  expect_true(length(unique(res$VariableCode)) == 2)

  expect_true(max(res$VariableCode) == 2)
})

testthat::test_that("Conflicting units are handled correctly", {

  flat <- data.frame(
    "datetime" = as.Date('2021-12-21'),
    "variable_name" = c('test_var', 'test_var'),
    "value" = c(8.2693, 3.9628),
    "unit" = c("degree celsius", "millimeter"))

  on.exit(flat, add = TRUE)

  res <- expect_warning(define_variable(
    L0_flat = flat,
    local_variable = 'test_var',
    variable_name = 'Temperature'),
    regexp = "Multiple units found for variable \"test_var\". Defaulting to first option: degree celsius")

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue")

  expect_true(all(expected_cols %in% names(res)))

  expect_true(length(unique(res$unit)) == 2)

  expect_true(length(unique(res$VariableUnitsName)) == 1)
})
