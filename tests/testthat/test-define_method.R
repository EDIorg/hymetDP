# Test the method() function
# See test-validate_arguments.R for other tests this function is involved in.

library(hymetDP)

testthat::test_that("Single method is added (no variable, no method link)", {

  flat <<- data.frame(
    "datetime" = as.Date('2021-12-21'),
    "variable_name" = c('test_var', 'another_test_var'),
    "value" = c(8.2693, 3.9628),
    "unit" = c("degree celsius", "millimeter"),
    "VariableCode" = c(1,2),
    "VariableName" = NA_character_,
    "VariableUnitsName" = NA_character_,
    "SampleMedium" = NA_character_,
    "ValueType" = NA_character_,
    "IsRegular" = NA_character_,
    "TimeSupport" = NA_character_,
    "TimeUnitsName" = NA_character_,
    "DataType" = NA_character_,
    "GeneralCategory" = NA_character_,
    "NoDataValue" = -9999)

  on.exit(flat, add = TRUE)

  res <- define_method(method_description = 'Here is the desciption of a test method.')

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue", "MethodDescription_1")

  expect_true(all(expected_cols %in% names(res)))

  expect_equal(length(unique(res$MethodDescription_1)), 1)


})

testthat::test_that("Single method is added to single variable (by variable name, no method link)", {

  flat <<- data.frame(
    "datetime" = as.Date('2021-12-21'),
    "variable_name" = c('test_var', 'another_test_var'),
    "value" = c(8.2693, 3.9628),
    "unit" = c("degree celsius", "millimeter"),
    "VariableCode" = c(1,2),
    "VariableName" = NA_character_,
    "VariableUnitsName" = NA_character_,
    "SampleMedium" = NA_character_,
    "ValueType" = NA_character_,
    "IsRegular" = NA_character_,
    "TimeSupport" = NA_character_,
    "TimeUnitsName" = NA_character_,
    "DataType" = NA_character_,
    "GeneralCategory" = NA_character_,
    "NoDataValue" = -9999)

  on.exit(flat, add = TRUE)

  res <- define_method(method_description = 'Here is the desciption of a test method.',
                       local_variable = 'test_var')

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue", "MethodDescription_1")

  expect_true(all(expected_cols %in% names(res)))

  expect_equal(length(unique(res$MethodDescription_1)), 2)

})


testthat::test_that("Single method is added to single variable (by variable code, no method link)", {

  flat <<- data.frame(
    "datetime" = as.Date('2021-12-21'),
    "variable_name" = c('test_var', 'another_test_var'),
    "value" = c(8.2693, 3.9628),
    "unit" = c("degree celsius", "millimeter"),
    "VariableCode" = c(1,2),
    "VariableName" = NA_character_,
    "VariableUnitsName" = NA_character_,
    "SampleMedium" = NA_character_,
    "ValueType" = NA_character_,
    "IsRegular" = NA_character_,
    "TimeSupport" = NA_character_,
    "TimeUnitsName" = NA_character_,
    "DataType" = NA_character_,
    "GeneralCategory" = NA_character_,
    "NoDataValue" = -9999)

  on.exit(flat, add = TRUE)

  res <- define_method(method_description = 'Here is the desciption of a test method.',
                       local_variable = 'test_var')

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue", "MethodDescription_1")

  expect_true(all(expected_cols %in% names(res)))

  expect_equal(length(unique(res$MethodDescription_1)), 2)

})
