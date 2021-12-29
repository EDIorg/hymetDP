# Test the method() function
# See test-validate_arguments.R for other tests this function is involved in.

library(hymetDP)


# Method is added (no variable) ---------------------------

testthat::test_that("Method is added (no variable)", {

  flat <- data.frame(
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

  # First method added

  flat <- define_method(L0_flat = flat, method_description = 'Here is the desciption of a test method.')
  on.exit(flat, add = TRUE)

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue", "MethodDescription_1")

  expect_true(all(expected_cols %in% names(flat)))

  expect_equal(length(unique(flat$MethodDescription_1)), 1)

  # Second method added

  res <- define_method(L0_flat = flat, method_description = 'Here is another desciption of a test method.',
                       method_link = 'https://my-method.html')

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue",
    "MethodDescription_1", "MethodDescription_2", "MethodLink_2")

  expect_true(all(expected_cols %in% names(res)))

  expect_equal(length(unique(res$MethodDescription_2)), 1)


})


# Method is added to variable by name -------------------

testthat::test_that("Method is added to variable (by name)", {

  flat <- data.frame(
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

  flat <- define_method(L0_flat = flat, method_description = 'Here is the desciption of a test method.',
                       local_variable = 'test_var')
  on.exit(flat, add = TRUE)


  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue", "MethodDescription_1")

  expect_true(all(expected_cols %in% names(flat)))

  expect_equal(length(unique(flat$MethodDescription_1)), 2)


  res <- define_method(L0_flat = flat, method_description = 'Here is the desciption of a test method.',
                       method_link = 'https://my-method.html',
                         local_variable = 'another_test_var')
  on.exit(flat, add = TRUE)


  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue",
    "MethodDescription_1", "MethodDescription_2", "MethodLink_2")

  expect_true(all(expected_cols %in% names(res)))

  expect_equal(length(unique(res$MethodDescription_2)), 2)

  expect_equal(length(unique(res$MethodLink_2)), 2)

})


# Method is added to variable (by code, no method link) -------------------

testthat::test_that("Method is added to variable (by code)", {

  flat <- data.frame(
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

  flat <- define_method(L0_flat = flat, method_description = 'Here is the desciption of a test method.',
                       variable_code = 1)
  on.exit(flat, add = TRUE)

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue", "MethodDescription_1")

  expect_true(all(expected_cols %in% names(flat)))

  expect_equal(length(unique(flat$MethodDescription_1)), 2)

  res <- define_method(L0_flat = flat, method_description = 'Here is the desciption of another test method.',
                       method_link = 'https://my-method.html',
                       variable_code = 2)

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue",
    "MethodDescription_1", "MethodDescription_2", "MethodLink_2")

  expect_true(all(expected_cols %in% names(res)))

  expect_equal(length(unique(res$MethodDescription_2)), 2)

  expect_equal(length(unique(res$MethodLink_2)), 2)

})


testthat::test_that("Method is added to multiple variables (by name)", {

  flat <- data.frame(
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

  flat <- define_method(L0_flat = flat, method_description = 'Here is the desciption of a test method.',
                        local_variable = c("test_var","another_test_var"))
  on.exit(flat, add = TRUE)

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue", "MethodDescription_1")

  expect_true(all(expected_cols %in% names(flat)))

  expect_equal(length(unique(flat$MethodDescription_1)), 1)

})

testthat::test_that("Method is added to multiple variables (by code)", {

  flat <- data.frame(
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

  flat <- define_method(L0_flat = flat, method_description = 'Here is the desciption of a test method.',
                        variable_code = c(1,2))
  on.exit(flat, add = TRUE)

  expected_cols <- c(
    "datetime", "variable_name", "value", "unit", "VariableCode", "VariableName",
    "VariableUnitsName", "SampleMedium", "ValueType", "IsRegular", "TimeSupport",
    "TimeUnitsName", "DataType", "GeneralCategory", "NoDataValue", "MethodDescription_1")

  expect_true(all(expected_cols %in% names(flat)))

  expect_equal(length(unique(flat$MethodDescription_1)), 1)

})
