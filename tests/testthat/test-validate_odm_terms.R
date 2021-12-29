# Tests are organized around function calls (e.g. all tests listed under
# search_data() are relevant to the argument inputs to that function).

#context("validate_odm_terms()")

library(hymetDP)


# define_variable() --------------------------------------------------------------

testthat::test_that("define_variable()", {

  # variable_name is in ODM CV

  inputs <- as.list(
    list(
      "variable_name" = "Temp",
      "variable_units" = "degree celsius",
      "sample_medium" = "Unknown",
      "value_type" = "Unknown",
      "time_units" = "hour",
      "data_type" = "Unknown",
      "general_category" = "Unknown"))
  expect_warning(
    expect_warning(
      validate_odm_terms("define_variable", inputs),
      regexp = "The VariableName term \"Temp\" was not found in the VariableNameCV."),
    regexp = "To see suggestions for similar terms in the VariableName controlled vocabularies, access the `cv` object.")

  # variable_units is in ODM CV

  inputs <- as.list(
    list(
      "variable_name" = "Temperature",
      "variable_units" = "celsius",
      "sample_medium" = "Unknown",
      "value_type" = "Unknown",
      "time_units" = "hour",
      "data_type" = "Unknown",
      "general_category" = "Unknown"))
  expect_warning(
    expect_warning(
      validate_odm_terms("define_variable", inputs),
      regexp = "The VariableUnits term \"celsius\" was not found in the UnitsCV."),
    regexp = "To see suggestions for similar terms in the VariableUnits controlled vocabularies, access the `cv` object.")

  # sample_medium is in ODM CV

  inputs <- as.list(
    list(
      "variable_name" = "Temperature",
      "variable_units" = "degree celsius",
      "sample_medium" = "ai",
      "value_type" = "Unknown",
      "time_units" = "hour",
      "data_type" = "Unknown",
      "general_category" = "Unknown"))
  expect_warning(
    expect_warning(
      validate_odm_terms("define_variable", inputs),
      regexp = "The SampleMedium term \"ai\" was not found in the SampleMediumCV."),
    regexp = "To see suggestions for similar terms in the SampleMedium controlled vocabularies, access the `cv` object.")

  # value_type is in ODM CV

  inputs <- as.list(
    list(
      "variable_name" = "Temperature",
      "variable_units" = "degree celsius",
      "sample_medium" = "Unknown",
      "value_type" = "Samp",
      "time_units" = "hour",
      "data_type" = "Unknown",
      "general_category" = "Unknown"))
  expect_warning(
    expect_warning(
      validate_odm_terms("define_variable", inputs),
      regexp = "The ValueType term \"Samp\" was not found in the ValueTypeCV."),
    regexp = "To see suggestions for similar terms in the ValueType controlled vocabularies, access the `cv` object.")

  # time_units is in ODM CV

  inputs <- as.list(
    list(
      "variable_name" = "Temperature",
      "variable_units" = "degree celsius",
      "sample_medium" = "Unknown",
      "value_type" = "Unknown",
      "time_units" = "hou",
      "data_type" = "Unknown",
      "general_category" = "Unknown"))
  expect_warning(
    expect_warning(
      validate_odm_terms("define_variable", inputs),
      regexp = "The TimeUnits term \"hou\" was not found in the UnitsCV."),
    regexp = "To see suggestions for similar terms in the TimeUnits controlled vocabularies, access the `cv` object.")

  # data_type is in ODM CV

  inputs <- as.list(
    list(
      "variable_name" = "Temperature",
      "variable_units" = "degree celsius",
      "sample_medium" = "Unknown",
      "value_type" = "Unknown",
      "time_units" = "hour",
      "data_type" = "Averag",
      "general_category" = "Unknown"))
  expect_warning(
    expect_warning(
      validate_odm_terms("define_variable", inputs),
      regexp = "The DataType term \"Averag\" was not found in the DataTypeCV"),
    regexp = "To see suggestions for similar terms in the DataType controlled vocabularies, access the `cv` object.")

  # general_category is in ODM CV

  inputs <- as.list(
    list(
      "variable_name" = "Temperature",
      "variable_units" = "degree celsius",
      "sample_medium" = "Unknown",
      "value_type" = "Unknown",
      "time_units" = "hour",
      "data_type" = "Unknown",
      "general_category" = "Hydrol"))
  expect_warning(
    expect_warning(
      validate_odm_terms("define_variable", inputs),
      regexp = "The GeneralCategory term \"Hydrol\" was not found in the GeneralCategoryCV"),
    regexp = "To see suggestions for similar terms in the GeneralCategory controlled vocabularies, access the `cv` object.")



})





