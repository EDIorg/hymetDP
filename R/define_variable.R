define_variable <- function(
  table = "flat", # the name of the flat table as a character
  variable = NULL, # Must be user-provided
  variable_name = variable, # if not specified, defaults to variable_name (variable) value. Either way, needs to be in ODM CV
  variable_units = NULL, # if set to null, defaults to `unit` from table. Validate returns an error if unit_<variable_code> is not in CV
  sample_medium = "Unknown",
  value_type = "Unknown",
  is_regular = FALSE,
  time_support = 0,
  time_units = "hour",
  data_type = "Unknown",
  general_category = "Unknown",
  no_data = -9999) {

  validate_arguments(fun.name = "define_variable", fun.args = as.list(environment()))

  # if set to null, defaults to `unit` from table. Validate returns an error if unit_<variable_code> is not in CV

  if(is.null(variable_units)) {

    unit_col <- unique(flat[flat$variable_name == variable,]$unit)

    if (length(unit_col) > 1) {

      warning("Multiple units found for variable \"", variable, "\". Defaulting to first option: ", unit_col[[1]])

      variable_units <- unit_col[[1]]

    } else {

      variable_units <- unit_col
    }

  }

  # Validate against the ODM CV:

  # TODO are global variables like this ok?

  cv <<- validate_odm_terms(fun.name = "define_variable", fun.args = as.list(environment()))

  # TODO assigns variableCode and joins to flat table by "variable"

  variable_table <- dplyr::tibble(
    VariableName = variable_name,
    VariableUnitsName = variable_units,
    SampleMedium = sample_medium,
    ValueType = value_type,
    IsRegular = is_regular,
    TimeSupport = time_support,
    TimeUnitsName = time_units,
    DataType = data_type,
    GeneralCategory = general_category,
    NoDataValue = no_data
  )

}
