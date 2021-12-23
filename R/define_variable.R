define_variable <- function(
  table = "flat", # the name of the flat table as a character
  local_variable_column = "variable_name",
  local_variable = NULL, # Must be user-provided
  variable_name = local_variable, # if not specified, defaults to variable_name (variable) value. Either way, needs to be in ODM CV
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

    unit_col <- unique(flat[flat$variable_name == local_variable,]$unit)

    if (length(unit_col) > 1) {

      warning("Multiple units found for variable \"", local_variable, "\". Defaulting to first option: ", unit_col[[1]])

      variable_units <- unit_col[[1]]

    } else {

      variable_units <- unit_col
    }

  }

  # Validate against the ODM CV:

  # TODO are global variables like this ok?

  cv <<- validate_odm_terms(fun.name = "define_variable", fun.args = as.list(environment()))


  # If VariableCode column doesnt exist, create it and make this variable id 1

  VariableCode <-  NA_integer_

  if (!"VariableCode" %in% names(flat)) {

    VariableCode <- 1

  } else {

    VariableCode <- max(flat$VariableCode, na.rm = TRUE) + 1

  }

  # Create table of variable information

  variable_table <- dplyr::tibble(
    local_variable_name = local_variable,
    VariableCode = VariableCode,
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

  lvn = "local_variable_name"

  # Join variable table to flat

  if (!"VariableCode" %in% names(flat)) {

    flat <- flat %>% left_join(variable_table, by = setNames(lvn, local_variable_column))

  } else {

    flat <- flat %>%
      left_join(variable_table, by = setNames(lvn, local_variable_column)) %>%
      transmute(VariableCode = coalesce(VariableCode.x, VariableCode.y),
                VariableName = coalesce(variable_name.x, variable_name.y),
                VariableUnitsName = coalesce(variable_units.x, variable_units.y),
                SampleMedium = coalesce(sample_medium.x, sample_medium.y),
                ValueType = coalesce(value_type.x, value_type.y),
                IsRegular = coalesce(is_regular.x, is_regular.y),
                TimeSupport = coalesce(time_support.x, time_support.y),
                TimeUnitsName = coalesce(time_units.x, time_units.y),
                DataType = coalesce(data_type.x, data_type.y),
                GeneralCategory = coalesce(general_category.x, general_category.y),
                NoDataValue = coalesce(no_data.x, no_data.y))
  }


  return(flat)


}
