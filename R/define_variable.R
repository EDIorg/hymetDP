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

  # Assign the argument for "table" to a variable

  flat_input <- get(table)

  # if set to null, defaults to `unit` from table. Validate returns an error if unit_<variable_code> is not in CV

  if(is.null(variable_units)) {

    unit_col <- unique(flat_input[flat_input$variable_name == local_variable,]$unit)

    if (length(unit_col) > 1) {

      # If multiple units found for one variable, use the first but issue a warning

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

  if (!"VariableCode" %in% names(flat_input)) {

    VariableCode <- 1

  } else {

    # Increment VariableCode for subsequent variables

    VariableCode <- max(flat_input$VariableCode, na.rm = TRUE) + 1

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

  # This is necessary to use setNames() in the by parameter of the join

  lvn = "local_variable_name"

  # Join variable table to flat

  if (!"VariableCode" %in% names(flat_input)) {

    # For the first variable added to flat table

    flat_output <- flat_input %>% dplyr::left_join(variable_table, by = setNames(lvn, local_variable_column))

  } else {

    # Save the final column names

    final_columns <- names(flat_input)

    # Merge tables with a join, merge columns with coalesce, select final columns with select

    flat_output <- flat_input %>%
      dplyr::left_join(variable_table, by = setNames(lvn, local_variable_column)) %>%
      dplyr::mutate(VariableCode = dplyr::coalesce(VariableCode.x, VariableCode.y),
                VariableName = dplyr::coalesce(VariableName.x, VariableName.y),
                VariableUnitsName = dplyr::coalesce(VariableUnitsName.x, VariableUnitsName.y),
                SampleMedium = dplyr::coalesce(SampleMedium.x, SampleMedium.y),
                ValueType = dplyr::coalesce(ValueType.x, ValueType.y),
                IsRegular = dplyr::coalesce(IsRegular.x, IsRegular.y),
                TimeSupport = dplyr::coalesce(TimeSupport.x, TimeSupport.y),
                TimeUnitsName = dplyr::coalesce(TimeUnitsName.x, TimeUnitsName.y),
                DataType = dplyr::coalesce(DataType.x, DataType.y),
                GeneralCategory = dplyr::coalesce(GeneralCategory.x, GeneralCategory.y),
                NoDataValue = dplyr::coalesce(NoDataValue.x, NoDataValue.y)) %>%
      dplyr::select(all_of(final_columns))
  }


  return(flat_output)


}
