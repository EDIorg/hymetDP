#' Define a hymetDP variable
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset,
#'   in "flat" format (see details).
#' @param local_variable_column (character) Column in \code{L0_flat} table
#'   containing the L0 variable name.
#' @param local_variable (character) Reference to a value in the
#'   \code{local_variable_column} from the \code{L0_flat} table to which the new
#'   hymetDP variable refers.
#' @param variable_name (character) The CUAHSI ODM Controlled Vocabulary name
#'   for the variable that was measured, observed, modeled, etc. Defaults to the
#'   \code{local_variable} value.
#' @param variable_units (character) The CUAHSI ODM Controlled Vocabulary name
#'   of units of the data values associated with a variable. Defaults to a
#'   column \code{unit} from the \code{L0_flat} table if left unspecified.
#' @param sample_medium (character) The CUAHSI ODM Controlled Vocabulary name of
#'   the medium in which the sample or observation was taken or made.
#' @param value_type (character) The CUAHSI ODM Controlled Vocabulary value that
#'   indicates how the data value was generated.
#' @param is_regular (boolean) Value indicates whether the data values are from
#'   a regularly sampled time series. Choose \code{TRUE} or \code{FALSE}.
#' @param time_support (numeric) Numerical value that indicates the time support
#'   (or temporal footprint) of the data values. 0 is used to indicate data
#'   values that are instantaneous. Other values indicate the time over which
#'   the data values are implicitly or explicitly averaged or aggregated. Goes
#'   along with \code{time_units} if \code{is_regular == TRUE}.
#' @param time_units (character) The CUAHSI ODM Controlled Vocabulary name of
#'   units of the time support. If \code{time_support == 0}, indicating an
#'   instantaneous observation, a unit needs to still be given for completeness,
#'   although it is arbitrary.
#' @param data_type (character) The CUAHSI ODM Controlled Vocabulary value that
#'   indicates how the value applies over a time interval.
#' @param general_category (character) The CUAHSI ODM Controlled Vocabulary
#'   value for general category of the data  (i.e. Hydrology).
#' @param no_data (numeric) Numeric value used to encode when a data value is
#'   not available for this variable. DataValues will be reformatted to match
#'   this value.
#'
#' @details This function appends columns to the \code{L0_flat} table and
#'   returns the augmented table.
#'
#'   "flat" format refers to the fully joined source L0 dataset in "wide" form
#'   with the exception of the core observation variables, which are in "long"
#'   form (i.e. using the variable_name, value, unit columns of the observation
#'   table). This "flat" format is the "widest" an L1 hymetDP dataset can be
#'   consistently spread due to the frequent occurrence of L0 source datasets
#'   with > 1 core observation variable.
#'
#'
#' @return (tbl_df, tbl, data.frame) An augmented version of the original flat
#'   table, with all of the original columns plus one for each of the specified
#'   variable values (i.e. variable_name, variable_units, etc.), plus the
#'   VariableCode column, with an auto-generated integer value that will become
#'   the primary key of the Variables table. Columns are only added the first
#'   time the function is run. Subsequent runs append values to the existing
#'   columns.
#'
#' @examples
#'
#' flat <- hymet_L0_flat[1:8]
#'
#' flat <- hymetDP::define_variable(
#'   L0_flat = flat,
#'   local_variable_column = "variable_name",
#'   local_variable = "DSCHRGE_RATE",
#'   variable_name = "Discharge",
#'   variable_units = "liters per second",
#'   sample_medium = "Surface water",
#'   value_type = "Derived Value",
#'   is_regular = TRUE,
#'   time_support = 15,
#'   time_units = "minute",
#'   data_type = "Continuous",
#'   general_category = "Hydrology",
#'   no_data = -9999)
#'
#' @export
#'
define_variable <- function(
  L0_flat,
  local_variable_column = "variable_name",
  local_variable = NULL,
  variable_name = local_variable,
  variable_units = NULL,
  sample_medium = "Unknown",
  value_type = "Unknown",
  is_regular = FALSE,
  time_support = 0,
  time_units = "hour",
  data_type = "Unknown",
  general_category = "Unknown",
  no_data = -9999) {

  validate_arguments(fun.name = "define_variable", fun.args = as.list(environment()))

  # Assign the L0_flat to a variable

  flat_input <- L0_flat

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

  cv <- validate_odm_terms(fun.name = "define_variable",
                           fun.args = as.list(environment()))


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

    flat_output <- flat_input %>%
      dplyr::left_join(variable_table,
                       by = stats::setNames(lvn, local_variable_column))

  } else {

    # Save the final column names

    final_columns <- names(flat_input)

    # Merge tables with a join, merge columns with coalesce, select final columns with select

    flat_output <- flat_input %>%
      dplyr::left_join(
        variable_table, by = stats::setNames(lvn, local_variable_column)) %>%
      dplyr::mutate(
        VariableCode = dplyr::coalesce(VariableCode.x, VariableCode.y),
        VariableName = dplyr::coalesce(VariableName.x, VariableName.y),
        VariableUnitsName = dplyr::coalesce(VariableUnitsName.x,
                                            VariableUnitsName.y),
        SampleMedium = dplyr::coalesce(SampleMedium.x, SampleMedium.y),
        ValueType = dplyr::coalesce(ValueType.x, ValueType.y),
        IsRegular = dplyr::coalesce(IsRegular.x, IsRegular.y),
        TimeSupport = dplyr::coalesce(TimeSupport.x, TimeSupport.y),
        TimeUnitsName = dplyr::coalesce(TimeUnitsName.x, TimeUnitsName.y),
        DataType = dplyr::coalesce(DataType.x, DataType.y),
        GeneralCategory = dplyr::coalesce(GeneralCategory.x, GeneralCategory.y),
        NoDataValue = dplyr::coalesce(NoDataValue.x, NoDataValue.y)) %>%
      dplyr::select(dplyr::all_of(final_columns))
  }


  return(flat_output)


}
