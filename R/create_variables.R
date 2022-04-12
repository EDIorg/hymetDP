#' Create the Variables table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset,
#'   in "flat" format (see details).
#' @param VariableCode (character) Column in \code{L0_flat} containing the user
#'   or organization-defined code to describe the variable.
#' @param VariableName (character) Column in \code{L0_flat} containing the full
#'   text name of the variable that was measured, observed, modeled, etc. Must
#'   be from the ODM CV (see \code{VariableNameCV})
#' @param VariableUnitsName (character) Column in \code{L0_flat} containing the
#'   name of units of the data values associated with a variable. Must be from
#'   the ODM CV (see \code{UnitsCV})
#' @param SampleMedium (character) Column in \code{L0_flat} containing the
#'   medium in which the sample or observation was taken or made. Must be from
#'   the ODM CV (see \code{SampleMediumCV})
#' @param ValueType (character) Column in \code{L0_flat} that indicates how the
#'   data value was generated. Must be from the ODM CV (see \code{ValueTypeCV})
#' @param IsRegular (character) Column in \code{L0_flat} that indicates whether
#'   the data values are from a regularly sampled time series.
#' @param TimeSupport (character) Column in \code{L0_flat} containing the
#'   numerical value that indicates the time support (or temporal footprint) of
#'   the data values. 0 is used to indicate data values that are instantaneous.
#'   Other values indicate the time over which the data values are implicitly or
#'   explicitly averaged or aggregated.
#' @param TimeUnitsName (character) Column in \code{L0_flat} containing the name
#'   of units of the time support. If TimeSupport is 0, indicating an
#'   instantaneous observation, a unit needs to still be given for completeness,
#'   although it is arbitrary. Must be from the ODM CV (see \code{UnitsCV})
#' @param DataType (character) Column in \code{L0_flat} that indicates how the
#'   value applies over a time interval. Must be from the ODM CV (see
#'   \code{DataTypeCV})
#' @param GeneralCategory (character) Column in \code{L0_flat} containing the
#'   general category of data. Must be from the ODM CV (see
#'   \code{GeneralCategoryCV})
#' @param NoDataValue (character) Column in \code{L0_flat} containing the
#'   numeric value used to encode when a data value is not available for this
#'   variable.
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
#' @family create required tables
#'
#' @return (tbl_df, tbl, data.frame) The Variables table.
#'
#' @examples
#'
#' flat <- hymet_L0_flat
#'
#'   Variables <- hymetDP::create_variables(
#'     L0_flat = flat,
#'     VariableCode = "VariableCode",
#'     VariableName = "VariableName",
#'     VariableUnitsName = "VariableUnitsName",
#'     SampleMedium = "SampleMedium",
#'     ValueType = "ValueType",
#'     IsRegular = "IsRegular",
#'     TimeSupport = "TimeSupport",
#'     TimeUnitsName = "TimeUnitsName",
#'     DataType = "DataType",
#'     GeneralCategory = "GeneralCategory",
#'     NoDataValue = "NoDataValue")
#'
#'   Variables
#'
#' @export
#'
create_variables <- function(
  L0_flat = flat,
  VariableCode,
  VariableName,
  VariableUnitsName,
  SampleMedium,
  ValueType,
  IsRegular,
  TimeSupport,
  TimeUnitsName,
  DataType,
  GeneralCategory,
  NoDataValue) {

  validate_arguments(fun.name = "create_variables", fun.args = as.list(environment()))

  cols_to_gather <- c(VariableCode,
                      VariableName,
                      VariableUnitsName,
                      SampleMedium,
                      ValueType,
                      IsRegular,
                      TimeSupport,
                      TimeUnitsName,
                      DataType,
                      GeneralCategory,
                      NoDataValue)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct()

  # coerce classes
  res <- coerce_table_classes(res, "Variables", class(L0_flat))

  return(res)

}
