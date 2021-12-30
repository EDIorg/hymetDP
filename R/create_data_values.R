#' Create the DataValues table
#'
#' @param L0_flat L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param ValueID Column in \code{L0_flat} containing the identifier assigned to each unique data value.
#' @param DataValue Column in \code{L0_flat} containing the numeric value of the observation.
#' @param ValueAccuracy Optional. Column in \code{L0_flat} containing the umeric value that describes the measurement accuracy of the data value.
#' @param LocalDateTime Column in \code{L0_flat} containing local date and time at which the data value was observed.
#' @param UTCOffset Column in \code{L0_flat} containing offset in hours from UTC time of the corresponding LocalDateTime value.
#' @param DateTimeUTC Column in \code{L0_flat} containing UTC date and time at which the data value was observed.
#' @param SiteCode Column in \code{L0_flat} containing code used by organization that collects the data to identify the site.
#' @param VariableCode Column in \code{L0_flat} containing code used by the organization that collects the data to identify the variable.
#' @param OffsetValue Optional. Column in \code{L0_flat} containing distance from a datum or control point to the point at which a data value was observed.
#' @param OffsetTypeCodeMandatory if OffsetValue is used. Column in \code{L0_flat} containing code used by the organization that collects the data to identify the OffsetType.
#' @param CensorCode Column in \code{L0_flat} containing text indication of whether the data value is censored. Defaults to "nc" (Not Censored).
#' @param QualifierCode Optional. Column in \code{L0_flat} containing a flag indicating a peculiarity with a particular data value.
#' @param MethodCode Column in \code{L0_flat} containing code used by the organization that collects the data to identify the Method.
#' @param QualityControlLevelCode Column in \code{L0_flat} containing code which identifies the level of quality control that the value has been subjected to.
#' @param NoDataValue Column in \code{L0_flat} containing numeric value used to encode when a data value is not available for this variable.
#'
#' @details This function appends columns to the \code{L0_flat} table and returns the augmented table.
#'
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 hymetDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#'
#' @return
#' @export
#'
#' @examples
#'
create_data_values <- function( # TODO change the "Code used by the organization" in the roxygen to something more accurate.
  L0_flat = flat,
  ValueID,
  DataValue,
  ValueAccuracy = NULL,
  LocalDateTime,
  UTCOffset,
  DateTimeUTC = NULL,
  SiteCode,
  VariableCode,
  OffsetValue = NULL,
  OffsetTypeCode = NULL,
  CensorCode = "nc",
  QualifierCode = NULL,
  MethodCode,
  QualityControlLevelCode,
  SourceCode,
  NoDataValue) {

  validate_arguments(fun.name = "create_data_values", fun.args = as.list(environment()))

  cols_to_gather <- c(ValueID,
                      DataValue,
                      ValueAccuracy,
                      LocalDateTime,
                      UTCOffset,
                      DateTimeUTC,
                      SiteCode,
                      VariableCode,
                      OffsetValue,
                      OffsetTypeCode,
                      CensorCode,
                      QualifierCode,
                      MethodCode,
                      SourceCode,
                      QualityControlLevelCode)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::mutate(DataValue = as.numeric(DataValue)) %>%
    dplyr::mutate(DataValue = ifelse(is.na(DataValue), NoDataValue, DataValue)) %>%
    dplyr::arrange(ValueID, VariableCode)

  # TODO add UTC time

  if (is.null(DateTimeUTC)) {
    # Calculate from Local and UTCOFfset
  }

  # TODO handle SourceCode

  # reorder
  res <- res %>%
    dplyr::select(all_of(cols_to_gather))

  # TODO what is happening in the function below
  # # coerce classes
  # res <- coerce_table_classes(res, "observation", class(L0_flat))
  return(res)



}
