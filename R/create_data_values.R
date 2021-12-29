create_data_values <- function(
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
  CensorCode = NULL,
  QualifierCode = NULL,
  MethodCode,
  QualityControlLevelCode,
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
                      QualityControlLevelCode)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::mutate(DataValue = as.numeric(DataValue)) %>%
    dplyr::mutate(DataValue = ifelse(is.na(DataValue), NoDataValue, DataValue)) %>%
    dplyr::arrange(ValueID, VariableCode)

  # reorder
  res <- res %>%
    dplyr::select(all_of(cols_to_gather))

  # TODO what is happening in the function below
  # # coerce classes
  # res <- coerce_table_classes(res, "observation", class(L0_flat))
  return(res)



}
