## code to prepare `supported_USGS_params` dataset goes here

library(dplyr)

supported_USGS_params <- dplyr::tibble(
  'usgs_code' = c("00060"),
  'VariableName' = c("Streamflow"),
  'VariableUnitsName' = c("cubic feet per second"),
  'SampleMedium' = c("Surface water"),
  'ValueType' = c("Derived Value"),
  'IsRegular' = c(TRUE),
  'TimeSupport' = c(15),
  'TimeUnitsName' = c("minute"),
  'DataType' = c("Continuous"),
  'GeneralCategory' = c("Hydrology"),
  'NoDataValue' = c(-9999))

usethis::use_data(supported_USGS_params, overwrite = TRUE)
