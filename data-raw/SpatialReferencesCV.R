# This script brings in the CUAHSI ODM SpatialReferences CV

library(rvest)
library(dplyr)

# Read from CUAHSI

SpatialReferencesCV <- rvest::read_html("http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=SpatialReferences") %>%
  rvest::html_elements("#dgCV_11") %>%
  rvest::html_table(header = TRUE) %>%
  .[[1]] %>%
  dplyr::select(-1) %>%
  dplyr::mutate(IsGeographic = as.logical(IsGeographic))

# Save to /data
usethis::use_data(SpatialReferencesCV, overwrite = TRUE)
