# This script brings in the CUAHSI ODM VerticalDatum CV

library(rvest)
library(dplyr)

# Read from CUAHSI

VerticalDatumCV <- rvest::read_html("http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=VerticalDatumCV") %>%
  rvest::html_elements("#dgCV_11") %>%
  rvest::html_table(header = TRUE) %>%
  .[[1]] %>%
  dplyr::select(-1)

# Save to /data
usethis::use_data(VerticalDatumCV, overwrite = TRUE)
