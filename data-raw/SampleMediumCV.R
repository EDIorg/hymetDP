# This script brings in the CUAHSI ODM SampleMedium CV

library(rvest)
library(dplyr)

# Read from CUAHSI

SampleMediumCV <- rvest::read_html("http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=SampleMediumCV") %>%
  rvest::html_elements("#dgCV_11") %>%
  rvest::html_table(header = TRUE) %>%
  .[[1]] %>%
  dplyr::select(-1)

# Save to /data
usethis::use_data(SampleMediumCV, overwrite = TRUE)
