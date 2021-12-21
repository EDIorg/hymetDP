# This script brings in the CUAHSI ODM GeneralCategory CV

library(rvest)
library(dplyr)

# Read from CUAHSI

GeneralCategoryCV <- rvest::read_html("http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=GeneralCategoryCV") %>%
  rvest::html_elements("#dgCV_11") %>%
  rvest::html_table(header = TRUE) %>%
  .[[1]] %>%
  dplyr::select(-1)

# Save to /data
usethis::use_data(GeneralCategoryCV, overwrite = TRUE)
