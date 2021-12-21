get_odm_controlled_vocabulary <- function(vocab) {

  if (paste0(vocab, ".rda") %in% dir(tempdir())) {
    load(paste0(tempdir(), "/", vocab, ".rda"))
  } else {

    x = c("VariableName" = "http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=VariableNameCV",
          "Units" = "http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=Units",
          "SampleMedium" = "http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=SampleMediumCV",
          "ValueType" = "http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=ValueTypeCV",
          "DataType" = "http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=DataTypeCV",
          "GeneralCategory" = "http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=GeneralCategoryCV"
          )

    message("Accessing CUAHSI...")

    odmcv <- list()

    odmcv[vocab] <- rvest::read_html(x[vocab]) %>%
      rvest::html_elements("#dgCV_11") %>%
      rvest::html_table(header = TRUE) %>%
      .[[1]] %>%
      dplyr::select(-1) %>%
      list()

    save(odmcv, file = paste0(tempdir(), "/", vocab, ".rda"), version = 3)
  }

  return(odmcv)
}
