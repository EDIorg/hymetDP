view_odm_controlled_vocabulary <- function(vocab = 'Units') {

  # if (paste0(vocab, ".rda") %in% dir(tempdir())) {
  #   load(paste0(tempdir(), "/", vocab, ".rda"))
  # } else {
  #
  #   x = c("VariableName" = "http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=VariableNameCV",
  #         "VariableUnits" = "http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=Units")
  #
  #   message("Accessing CUAHSI...")
  #
  #   odmcv <- list()
  #
  #   odmcv[vocab] <- rvest::read_html(x[vocab]) %>%
  #     rvest::html_elements("#dgCV_11") %>%
  #     rvest::html_table(header = TRUE) %>%
  #     .[[1]] %>%
  #     dplyr::select(-1) %>%
  #     list()
  #
  #   save(odmcv, file = paste0(tempdir(), "/", vocab, ".rda"), version = 3)
  # }

  odmcv <- get_odm_controlled_vocabulary(vocab = vocab)

  message(paste0("Viewing the ODM 1.1 ", vocab, " controlled vocabulary"))

  odmcv[[vocab]]

}
