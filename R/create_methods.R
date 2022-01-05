create_methods <- function(
  L0_flat = flat,
  MethodCode,
  MethodDescription,
  MethodLink = NULL) {

  validate_arguments(fun.name = "create_methods", fun.args = as.list(environment()))

  # TODO this needs to be handled differently than other entries in the table
  # TODO methods are stored "wide" in the flat table MethodDescription_1, MethodDescription_2 etc.

  cols_to_gather <- c(MethodCode, MethodDescription, MethodLink)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct()

  # add missing columns

  if (is.null(MethodLink)) {
    res$MethodLink <- NA_character_
  }

  # reorder
  res <- res %>%
    dplyr::select(MethodCode, MethodDescription, MethodLink)

  # coerce classes
  res <- coerce_table_classes(res, "Methods", class(L0_flat))
  return(res)

}
