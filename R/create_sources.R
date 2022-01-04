create_sources <- function(
  L0_flat = flat,
  SourceCode,
  Organization,
  SourceDescription,
  SourceLink = NULL,
  ContactName,
  Phone,
  Email,
  Address,
  City,
  State,
  ZipCode,
  Citation) {

  validate_arguments(fun.name = "create_sources", fun.args = as.list(environment()))

  cols_to_gather <- c(SourceCode, Organization, SourceDescription, SourceLink, ContactName, Phone, Email, Address, City, State, ZipCode, Citation)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct()

  if (is.null(SourceLink)) {
    res$SourceLink <- NA_character_
  }

  # reorder
  res <- res %>%
    dplyr::select(all_of(cols_to_gather))

  # coerce classes
  res <- coerce_table_classes(res, "Sources", class(L0_flat))
  return(res)

}



