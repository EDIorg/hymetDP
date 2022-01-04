#' Create the Sources table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param SourceCode (character) Column in \code{L0_flat} containing the code which identifies the organization that created the data.
#' @param Organization (character) Column in \code{L0_flat} containing the name of the organization that collected the data.
#' @param SourceDescription (character) Column in \code{L0_flat} containing the full text description of the source of the data.
#' @param SourceLink (character) Column in \code{L0_flat} containing the link to the original data file of the data source.
#' @param ContactName (character) Column in \code{L0_flat} containing the name of the contact person for the data source.
#' @param Phone (character) Column in \code{L0_flat} containing the phone number for the contact person.
#' @param Email (character) Column in \code{L0_flat} containing the email address for the contact person.
#' @param Address (character) Column in \code{L0_flat} containing the street address for the contact person.
#' @param City (character) Column in \code{L0_flat} containing the city in which the contact person is located.
#' @param State (character) Column in \code{L0_flat} containing the state in which the contact person is located.
#' @param ZipCode (character) Column in \code{L0_flat} containing the US Zip Code or country postal code.
#' @param Citation (character) Column in \code{L0_flat} containing the Text string that gives the citation to be used when the data from each source are referenced.
#'
#' @details This function appends columns to the \code{L0_flat} table and returns the augmented table.
#'
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 hymetDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.#'
#'
#' @return (tbl_df, tbl, data.frame) The Sources table.
#'
#' @examples
#'
#' @export
#'
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



