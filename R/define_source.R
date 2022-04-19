#' Define a hymetDP source
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param eml ('xml_document' 'xml_node') EML metadata.
#' @param Organization (character) Name of the organization that collected the data.
#' @param SourceDescription (character) Full text description of the source of the data. If not provided, will default to the abstract from the EML document.
#' @param SourceLink (character) Optional. Full text description of the source of the data. If not provided, will default to the DOI in the EML document.
#' @param ContactName (character) Name of the contact person for the data source. If not provided, all contact information will default to the first contact listed in the EML document.
#' @param Phone (character) Phone number for the contact person.
#' @param Email (character) Email addresss for the contact person.
#' @param Address (character) Street address for the contact person.
#' @param City (character) City in which the contact person is located.
#' @param State (character) State in which the contact person is located. Use two letter abbreviations for US. For other countries give the full country name.
#' @param ZipCode (character) US Zip Code or country postal code.
#' @param Citation (character) Text string that gives the citation to be used when the data from each source are referenced. If not provided, will default to the Citation that appears (or that would appear) on the EDI Data Portal for the EML document.
#'
#' @details This function appends columns to the \code{L0_flat} table and returns the augmented table.
#'
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 hymetDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#'
#' @return (tbl_df, tbl, data.frame) An augmented version of the original flat table, with all of the original columns plus additional columns for the Source information.
#'
#' @examples
#'
#' flat <- hymet_L0_flat[1:30]
#'
#' eml <- EDIutils::read_metadata('knb-lter-mcm.9003.11')
#'
#' flat <- define_source(
#'   L0_flat = flat,
#'   eml = eml)
#'
#' @export
#'
define_source <- function(
  L0_flat = flat,
  eml = eml,
  Organization = NULL,
  SourceDescription = NULL,
  SourceLink = NULL,
  ContactName = NULL,
  Phone = NULL,
  Email = NULL,
  Address = NULL,
  City = NULL,
  State = NULL,
  ZipCode = NULL,
  Citation = NULL) {

  validate_arguments(fun.name = "define_source", fun.args = as.list(environment()))

  flat_input <- L0_flat

  flat_input$SourceCode <- "1"

  eml_exists <- exists('eml') & all(class(eml) == c("xml_document", "xml_node"))

  if (is.null(SourceDescription) & !"SourceDescription" %in% names(flat_input) & eml_exists) {
    flat_input$SourceDescription <- xml2::xml_text(xml2::xml_find_first(eml, './/abstract'))
  } else if (!is.null(SourceDescription)) {
    flat_input$SourceDescription <- SourceDescription
  }

  message("SourceDescription added")

  if (is.null(SourceLink) & !"SourceLink" %in% names(flat_input)) {
    if(eml_exists) {
      full_doi <- xml2::xml_text(xml2::xml_find_first(eml, './/alternateIdentifier'))
      doi <- substr(full_doi, 5, nchar(full_doi))
      flat_input$SourceLink <- paste0("https://doi.org/", doi)}
  } else if (!is.null(SourceLink)) {
    flat_input$SourceLink <- SourceLink
  }

  message("SourceLink added")

  # create the citation

  if (is.null(Citation) & !"Citation" %in% names(flat_input) & eml_exists) {
    flat_input$Citation <- create_citation(eml)
  } else if (!is.null(Citation)) {
    flat_input$Citation <- Citation
  }

  message("Citation added")

  contact_info <- c(ContactName, Phone, Email, Address, City, State, ZipCode)
  contact_cols <- c("ContactName", "Phone", "Email", "Address", "City", "State", "ZipCode")

  # Only look up information from EML if set is completely empty. If any part of the Source info is provided, no automatic lookup.

  if (is.null(contact_info) & all(contact_cols %in% names(flat_input) == FALSE) & eml_exists) {

    #get all fields

  eml_phone <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/phone'))
  eml_email <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/electronicMailAddress'))
  eml_point <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/deliveryPoint'))
  eml_city <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/city'))
  eml_country <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/country'))
  eml_admin <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/administrativeArea'))
  eml_postal <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/administrativeArea'))

    flat_input$ContactName <- paste0(xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/givenName')), ' ',
                              xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/surName')))

    flat_input$Phone <- ifelse(is.na(eml_phone), "Unknown", eml_phone)

    flat_input$Email <- ifelse(is.na(eml_email), "Unknown", eml_email)

    if (!is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address')))) {

      flat_input$Address <- ifelse(is.na(eml_point), "Unknown", eml_point)

      flat_input$City <- ifelse(is.na(eml_city), "Unknown", eml_city)

      if (eml_country == "USA") {

        flat_input$State <- ifelse(is.na(eml_admin), "Unknown", eml_admin)
      } else {

        flat_input$State <- ifelse(is.na(eml_country), "Unknown", eml_country)
      }

      flat_input$ZipCode <- ifelse(is.na(eml_postal), "Unknown", eml_postal)
    }
  } else {

    # If there is one piece of contact info provided (either in the flat table or as an argument),
    # then default to the "Argument --> Column --> Unknown" scheme. No EML lookup.

    flat_input$ContactName <- ifelse(!is.null(ContactName), ContactName,
                                     ifelse("ContactName" %in% names(flat_input), flat_input$ContactName, "Unknown"))
    flat_input$Phone <- ifelse(!is.null(Phone), Phone,
                               ifelse("Phone" %in% names(flat_input), flat_input$Phone, "Unknown"))
    flat_input$Email <- ifelse(!is.null(Email), Email,
                               ifelse("Email" %in% names(flat_input), flat_input$Email, "Unknown"))
    flat_input$Address <- ifelse(!is.null(Address), Address,
                                 ifelse("Address" %in% names(flat_input), flat_input$Address, "Unknown"))
    flat_input$City <- ifelse(!is.null(City), City,
                              ifelse("City" %in% names(flat_input), flat_input$City, "Unknown"))
    flat_input$State <- ifelse(!is.null(State), State,
                               ifelse("State" %in% names(flat_input), flat_input$State, "Unknown"))
    flat_input$ZipCode <- ifelse(!is.null(ZipCode), ZipCode,
                                 ifelse("ZipCode" %in% names(flat_input), flat_input$ZipCode, "Unknown"))
  }

  message("Contact Info Added")

  return(flat_input)


  # TODO the below chunk would be useful if more than 1 source is allowed. Not sure that it is.

  # else if (length(contact_info) < 7) {
  #
  #   newContactName <- paste0(xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/givenName')), ' ',
  #                             xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/surName')))
  #
  #   newPhone <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/phone'))), "Unknown", .)
  #
  #   newEmail <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/electronicMailAddress'))), "Unknown", .)
  #
  #   if (!is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address')))) {
  #
  #     newAddress <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/deliveryPoint'))), "Unknown", .)
  #
  #     newCity <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/city'))), "Unknown", .)
  #
  #     if (xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/country')) == "USA") {
  #
  #       newState <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/administrativeArea'))), "Unknown", .)
  #     } else {
  #
  #       newState <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/country'))), "Unknown", .)
  #     }
  #
  #     newZipCode <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/postalCode'))), "Unknown", .)
  #   }
  #
  #   if (length(c(newContactName, newPhone, newEmail, newAddress, newCity, newState, newZipCode)) > length(contact_info))
  #
  #   new_source <- data.frame(
  #     "Organization" = res$Organization[[1]],
  #     "SourceDescription" = res$SourceDescription[[1]],
  #     "SourceLink" = res$SourceLink[[1]],
  #     "ContactName" = newContactName,
  #     "Phone" = newPhone,
  #     "Email" = newEmail,
  #     "Address" = newAddress,
  #     "City" = newCity,
  #     "State" = newState,
  #     "ZipCode" = newZipCode,
  #     "Citation" = res$Citation[[1]]
  #   )
  #
  #   dplyr::bind_rows(res, new_source)
  # }
}





#' Create an EDI data package citation from EML hosted on the EDI Data Portal
#'
#' @param eml (character) An EML document from which a valid EDI package identifier can be extracted.
#'
#' @return (character) An EDI data package citation.
#'
#' @examples
#'
#' @keywords internal
#'
#'

create_citation <- function(eml = eml) {
  id <- xml2::xml_attr(eml, 'packageId')

  citation_object <- httr::content(httr::GET(paste0('https://cite.edirepository.org/cite/', id, '?style=ESIP')))

  citation <- paste(citation_object$authors, citation_object$pub_year,
                    citation_object$title, citation_object$version,
                    citation_object$publisher, citation_object$doi,
                    paste0("(Accessed ", Sys.Date(), ")."))

}



