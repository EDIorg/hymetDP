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

  if (is.null(SourceDescription) & !"SourceDescription" %in% names(flat_input)) {
    flat_input$SourceDescription <- xml2::xml_text(xml2::xml_find_first(eml, './/abstract'))
  }

  message("SourceDescription added")

  if (is.null(SourceLink) & !"SourceLink" %in% names(flat_input)) {
    if(all(class(eml) == c("xml_document", "xml_node"))) {
      full_doi <- xml2::xml_text(xml2::xml_find_first(eml, './/alternateIdentifier'))
      doi <- substr(full_doi, 5, nchar(full_doi))
      flat_input$SourceLink <- paste0("https://doi.org/", doi)
  }}

  message("SourceLink added")

  # create the citation

  if (is.null(Citation) & !"Citation" %in% names(flat_input)) {
    flat_input$Citation <- create_citation(eml, Citation)
  }

  message("Citation added")

  contact_info <- c(ContactName, Phone, Email, Address, City, State, ZipCode)
  contact_cols <- c("ContactName", "Phone", "Email", "Address", "City", "State", "ZipCode")

  # Only look up information from EML if set is completely empty. If any part of the Source info is provided, no automatic lookup.

  if (is.null(contact_info) & all(contact_cols %in% names(flat_input) == FALSE)) {

    #get all fields
    flat_input$ContactName <- paste0(xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/givenName')), ' ',
                              xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/surName')))

    eml_phone <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/phone'))
    eml_email <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/electronicMailAddress'))
    eml_point <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/deliveryPoint'))
    eml_city <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/city'))
    eml_country <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/country'))
    eml_admin <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/administrativeArea'))
    eml_postal <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/administrativeArea'))

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
#' @param Citation (character) If anything other than NULL is passed to this function, a citation will not be generated
#'
#' @return (character) An EDI data package citation
#'
#' @examples
#'
create_citation <- function(eml = eml) {

  creator_firsts <- xml2::xml_text(xml2::xml_find_all(eml, './/creator/individualName/givenName'))
  creator_lasts <- xml2::xml_text(xml2::xml_find_all(eml, './/creator/individualName/surName'))
  names <- knitr::combine_words(paste(substr(creator_firsts, 0, 1), creator_lasts, sep = ". "))
  pid <- xml2::xml_attr(eml, 'packageId')
  quality_report <- xml2::xml_ns_strip(read_data_package_report(pid))
  creation_year <- substr(xml2::xml_text(xml2::xml_find_all(quality_report, 'creationDate')), 0, 4)
  title <- xml2::xml_text(xml2::xml_find_all(eml, './/dataset/title'))
  versioned_title <- paste0(title, " ver ", parse_packageId(pid)$rev)
  full_doi <- xml2::xml_text(xml2::xml_find_first(eml, './/alternateIdentifier'))
  doi <- substr(full_doi, 5, nchar(full_doi))
  doi_link <- paste0(xml2::xml_attr(xml2::xml_find_first(eml, './/alternateIdentifier'), 'system'), "/", doi)
  doi_accessed <- paste0(doi_link, " (Accessed ", Sys.Date(), ").")

  Citation <- paste(names, creation_year, versioned_title, "Environmental Data Initiative", doi_accessed, sep = ". ")

  return(Citation)

}
