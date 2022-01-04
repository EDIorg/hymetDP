create_sources <- function(
  L0_flat = flat,
  Organization = "Organization",
  SourceDescription = "SourceDescription",
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

  cols_to_gather <- c(Organization, SourceDescription, SourceLink, ContactName, Phone, Email, Address, City, State, ZipCode, Citation)



  res <- L0_flat %>%
    select(all_of(cols_to_gather))

  # create the citation

  res$Citation <- create_citation(eml, Citation)

  # Only look up information from EML if set is completely empty

  contact_info <- c(ContactName, Phone, Email, Address, City, State, ZipCode)

  if (is.null(contact_info)) {

    #get all fields
    res$ContactName <- paste0(xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/givenName')), ' ',
                              xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/surName')))

    res$Phone <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/phone'))), "Unknown", .)

    res$Email <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/electronicMailAddress'))), "Unknown", .)

    if (!is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address')))) {

      res$Address <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/deliveryPoint'))), "Unknown", .)

      res$City <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/city'))), "Unknown", .)

      if (xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/country')) == "USA") {

        res$State <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/administrativeArea'))), "Unknown", .)
      } else {

        res$State <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/country'))), "Unknown", .)
      }

      res$ZipCode <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/postalCode'))), "Unknown", .)
    }
  }

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

  # Primary Key

  res$SourceCode <- seq(nrow(res))

  # reorder
  res <- res %>%
    dplyr::select(SourceCode, all_of(cols_to_gather))

  return(res)

}



