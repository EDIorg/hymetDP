create_source <- function(
  eml,
  L0_flat = flat,
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

  validate_arguments(fun.name = "create_sources", fun.args = as.list(environment()))

  cols_to_gather <- c(Organization, SourceDescription, SourceLink, ContactName, Phone, Email, Address, City, State, ZipCode, Citation)

  res <- L0_flat %>%
    select(all_of(cols_to_gather))

  # TODO maybe SourceDescription should not be abstract. Consider this more and RFC

  if (is.null(SourceDescription)) {
    res$SourceDescription <- xml2::xml_text(xml2::xml_find_first(eml, './/abstract'))
  }

  if (is.null(SourceLink) & all(class(eml) == c("xml_document", "xml_node"))) {
    full_doi <- xml2::xml_text(xml2::xml_find_first(eml, './/alternateIdentifier'))
    doi <- substr(full_doi, 5, nchar(full_doi))
    res$SourceLink <- paste0("https://doi.org/", doi)
  }

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



  } else if (length(contact_info) < 7) {

    newContactName <- paste0(xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/givenName')), ' ',
                              xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/surName')))

    newPhone <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/phone'))), "Unknown", .)

    newEmail <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/electronicMailAddress'))), "Unknown", .)

    if (!is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address')))) {

      newAddress <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/deliveryPoint'))), "Unknown", .)

      newCity <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/city'))), "Unknown", .)

      if (xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/country')) == "USA") {

        newState <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/administrativeArea'))), "Unknown", .)
      } else {

        newState <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/country'))), "Unknown", .)
      }

      newZipCode <- ifelse(is.na(xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/postalCode'))), "Unknown", .)
    }

    if (length(c(newContactName, newPhone, newEmail, newAddress, newCity, newState, newZipCode)) > length(contact_info))

    new_source <- data.frame(
      "Organization" = res$Organization[[1]],
      "SourceDescription" = res$SourceDescription[[1]],
      "SourceLink" = res$SourceLink[[1]],
      "ContactName" = newContactName,
      "Phone" = newPhone,
      "Email" = newEmail,
      "Address" = newAddress,
      "City" = newCity,
      "State" = newState,
      "ZipCode" = newZipCode,
      "Citation" = res$Citation[[1]]
    )

    dplyr::bind_rows(res, new_source)
  }


  # TODO if (x_paramater == "Unknown") try(<to find in the eml>) else "Unknown"




  # TODO SourceCode <- length(Organization)
}

#' Create an EDI data package citation from EML hosted on the EDI Data Portal
#'
#' @param Citation (character) If anything other than NULL is passed to this function, a citation will not be generated
#'
#' @return (character) An EDI data package citation
#'
#' @examples
#'
create_citation <- function(eml = eml, Citation = Citation) {

  if (is.null(Citation)) {
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
  }

  return(Citation)

}
