#' Read data package report
#'
#' @param packageId (character) Data package identifier
#' @param frmt (character) Format of the returned report. Can be: "xml", "html", or "char".
#' @param env (character) Repository environment. Can be: "production", "staging", or "development".
#'
#' @return (xml_document) Data package report
#'
#' @export
#'
#' @examples
#' # Read as XML
#' qualityReport <- read_data_package_report("knb-lter-knz.260.4")
#' qualityReport
#'
#' # Read as HTML
#' qualityReport <- read_data_package_report("knb-lter-knz.260.4", frmt = "html")
#' qualityReport
#'
#' # Read as character
#' qualityReport <- read_data_package_report("knb-lter-knz.260.4", frmt = "char")
#' # writeLines(qualityReport, "./data/report.txt"))
#'
read_data_package_report <- function(packageId,
                                     frmt = "xml",
                                     env = "production") {
  url <- paste0(base_url(env), "/package/report/eml/",
                paste(parse_packageId(packageId), collapse = "/"))
  if (frmt == "html") {
    resp <- httr::GET(url,
                      set_user_agent(),
                      httr::accept("text/html"),
                      handle = httr::handle(""))
    res <- httr::content(resp, as = "text", encoding = "UTF-8")
    httr::stop_for_status(resp, res)
    return(xml2::read_html(res))
  } else if (frmt %in% c("xml", "char")) {
    resp <- httr::GET(url, set_user_agent(), handle = httr::handle(""))
    res <- httr::content(resp, as = "text", encoding = "UTF-8")
    httr::stop_for_status(resp, res)
    if (frmt == "xml") {
      return(xml2::read_xml(res))
    } else if (frmt == "char") {
      char <- report2char(xml2::read_xml(res), env = env)
      return(char)
    }
  }
}



#' Parse package ID into scope, identifier, and revision
#'
#' @param package.id (character) Data packageId
#'
#' @return (list) Data package scope, identifier, and revision
#'
#' @noRd
#'
parse_packageId <- function(package.id) {
  parts <- unlist(strsplit(package.id, ".", fixed = TRUE))
  res <- list(scope = parts[1], id = parts[2], rev = parts[3])
  return(res)
}



#' Construct base URL of the EDI repository web services
#'
#' @param env (character) Data repository environment to perform the evaluation in. Can be: 'development', 'staging', 'production'.
#'
#' @return (character) Base url
#'
#' @noRd
#'
base_url <- function(env){
  env <- tolower(env)
  if (env == 'development'){
    res <- 'https://pasta-d.lternet.edu'
  } else if (env == 'staging'){
    res <- 'https://pasta-s.lternet.edu'
  } else if (env == 'production'){
    res <- 'https://pasta.lternet.edu'
  }
  return(res)
}




#' Set EDIutils user agent for http requests
#'
#' @return (request) EDIutils user agent
#'
#' @noRd
#'
set_user_agent <- function() {
  res <- httr::user_agent("https://github.com/EDIorg/EDIutils")
  return(res)
}




# Read hymetDP criteria
#
# @return (data.frame) hymetDP criteria
#
read_criteria <- function() {
  res <- data.table::fread(
    system.file('extdata', 'validation_criteria.txt', package = 'hymetDP'))
  return(res)
}




# Coerce table classes to hymetDP specifications
#
# @param tbl (data.frame) Table to coerce
# @param name (character) Table name
# @param cls (character) Class of L0_flat input.
#
# @return \code{tbl} with column classes coerced to hymetDP model specifications and of the input type specified by \code{cls}.
#
# @details Datetime columns are not coerced. These are unchanged from the input class.
#
coerce_table_classes <- function(tbl, name, cls) {

  crit <- read_criteria() %>%
    dplyr::filter(table == name) %>%
    dplyr::select(column, class) %>%
    stats::na.omit()
  for (col in colnames(tbl)) {
    colclass <- crit$class[crit$column == col]
    if (colclass == "character") {
      tbl[[col]] <- as.character(tbl[[col]])
    } else if (colclass == "numeric") {
      tbl[[col]] <- as.numeric(tbl[[col]])
    }
  }
  if (all(c("tbl_df", "tbl", "data.frame") %in% cls)) {
    tbl <- tidyr::as_tibble(tbl)
  } else {
    tbl <- as.data.frame(tbl)
  }
  return(tbl)
}




# TODO rework this for hymetdp
write_tables <- function(
  path, sep = ",", observation = NULL, location = NULL,
  taxon = NULL, dataset_summary = NULL, observation_ancillary = NULL,
  location_ancillary = NULL, taxon_ancillary = NULL, variable_mapping = NULL) {

  # Validate arguments

  if (missing(path)){
    stop('Input argument "path" is required.', call. = FALSE)
  }

  # Write tables to file

  message('Writing tables to file:')

  if (sep == ",") {
    suffix <- ".csv"
  } else {
    suffix <- ".txt"
  }

  if (!is.null(observation)) {
    message("  observation")
    f <- paste0(path, "/", paste0("observation", suffix))
    data.table::fwrite(x = observation, file = f, sep = sep, na = "NA")
  }

  if (!is.null(location)) {
    message("  location")
    f <- paste0(path, "/", paste0("location", suffix))
    data.table::fwrite(x = location, file = f, sep = sep, na = "NA")
  }

  if (!is.null(taxon)) {
    message("  taxon")
    f <- paste0(path, "/", paste0("taxon", suffix))
    data.table::fwrite(x = taxon, file = f, sep = sep, na = "NA")
  }

  if (!is.null(dataset_summary)) {
    message("  dataset_summary")
    f <- paste0(path, "/", paste0("dataset_summary", suffix))
    data.table::fwrite(x = dataset_summary, file = f, sep = sep, na = "NA")
  }

  if (!is.null(observation_ancillary)) {
    message("  observation_ancillary")
    f <- paste0(path, "/", paste0("observation_ancillary", suffix))
    data.table::fwrite(x = observation_ancillary, file = f, sep = sep, na = "NA")
  }

  if (!is.null(location_ancillary)) {
    message("  location_ancillary")
    f <- paste0(path, "/", paste0("location_ancillary", suffix))
    data.table::fwrite(x = location_ancillary, file = f, sep = sep, na = "NA")
  }

  if (!is.null(taxon_ancillary)) {
    message("  taxon_ancillary")
    f <- paste0(path, "/", paste0("taxon_ancillary", suffix))
    data.table::fwrite(x = taxon_ancillary, file = f, sep = sep, na = "NA")
  }

  if (!is.null(variable_mapping)) {
    message("  variable_mapping")
    f <- paste0(path, "/", paste0("variable_mapping", suffix))
    data.table::fwrite(x = variable_mapping, file = f, sep = sep, na = "NA")
  }

}

