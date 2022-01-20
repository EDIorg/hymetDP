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





#' Write tables to file
#'
#' @param path (character) A path to the directory in which the files will be written.
#' @param sep (character) Field delimiter to use when writing files. Default is comma.
#' @param DataValues (tbl_df, tbl, data.frame) The DataValues table.
#' @param Variables (tbl_df, tbl, data.frame) The Variables table.
#' @param Methods (tbl_df, tbl, data.frame) The Methods table.
#' @param Sources (tbl_df, tbl, data.frame) The Sources table.
#' @param Sites (tbl_df, tbl, data.frame) The Sites table.
#' @param QualityControlLevels (tbl_df, tbl, data.frame) The QualityControlLevels table.
#' @param SeriesCatalog (tbl_df, tbl, data.frame) The SeriesCatalog table.
#'
#' @return hymetDP tables as \code{sep} delimited files
#'
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
#'
write_tables <- function(
  path, sep = ",", DataValues = NULL, Variables = NULL,
  Methods = NULL, Sources = NULL, Sites = NULL,
  QualityControlLevels = NULL, SeriesCatalog = NULL) {

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

  if (!is.null(DataValues)) {
    message("  DataValues")
    f <- paste0(path, "/", paste0("DataValues", suffix))
    data.table::fwrite(x = DataValues, file = f, sep = sep, na = "NA")
  }

  if (!is.null(Variables)) {
    message("  Variables")
    f <- paste0(path, "/", paste0("Variables", suffix))
    data.table::fwrite(x = Variables, file = f, sep = sep, na = "NA")
  }

  if (!is.null(Methods)) {
    message("  Methods")
    f <- paste0(path, "/", paste0("Methods", suffix))
    data.table::fwrite(x = Methods, file = f, sep = sep, na = "NA")
  }

  if (!is.null(Sources)) {
    message("  Sources")
    f <- paste0(path, "/", paste0("Sources", suffix))
    data.table::fwrite(x = Sources, file = f, sep = sep, na = "NA")
  }

  if (!is.null(Sites)) {
    message("  Sites")
    f <- paste0(path, "/", paste0("Sites", suffix))
    data.table::fwrite(x = Sites, file = f, sep = sep, na = "NA")
  }

  if (!is.null(QualityControlLevels)) {
    message("  QualityControlLevels")
    f <- paste0(path, "/", paste0("QualityControlLevels", suffix))
    data.table::fwrite(x = QualityControlLevels, file = f, sep = sep, na = "NA")
  }

  if (!is.null(SeriesCatalog)) {
    message("  SeriesCatalog")
    f <- paste0(path, "/", paste0("SeriesCatalog", suffix))
    data.table::fwrite(x = SeriesCatalog, file = f, sep = sep, na = "NA")
  }

}


#' Detect the type of object input to the \code{data} parameter
#'
#' @description The \code{data} parameter, used by a few functions in the ecocomDP package, can accept different object types. \code{detect_data_type()} identifies the object type, which the calling function typically uses in some flow control logic.
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#'
#' @return (character) The type of \code{data}, which is one of:
#' \itemize{
#'   \item "dataset": The default return of \code{read_data()}
#'   \item "list_of_datasets": > 1 "dataset"
#'   \item "table": A flat table
#'   \item "list_of_tables": The named list of L1 tables (i.e. read_data()$tables)
#'   \item "dataset_old": The old, and since deprecated, return of \code{read_data()}
#'   \item "list_of_datasets_old": > 1 "dataset_old"
#' }
#'
#' Unrecognized types will thorw an error.
#'
#' @noRd
#'
detect_data_type <- function(data){
  table_names <- unique(read_criteria()$table)
  # dataset
  if (("list" %in% class(data)) & ("tables" %in% names(data))) {
    if (sum(names(data) == "tables") == 1) {
      return("dataset")
    }
  }
  # list_of_datasets
  if (("list" %in% class(data)) & (length(data) > 1)) {
    res <- lapply(data, function(x) {"tables" %in% names(x)})
    if (all(unlist(res))) {
      return("list_of_datasets")
    }
  }
  # table
  if(all(class(data) %in% c("data.frame", "tbl_df", "tbl"))){
    return("table")
  }
  # list_of_tables
  if(("list" %in% class(data)) & any(table_names %in% names(data))) {
    return("list_of_tables")
  }
  # dataset_old
  is_dataset_old <- function(x) {
    if ("list" %in% class(x)) {
      if ("tables" %in% names(x)) {
        return(FALSE)
      }
      if (length(x) == 1) {
        if ("tables" %in% names(x[[1]])) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }
  if (is_dataset_old(data)) {
    warning('Input to "data" is an old and deprecated format. Please use the ',
            'new format instead. See ?read_data for more info.', call. = FALSE)
    return("dataset_old")
  }
  # list_of_dataset_old
  if (!is.null(data) & all(unlist(lapply(data, is_dataset_old)))) {
    return("list_of_datasets_old")
  }
  # unrecognized
  stop('Input to "data" is not one of the supported types.', call. = FALSE)
}
