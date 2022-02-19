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






# Read EML metadata from a data repository
#
# @description A wrapper function to repository specific read methods (the repository arg drives the logic).
#
# @param package.id (character) Data package identifier
#
# @return (xml_document, xml_node) EML metadata
#
read_eml <- function(package.id) {

  # Load Global Environment config

  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }

  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }

  # Get EML

  if (repository == "EDI") {
    eml <- api_read_metadata(package.id, environment)
  }

  return(eml)

}




# Read metadata
#
# @description
#     Read Metadata (EML) operation, specifying the scope, identifier, and
#     revision of the EML document to be read in the URI.
#
# @param package.id
#     (character) Package identifier composed of scope, identifier, and
#     revision (e.g. 'edi.101.1').
# @param environment
#     (character) Data repository environment to create the package in.
#     Can be: 'development', 'staging', 'production'.
#
# @return
#     ('xml_document' 'xml_node') EML metadata.
#
#
api_read_metadata <- function(package.id, environment = 'production'){

  message(paste('Retrieving EML for data package', package.id))

  ping_edi()

  r <- httr::GET(
    url = paste0(
      url_env(environment),
      '.lternet.edu/package/metadata/eml/',
      stringr::str_replace_all(package.id, '\\.', '/')
    )
  )

  eml <- httr::content(
    r,
    as = 'parsed',
    encoding = 'UTF-8'
  )

  eml

}




# Make URL for PASTA+ environment
#
# @description
#     Create the URL suffix to the PASTA+ environment specified by the
#     environment argument.
#
# @param environment
#     (character) Data repository environment to perform the evaluation in.
#     Can be: 'development', 'staging', 'production'.
#
url_env <- function(environment){

  environment <- tolower(environment)
  if (environment == 'development'){
    url_env <- 'https://pasta-d'
  } else if (environment == 'staging'){
    url_env <- 'https://pasta-s'
  } else if (environment == 'production'){
    url_env <- 'https://pasta'
  }

  url_env

}





# Is the EDI Data Repository accessible?
#
ping_edi <- function() {
  r <- httr::GET(url = "https://pasta.lternet.edu/package/eml/edi/759") # Warn if EDI is down
  if (httr::status_code(r) != 200) {
    stop("The EDI Repository is down for regular maintenance (Wednesday 01:00",
         " - 03:00 UTC). If you have reached this message outside maintenance",
         " hours, then there is an unexpected issue that will be resolved ",
         "shortly. Our apologies for the inconvenience. Please try again ",
         "later.", call. = FALSE)
  }
}






# Validate file names
#
# @description
#     Identify whether input data file names exist in the specified directory.
#
# @param path
#     (character) A character string specifying a path to the dataset working
#     directory.
# @param data.files
#     A list of character strings specifying the names of the data files of
#     your dataset.
#
# @return
#     A warning message if the data files don't exist at path, and which of
#     the input data files are missing.
#
#     The full names of files listed in the data.files argument.
#
validate_file_names <- function(path, data.files){

  # Validate file presence

  # Index data.files in path
  files <- list.files(path)
  use_i <- data.files %in% files

  # Throw an error if any data.files are missing
  if (sum(use_i) != length(data.files)){
    stop(
      paste0(
        "\nThese files don't exist in the specified directory:\n",
        paste(data.files[!use_i], collapse = "\n")
      ),
      call. = FALSE
    )
  }

  # Check file naming convention

  # Index file names that are not composed of alphanumerics and underscores
  use_i <- stringr::str_detect(
    string = tools::file_path_sans_ext(data.files),
    pattern = "([:blank:]|([:punct:]^_))"
  )

  # Issue warning if this best practice is not followed
  if (any(isTRUE(use_i))) {
    warning(
      paste0(
        "Composing file names from only alphanumerics and underscores is a ",
        "best practice. These files don't follow this recommendation:\n",
        paste(data.files[use_i], collapse = "\n"),
        "\nPlease consider renaming these files."
      ),
      call. = FALSE
    )
  }

  # Get file names

  files <- list.files(path)
  use_i <- stringr::str_detect(string = files,
                               pattern = stringr::str_c("^", data.files, collapse = "|"))
  data_files <- files[use_i]

  # Reorder file names to match input ordering

  data_files_out <- c()
  for (i in 1:length(data.files)){
    use_i <- stringr::str_detect(string = data_files,
                                 pattern = stringr::str_c("^", data.files[i], collapse = "|"))
    data_files_out[i] <- data_files[use_i]
  }

  data_files_out

}





# Parse datetime format from values
#
# @param vals (character) Vector of datetimes
#
# @details Only works for \code{vals} of the format "YYYY-MM-DD hh:mm:ss" and subsets thereof. Values in other formats will return errant formats (e.g. "07/20/2021" returns "YYYY-MM-DD hh").
#
# @return (character) Datetime format string of \code{vals}
#
parse_datetime_frmt_from_vals <- function(vals) {
  # Modify inputs for processing
  vals <- as.character(vals)
  # Best match has the fewest coercions
  na_start <- sum(is.na(vals))
  na_end <- suppressWarnings(
    c(sum(is.na(lubridate::parse_date_time(vals, "ymdHMS"))),
      sum(is.na(lubridate::parse_date_time(vals, "ymdHM"))),
      sum(is.na(lubridate::parse_date_time(vals, "ymdH"))),
      sum(is.na(lubridate::parse_date_time(vals, "ymd"))),
      sum(is.na(lubridate::parse_date_time(vals, "y")))))
  na_coerced <- na_end - na_start
  if (stats::var(na_coerced) == 0) {    # When format of vals are NA or unsupported
    frmt <- NULL
  } else {                       # When format of vals are supported
    best_match <- which(na_coerced == min(na_coerced))[1]
    frmt <- c("YYYY-MM-DD hh:mm:ss",
              "YYYY-MM-DD hh:mm",
              "YYYY-MM-DD hh",
              "YYYY-MM-DD",
              "YYYY")[best_match]
    if (min(na_coerced) != 0) {  # When the best match doesn't represent all vals
      warning("The best match '", frmt, "' may not describe all datetimes")
    }
  }
  return(frmt)
}




# Get attribute definitions from EML
#
# @param eml (xml_document, xml_node) EML metadata
#
# @return (named list) Definitions
#
# @note Duplicate names are dropped.
#
get_attr_defs <- function(eml) {
  nodes <- attrs <- xml2::xml_find_all(eml, ".//dataTable")
  nmes <- xml2::xml_text(xml2::xml_find_all(nodes, ".//attribute/attributeName"))
  defs <- xml2::xml_text(xml2::xml_find_all(nodes, ".//attribute/attributeDefinition"))
  dups <- duplicated(nmes)
  if (any(dups)) {
    # warning("Duplicate attribute names were found in the parent EML when looking up definitions for attributes of this dataset. Dropping these attributes: ", paste(nmes[dups], collapse = ", "), call. = FALSE)
    nmes <- nmes[!dups]
    defs <- defs[!dups]
  }
  res <- defs
  names(res) <- nmes
  return(res)
}





# Get provenance metadata
#
# @description
#     Add Provenance Metadata from Level-1 metadata in PASTA to an XML
#     document containing a single methods element in the request message
#     body.
#
# @param package.id
#     (character) Package identifier composed of scope, identifier, and
#     revision (e.g. 'edi.101.1').
# @param environment
#     (character) Data repository environment to create the package in.
#     Can be: 'development', 'staging', 'production'.
#
# @return
#     ("xml_document" "xml_node") EML metadata.
#
#
api_get_provenance_metadata <- function(package.id, environment = 'production'){

  message(paste('Retrieving provenance metadata for ', package.id))

  ping_edi()

  r <- httr::GET(
    url = paste0(
      url_env(environment),
      '.lternet.edu/package/provenance/eml/',
      stringr::str_replace_all(package.id, '\\.', '/')
    )
  )

  output <- httr::content(
    r,
    as = 'parsed',
    encoding = 'UTF-8'
  )

  output

}





remove_empty_templates <- function(x) {
  # Removes empty templates (NULL, data frames with 0 rows, or TextType of 0
  # characters) from the list object created by template_arguments().
  # x = template_arguments()$x
  attr_tmp <- read_template_attributes()
  use_i <- rep(F, length(x$template))
  for (i in 1:length(x$template)) {
    if (is.null(x$template[[i]]$content)) {
      use_i[i] <- T
    } else {
      if (any(attr_tmp$template_name ==
              tools::file_path_sans_ext(names(x$template[i])))) {
        if ((attr_tmp$type[
          attr_tmp$template_name ==
          tools::file_path_sans_ext(names(x$template[i]))]) == "text") {
          if (sum(nchar(unlist(x$template[[i]]))) == 0) {
            use_i[i] <- T
          }
        } else if ((attr_tmp$type[
          attr_tmp$template_name ==
          tools::file_path_sans_ext(names(x$template[i]))]) == "xml") {
          if (length(x$template[[i]]$content$taxonomicClassification) == 0) {
            use_i[i] <- T
          }
        } else {
          if (nrow(x$template[[i]]$content) == 0) {
            use_i[i] <- T
          }
        }
      }
    }
  }
  if (all(use_i)) {
    x["template"] <-list(NULL)
  } else {
    x$template[use_i] <- NULL
  }
  x
}




# Get end of line (EOL) character
#
# @description
#     Get EOL character of input file(s).
#
# @param path
#     (character) A path to the target file directory.
# @param file.name
#     (character) The target file name.
# @param os
#     (character) The operating system in which this function is called
#     called. Valid options are generated from \code{detect_os}.
#
# @return
#     A character string representation of the EOL character.
#
get_eol <- function(path, file.name, os){

  # Validate file.name

  file_name <- validate_file_names(path, file.name)

  # Detect end of line character

  if (os == 'mac'){ # Macintosh OS

    command <- paste0(
      'od -c "',
      path,
      '/',
      file.name,
      '"'
    )

    output <- system(
      command,
      intern = T
    )

    use_i <- stringr::str_detect(
      output,
      '\\\\r  \\\\n'
    )

    if (sum(use_i) > 0){
      eol <- '\\r\\n'
    } else {
      use_i <- stringr::str_detect(
        output,
        '\\\\n'
      )
      if (sum(use_i) > 0){
        eol <- '\\n'
      } else {
        eol <- '\\r'
      }
    }

  } else if ((os == 'win') | (os == 'lin')){ # Windows & Linux OS

    output <- readChar(
      paste0(
        path,
        '/',
        file.name
      ),
      nchars = 10000
    )

    eol <- parse_delim(output)

  }

  eol

}




# Detect operating system
#
# @description
#     This function uses \code{Sys.info} to detect the user's operating system
#     and outputs an abbreviated character string to be used as inputs to OS
#     specific function calls.
#
# @return
#     \item{win}{Windows OS}
#     \item{mac}{Mac OS}
#
detect_os <- function(){
  sysinfo <- Sys.info()['sysname']
  if (sysinfo == 'Darwin'){
    os <- 'mac'
  } else if (sysinfo == 'Windows'){
    os <- 'win'
  } else {
    os <- 'lin'
  }
  os
}





# Detect field delimiter of file
#
# @param path (character) Path in which \code{data.files} are found
# @param data.files (character) File names
# @param os (character) Return from \code{detect_os()}.
#
# @details Parses the verbose return from \code{data.table::fread()} to
# get the delimiter value. If this fails, then a secondary function is called utilizing the suggested \code{reader} package. If this secondary approach fails, then a default "," is returned.
#
# @return (character) Field delimiter of \code{data.files}
#
detect_delimiter <- function(path, data.files, os) {
  f <- paste0(path, "/", data.files)
  msg <- utils::capture.output(data.table::fread(f, verbose = TRUE) %>% {NULL}) # primary method
  seps <- stringr::str_extract_all(msg, "(?<=(sep=')).+(?='[:blank:])")
  sep <- unique(unlist(seps))
  if (length(sep) == 1) {
    return(sep)
  } else {
    warning("Could not detect field delimiter for ", f, ". Trying alternate ",
            "method.", call. = FALSE)
    if (!requireNamespace("reader", quietly = TRUE)) {                   # default value
      warning("Package 'reader' is required for the alternate field delimiter",
              " detection method but is not installed.", call. = FALSE)
      warning("Could not detect field delimiter for ", f, ". Defaulting to ",
              "','.", call. = FALSE)
      return(",")
    } else {                                                             # secondary method
      res <- detect_delimiter_method_2(path, data.files, detect_os())
      return(res)
    }
  }
}




# Is provenance node?
#
# @param nodeset (xml_nodeset) methods nodeset at \code{/eml:eml/dataset/methods/methodStep}
#
# @details Looks for provenance in \code{./dataSource}
#
# @return (logical) TRUE if nodeset has provenance
#
is_prov <- function(nodeset) {
  dasource <- xml2::xml_find_all(nodeset, "./dataSource")
  res <- !is_empty_nodeset(dasource)
  return(res)
}



# Is empty nodeset?
#
# @param nodeset (xml_nodeset) Any nodeset returned by the xml2 library
#
# @return (logical) TRUE if nodeset length = 0
#
is_empty_nodeset <- function(nodeset) {
  res <- length(nodeset) == 0
  return(res)
}
