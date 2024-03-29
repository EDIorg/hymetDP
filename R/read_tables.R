#'Read data tables of a data package
#'
#'@description Read data tables of a data package from the EML metadata.
#'
#'@param eml (xml_document, xml_node) EML metadata returned from
#'  \code{read_eml()}.
#'@param strip.white (logical) Strips leading and trailing whitespaces of
#'  unquoted fields. Default if FALSE.
#'@param na.strings (character) Strings to be interpreted as NA. Setting
#'  \code{na.strings = ""} converts "" to NA. By default, blank strings "" are
#'  read as is.
#'@param convert.missing.value (logical) Converts all missing value codes
#'  specified in \code{eml} (e.g. "-99999", "NaN", "Not measured") to NA.
#'  Missing value codes vary across data packages and converting to a consistent
#'  form recognized by R makes downstream use simpler. However, care must be
#'  exercised when using this argument. The author of a dataset described by
#'  \code{eml} may have defined "missing value code" to mean something different
#'  than you expect (e.g. "below detection limit") therefore reviewing the
#'  authors missing value code definitions is a good idea. Default is FALSE.
#'@param add.units (logical) If TRUE, a variable's unit of measurement will be
#'  added to the table in a separate column with a column name of the form:
#'  \code{<unit>_<variable_name>}. This argument is useful when gathering
#'  variables into a long (attribute-value) table.
#'
#'@param table.names (character) Character vector of one or more table names
#'  (\code{<objectName>} from EML) to selectively download tables.
#'
#'@return (list) List of named data frames
#'
#'@details This function uses \code{data.table::fread()} and uses default
#'  argument values if the EML based values return an error.
#'
#'  Default settings preserve the form the data were originally published in.
#'
#'@export
#'
#' @examples
#'\dontrun{
#' eml <- read_metadata('knb-lter-mcm.9003.11')
#'
#' tables <- read_tables(
#'   eml = eml,
#'   strip.white = TRUE,
#'   na.strings = "",
#'   convert.missing.value = TRUE,
#'   add.units = TRUE)
#'
#'}
#'
#'
#'
read_tables <- function(eml,
                        strip.white = FALSE,
                        na.strings = NULL,
                        convert.missing.value = NULL,
                        add.units = FALSE,
                        table.names = NULL) {


  tbl_metadata <- xml2::xml_find_all(eml, ".//dataTable/physical")

  # Use only selected tables

  if (!is.null(table.names)) {
    if (!all(stringr::str_detect(table.names, "\\.\\w+"))) {
      stop("The files specified in table.names should include file extensions. Make sure to specify an objectName.", call. = FALSE)
    }

    if (!all(table.names %in% unlist(xml2::as_list(xml2::xml_find_all(eml, ".//dataTable/physical/objectName"))))) {

      use_i <- !table.names %in% unlist(xml2::as_list(xml2::xml_find_all(eml, ".//dataTable/physical/objectName")))

      stop(paste0("There are no files named ", knitr::combine_words(table.names[use_i]), ". Please remove these from the function call."), call. = FALSE)
    }


  }


  if (!is.null(table.names) & any(table.names %in% unlist(xml2::as_list(xml2::xml_find_all(eml, ".//dataTable/physical/objectName"))))) {

    xp <- lapply(table.names, function(x) paste0("..//physical[objectName='",x,"']"))
    tbl_metadata <- xml2::xml_find_all(tbl_metadata, paste(xp, collapse="|"))
  }

  tbls <- lapply(
    tbl_metadata,
    function(x) {

      # Get physical attributes from eml
      object_name <- xml2::xml_text(
        xml2::xml_find_all(x, ".//objectName"))
      orientation <- xml2::xml_text(
        xml2::xml_find_all(x, ".//attributeOrientation"))
      header_lines <- xml2::xml_integer(
        xml2::xml_find_all(x, ".//numHeaderLines"))
      field_delimiter <- xml2::xml_text(
        xml2::xml_find_all(x, ".//fieldDelimiter"))
      url <- xml2::xml_text(
        xml2::xml_find_all(x, ".//url"))

      # Stop if orientation is not column
      # FIXME: Extend support to other orientations
      if (!("column" %in% orientation)) {
        stop("Only column oriented tables are supported at this time.",
             call. = FALSE)
      }

      # Read table based on physical attributes. If error, then try default
      # argument values.
      # FIXME: Column type/class parsing should be explicitly controlled by
      # specifications in the dataset's EML. Currently, we are defering to
      # data.table's assumptions.
      tbl <- tryCatch(
        data.table::fread(
          input = url,
          sep = field_delimiter,
          skip = header_lines - 1,
          na.strings = na.strings),
        error = function(e) {
          warning("Could not read ", object_name, ". Trying ",
                  "data.table::fread() defaults.", call. = FALSE)
          data.table::fread(input = url)})
      tbl <- as.data.frame(tbl)

      # Strip white space
      tbl <- list2DF(lapply(tbl, trimws))

      # Convert missing value codes to NA
      if (!is.null(convert.missing.value)) {
        dataTable <- xml2::xml_find_all(eml, paste0(".//dataTable[.//objectName='", object_name, "']"))
        attrs <- xml2::xml_find_all(dataTable, ".//attribute")
        for (attrname in attrs) { # iterate through EML attributes
          mvcode <- xml2::xml_text(xml2::xml_find_all(attrname, ".//missingValueCode/code"))
          if (length(mvcode) != 0) { # attribute has a missing value code
            eml_attr <- xml2::xml_text(xml2::xml_find_all(attrname, ".//attributeName"), trim = TRUE)
            tbl_cols <- trimws(colnames(tbl))
            if (eml_attr %in% tbl_cols) { # attribute has matching table column
              measurement_scale <- xml2::xml_name(xml2::xml_child(xml2::xml_find_all(attrname, "./measurementScale")))
              if (measurement_scale %in% c("nominal", "ordinal")) {         # is character
                tbl[[eml_attr]] <- convert_missing_value(tbl[[eml_attr]], mvcode, type = "character")
              } else if (measurement_scale %in% c("interval", "ratio")) {   # is numeric
                tbl[[eml_attr]] <- convert_missing_value(tbl[[eml_attr]], mvcode, type = "numeric")
              } else if (measurement_scale %in% "dateTime") {               # is datetime
                tbl[[eml_attr]] <- convert_missing_value(tbl[[eml_attr]], mvcode, type = "datetime")
              }
            }
          }
        }
      }

      # Add units
      if (isTRUE(add.units)) {
        dataTable <- xml2::xml_find_all(eml, paste0(".//dataTable[.//objectName='", object_name, "']"))
        attrs_w_units <- xml2::xml_find_all(dataTable, ".//attribute[.//standardUnit|.//customUnit]")
        if (length(attrs_w_units) != 0) {
          newcols <- paste0("unit_", xml2::xml_text(xml2::xml_find_all(attrs_w_units, ".//attributeName")))
          newvals <- xml2::xml_text(xml2::xml_find_all(attrs_w_units, ".//standardUnit|.//customUnit"))
          for (i in seq_along(newcols)) {
            tbl[[newcols[i]]] <- newvals[i]
          }
        }
      }

      tbl <- list(tbl)
      names(tbl) <- object_name
      return(tbl)

    })

  return(unlist(tbls, recursive = FALSE))

}








#' Convert missing value codes to NA
#'
#' @param v Vector of values
#' @param code (character) Missing value code
#' @param type (character) Type (class) \code{v} should be. Supported types are: "character", "numeric", "datetime"
#'
#' @return Vector of values with \code{code} replaced by NA in the class of \code{type}
#'
#' @keywords internal
#'
convert_missing_value <- function(v, code, type) {

  code <- unlist(lapply(code, function(x) if (x %in% c(".")) {x = paste0("\\", x)} else x))

  if (type == "character") {
    res <- stringr::str_replace_all(as.character(v), paste(paste0("^", code, "$"), collapse = "|"), NA_character_)
  } else if (type == "numeric") {
    res <- stringr::str_replace_all(as.character(v), paste(paste0("^", code, "$"), collapse = "|"), NA_character_)
    res <- as.numeric(res)
  } else if (type == "datetime") {
    # TODO: Parse datetime according to date time format specifier
    res <- v
  }
  return(res)
}
