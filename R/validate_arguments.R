#' Validate arguments of hymetDP functions
#'
#' @description
#'     Validate input arguments to hymetDP functions.
#'
#' @param fun.name
#'     (character) Name of function from which \code{validate_arguments()} is
#'     called.
#' @param fun.args
#'     (named list) Arguments passed to calling function and formatted as
#'     \code{as.list(environment())}.
#'
#' @details
#'     Validation checks are function specific.
#'
validate_arguments <- function(fun.name, fun.args) {


  # Parameterize ------------------------------------------------------------

  criteria <- read_criteria()


  # define_*() functions ----------------------------------------------------

  if (stringr::str_detect(fun.name, "^define_")) {

    flat <- fun.args$L0_flat

    # Flat table is specified
    if (!any(class(flat) == 'data.frame')) stop("Please specify or create the \"flat\" table.", call. = FALSE)


    # define_variable() -------------------------------------------------------

    if (fun.name == 'define_variable') {

      # Variable is specified
      if (is.null(fun.args$local_variable)) stop("Please specify at least one variable.", call. = FALSE)

      # Unit is specified or unit column exists in table
      #table <- fun.args$L0_flat
      if (is.null(fun.args$variable_units) & !"unit" %in% names(flat)) stop("A unit must be given for this variable if a \"unit\" column does not exist.", call. = FALSE)
    }


    # define_method() -------------------------------------------------------

    if (fun.name == 'define_method') {

      if(is.null(fun.args$MethodDescription)) stop("Please provide a method description.", call. = FALSE)
    }


    # define_source() ---------------------------------------------------------

    if (fun.name == 'define_source') {

      if (is.null(fun.args$Organization) & !"Organization" %in% names(flat)) stop("A source Organization must be provided.")
    }
  }


  # create_tables() -----------------------------------------------------------

  if (stringr::str_detect(fun.name, "^create_")) {

    flat <- fun.args$L0_flat
    cols <- fun.args[names(fun.args) %in% stats::na.omit(criteria$column)] # Col args can be assigned any value, so we need a map

    # Table specific checks

    if (fun.name == 'create_data_values') {

      # TODO if OffsetValue is not null, OffsetCode must exist

      var_lengths <- c(length(fun.args$ValueID), length(fun.args$DataValue), length(fun.args$VariableCode))
      if (any(var_lengths != 1)) {
        stop("Only one input is allowed to 'DataValue', 'VariableCode', and 'ValueID'. Gather your primary observation variables into long format before calling this function. ", call. = FALSE)
      }

      var_lengths <- c(length(fun.args$MethodCode), length(fun.args$SourceCode))
      if (any(var_lengths != 1)) {
        stop("Only one input is allowed to 'MethodCode', and 'SourceCode'. Use create_methods and create_sources to combine methods and sources into long format before calling this function. ", call. = FALSE)
      }

    }

    if (fun.name == 'create_sources') {

      if (is.null(fun.args$Organization)) stop("A source Organization must be provided.")

      # TODO not sure why this was here, consider deleting
      #if (all(class(eml) == c("xml_document", "xml_node")) & is.null(fun.args$SourceDescription)) stop("A valid EML document or a SourceDescription must be provided.")

      var_lengths <- c(length(fun.args$ContactName), length(fun.args$Phone), length(fun.args$Email), length(fun.args$Address), length(fun.args$City), length(fun.args$State), length(fun.args$ZipCode))
      if (length(fun.args$Organization) > 1) {
        for (i in var_lengths) {
          if (i != length(fun.args$Organization) | i == 1) stop("If multiple Organizations are specified, all other arguments must be of the same length or length == 1", call. = FALSE)
        }
      }

      # Only one sourcelink, sourcedescription, citation

      var_lengths <- c(length(fun.args$SourceLink), length(fun.args$SourceDescription), length(fun.args$Citation))
      if (any(var_lengths > 1)) {
        stop("Only one input is allowed to 'SourceLink', 'SourceDescription', and 'Citation'. This will typically be the source data package's DOI, abstract, and citation, respectively. ", call. = FALSE)
      }

    }
  }







}
