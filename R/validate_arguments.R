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
#' @keywords internal
#'
validate_arguments <- function(fun.name, fun.args) {


  # Parameterize ------------------------------------------------------------

  use_i <- sapply(fun.args, function(X) identical(X, quote(expr=)))
  fun.args[use_i] <- list(NULL)

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

    if (fun.name == 'create_series_catalog') {

      required_tables <- c("Sources", "Methods", "Variables", "Sites", "QualityControlLevels", "DataValues")

      # Confirm that all mandatory tables are present, error message about missing tables

      Sources = fun.args$Sources
      Methods = fun.args$Methods
      Variables = fun.args$Variables
      Sites = fun.args$Sites
      QualityControlLevels = fun.args$QualityControlLevels
      DataValues = fun.args$DataValues

      missing_tables <- c()

      missing_tables <- lapply(required_tables, function(x) if(is.null(get(x))) c(missing_tables, x))

      if (!is.null(unlist(missing_tables))) stop(paste0("This function requires all mandatory tables to create the SeriesCatalog. The following tables were not found: ", knitr::combine_words(unlist(missing_tables)), "."))

      # Confirm that all necessary columns are present

      missing_cols <- lapply(required_tables, function(x) {

        # get required columns from criteria table

        cols <- dplyr::filter(criteria, table == x, catalog == TRUE)$column

        if (!all(cols %in% names(get(x)))) x = cols[!cols %in% names(get(x))]
      })

      if(!is.null(unlist(missing_cols))) {

        errors <- c()

        for (i in seq_along(required_tables)) {

          if (!is.null(missing_cols[[i]])) {

            table <- required_tables[[i]]
            cols <- knitr::combine_words(missing_cols[[i]])

            msg <- paste0("The ", table, " table is missing the following: ", cols, '.\n')

            errors <- c(errors, msg)
          }
        }

        stop(paste0("The following tables are missing columns:\n", stringr::str_flatten(errors)))
      }
    }
  }


  # search_data() -------------------------------------------------------------

  if (fun.name == "search_data") {

    # text

    if (!is.null(fun.args$text) & !is.character(fun.args$text)) {
      stop("Input 'text' must be of class 'character'.", call. = F)
    }

    # variable_name

    if (!is.null(fun.args$variable_name) & !is.character(fun.args$variable_name)) {
      stop("Input 'variable_name' must be of class 'character'.", call. = F)
    }

    # sample_medium

    if (!is.null(fun.args$sample_medium) & !is.character(fun.args$sample_medium)) {
      stop("Input 'sample_medium' must be of class 'character'.", call. = F)
    }

    # general_category

    if (!is.null(fun.args$general_category) & !is.character(fun.args$general_category)) {
      stop("Input 'general_category' must be of class 'character'.", call. = F)
    }

    # time_support

    if (!is.null(fun.args$time_support)) {
      if (!is.numeric(fun.args$time_support)) {
        stop("Input 'time_support' must be of class 'numeric'.", call. = F)
      }
      if (length(fun.args$time_support) != 2) {
        stop(
          "Input 'time_support' must have a minimum and maximum value.",
          call. = F)
      }
    }


    # number_years

    if (!is.null(fun.args$number_years)) {
      if (!is.numeric(fun.args$number_years)) {
        stop("Input 'number_years' must be of class 'numeric'.", call. = F)
      }
      if (length(fun.args$number_years) != 2) {
        stop(
          "Input 'number_years' must have a minimum and maximum value.",
          call. = F)
      }
    }

    # starts_before

    if (!is.null(fun.args$starts_before) & !lubridate::is.Date(fun.args$starts_before)) {
      if (any(class(tryCatch(as.Date(fun.args$starts_before),
                             error=function(e) e)) == 'error')) {

        stop("Input 'starts_before' must be well-formatted date.", call. = F)
      }
    }

    # ends_after

    if (!is.null(fun.args$ends_after) & !lubridate::is.Date(fun.args$ends_after)) {
      if (any(class(tryCatch(as.Date(fun.args$ends_after),
                             error=function(e) e)) == 'error')) {

        stop("Input 'ends_after' must be well-formatted date.", call. = F)
      }
    }

    # area

    if (!is.null(fun.args$area)) {
      if (!is.numeric(fun.args$area)) {
        stop(
          "Input 'area' must be of class 'numeric'.",
          call. = F)
      }
      if (length(fun.args$area) != 4) {
        stop(
          paste0(
            "Input 'area' must have North, East, South, and West ",
            "coordinates." ),
          call. = F)
      }
    }

    # boolean

    if (!is.null(fun.args$boolean)) {
      if (!(tolower(fun.args$boolean) %in% c("and", "or"))) {
        stop(
          "Valid inputs to 'boolean' are: 'AND', 'OR'",
          call. = F)
      }
    }

  }


}
