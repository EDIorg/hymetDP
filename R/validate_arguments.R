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


  # define_variable() -------------------------------------------------------

  if (fun.name == 'define_variable') {

    # Flat table is specified
    if (!any(class(fun.args$L0_flat) == 'data.frame')) stop("Please specify or create the \"flat\" table.", call. = FALSE)

    # Variable is specified
    if (is.null(fun.args$local_variable)) stop("Please specify at least one variable.", call. = FALSE)

    # Unit is specified or unit column exists in table
    #table <- fun.args$L0_flat
    if (is.null(fun.args$variable_units) & !"unit" %in% names(fun.args$L0_flat)) stop("A unit must be given for this variable if a \"unit\" column does not exist.", call. = FALSE)


  }


  # define_method() -------------------------------------------------------

  if (fun.name == 'define_method') {

    # Flat table is specified
    if (!any(class(fun.args$L0_flat) == 'data.frame')) stop("Please specify or create the \"flat\" table.", call. = FALSE)

    if(is.null(fun.args$method_description)) stop("Please provide a method description.", call. = FALSE)

  }


  # create_data_values() ----------------------------------------------------

  if (fun.name == 'create_data_values') {
    var_lengths <- c(length(fun.args$ValueID), length(fun.args$DataValue), length(fun.args$VariableCode))
    if (any(var_lengths != 1)) {
      stop("Only one input is allowed to 'DataValue', 'VariableCode', and 'ValueID'. Gather your primary observation variables into long format before calling this function. ", call. = FALSE)
    }

    var_lengths <- c(length(fun.args$MethodCode), length(fun.args$SourceCode))
    if (any(var_lengths != 1)) {
      stop("Only one input is allowed to 'MethodCode', and 'SourceCode'. Use create_methods and create_sources to combine methods and sources into long format before calling this function. ", call. = FALSE)
    }

  }




}
