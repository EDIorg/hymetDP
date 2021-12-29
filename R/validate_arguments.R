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

    print(deparse(substitute(fun.args$L0_flat)))
    print(class(fun.args$L0_flat))

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

}
