#' Validate arguments of hymetDP functions that should be using CUAHSI ODM CV terms
#'
#' @description
#'     Validate input arguments to hymetDP functions that should have direct matches to terms in CUAHSI ODM CVs.
#'
#' @param fun.name
#'     (character) Name of function from which \code{validate_omd_terms()} is
#'     called.
#' @param fun.args
#'     (named list) Arguments passed to calling function and formatted as
#'     \code{as.list(environment())}.
#'
#' @details
#'     Validation checks are function specific.
#'

validate_odm_terms <- function(fun.name, fun.args) {


  # define_variable() -------------------------------------------------------

  if (fun.name == 'define_variable') {

    # TODO this should  be put in a loop, but its difficult to automate the
    ## translation of object names to text (i.e. getting "VariableNameCV" from the object VariableNameCV)

    close_list <- list()

    # Check variable_name
    if (!check_odm_cv(fun.args$variable_name, VariableNameCV$Term)) {

      warning(paste0("The VariableName term \"", fun.args$variable_name, "\" was not found in the VariableNameCV.\n"), call. = FALSE)
      close_list <- list(close_list, "VariableName" = return_close(fun.args$variable_name, VariableNameCV$Term))}

    # Check variable_units
    if (!check_odm_cv(fun.args$variable_units, UnitsCV$UnitsName)) {

      warning(paste0("The VariableUnits term \"", fun.args$variable_units, "\" was not found in the UnitsCV.\n"), call. = FALSE)
      close_list <- list(close_list, "VariableUnits" = return_close(fun.args$variable_units, UnitsCV$UnitsName))}

    # Check sample_medium
    if (!check_odm_cv(fun.args$sample_medium, SampleMediumCV$Term)) {
      warning(paste0("The SampleMedium term \"", fun.args$sample_medium, "\" was not found in the SampleMediumCV.\n"), call. = FALSE)
      close_list <- list(close_list, "SampleMedium" = return_close(fun.args$sample_medium, SampleMediumCV$Term))}

    # Check value_type
    if (!check_odm_cv(fun.args$value_type, ValueTypeCV$Term)) {
      warning(paste0("The ValueType term \"", fun.args$value_type, "\" was not found in the ValueTypeCV.\n"), call. = FALSE)
      close_list <- list(close_list, "ValueType" = return_close(fun.args$value_type, ValueTypeCV$Term))}

    # Check time_units
    if (!check_odm_cv(fun.args$time_units, UnitsCV$UnitsName)) {
      warning(paste0("The TimeUnits term \"", fun.args$time_units, "\" was not found in the UnitsCV.\n"), call. = FALSE)
      close_list <- list(close_list, "TimeUnits" = return_close(fun.args$time_units, UnitsCV$UnitsName))}

    # Check #general_category
    if (!check_odm_cv(fun.args$general_category, GeneralCategoryCV$Term)) {
      warning(paste0("The GeneralCategory term \"", fun.args$general_category, "\" was not found in the GeneralCategoryCV.\n"), call. = FALSE)
      close_list <- list(close_list, "GeneralCategory" = return_close(fun.args$general_category, GeneralCategoryCV$Term))}

  }

  close_list <- purrr::compact(close_list)

  if (length(close_list) > 0) {
    warning("To see suggestions for similar terms in the ", paste(names(close_list), collapse = ", "), " controlled vocabularies, access the `cv` object.", call. = FALSE)
    return(close_list)
    }
}

#' Check that a term is in a ODM Controlled Vocabulary
#'
#' @param term (character) The user-provided term that needs to be validated
#' @param cv (character) The "Term" column of the corresponding CV
#'
#' @return (logical) \code{TRUE} if there is an exact match in the CV, otherwise \code{FALSE}
check_odm_cv <- function(term, cv) {term %in% cv}


#' Return values from the CV that may be close to users' term
#'
#' @param term (character) The user-provided term that needs to be validated
#' @param cv (character) The "Term" column of the corresponding CV
#'
#' @return (list) Named list with any CV entries that contain the string
return_close <- function(term, cv) cv[!is.na(cv %>% stringr::str_extract(paste0('(?i)', term)))]
