#' Create the Methods table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset,
#'   in "flat" format (see details).
#' @param MethodCode (character) Column in \code{L0_flat} containing the code
#'   used by the organization that collects the data to identify the Method.
#' @param MethodDescription (character) Column in \code{L0_flat} containing the
#'   text description of each method.
#' @param MethodLink (character) Optional. Column in \code{L0_flat} containing a
#'   link to additional reference material on the method. Should be a single
#'   valid URL.
#'
#' @details This function appends columns to the \code{L0_flat} table and
#'   returns the augmented table.
#'
#'   "flat" format refers to the fully joined source L0 dataset in "wide" form
#'   with the exception of the core observation variables, which are in "long"
#'   form (i.e. using the variable_name, value, unit columns of the observation
#'   table). This "flat" format is the "widest" an L1 hymetDP dataset can be
#'   consistently spread due to the frequent occurrence of L0 source datasets
#'   with > 1 core observation variable.
#'
#' @family create required tables
#'
#' @return (tbl_df, tbl, data.frame) The Methods table.
#'
#' @examples
#'
#' flat <- hymet_L0_flat
#'
#'  Methods <- hymetDP::create_methods(
#'    L0_flat = flat,
#'    MethodCode = "MethodCode",
#'    MethodDescription = "MethodDescription")
#'
#'  Methods
#'
#' @export
#'
create_methods <- function(
  L0_flat = flat,
  MethodCode,
  MethodDescription,
  MethodLink = NULL) {

  validate_arguments(fun.name = "create_methods", fun.args = as.list(environment()))

  cols_to_gather <- c(MethodCode, MethodDescription, MethodLink)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct()

  # add missing columns

  if (is.null(MethodLink)) {
    res$MethodLink <- NA_character_
  }

  # reorder
  res <- res %>%
    dplyr::select(MethodCode, MethodDescription, MethodLink)

  # coerce classes
  res <- coerce_table_classes(res, "Methods", class(L0_flat))
  return(res)

}
