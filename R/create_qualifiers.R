#' Create the Qualifiers table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset,
#'   in "flat" format (see details).
#' @param QualifierCode (character) Column in \code{L0_flat} containing the code
#'   to indicate a given data qualifier.
#' @param QualifierDescription (character) Column in \code{L0_flat} containing
#'   the text of the data qualifying comment, e.g., low battery voltage on
#'   sensor.
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
#' @family create optional tables
#'
#' @return
#'
#' flat <- hymet_L0_flat
#'
#' Qualifiers <- hymetDP::create_qualifiers( L0_flat = flat, QualifierCode =
#' "QualifierCode", QualifierDescription = "QualifierDescription")
#'
#' Qualifiers
#'
#'
#' @export
#'
create_qualifiers <- function(
  L0_flat,
  QualifierCode,
  QualifierDescription) {

  validate_arguments(fun.name = "create_qualifiers",
                     fun.args = as.list(environment()))

  cols_to_gather <- c(QualifierCode,
                      QualifierDescription)

  res <- L0_flat %>%
    dplyr::select(dplyr::all_of(cols_to_gather)) %>%
    dplyr::distinct() %>%
    tidyr::drop_na()

  # reorder
  res <- res %>%
    dplyr::select(QualifierCode,
                  QualifierDescription)

  res <- coerce_table_classes(res, "Qualifiers", class(L0_flat))
  return(res)
}
