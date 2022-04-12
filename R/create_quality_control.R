#' Create the QualityControlLevels table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset,
#'   in "flat" format (see details).
#' @param QualityControlLevelCode (character) Column in \code{L0_flat}
#'   containing the code used to identify the level of quality control to which
#'   data values have been subjected.
#' @param Definition (character) Column in \code{L0_flat} containing the
#'   definition of Quality Control Level. Examples: Raw Data, Quality Controlled
#'   Data. This is not to be confused with data qualifiers.
#' @param Explanation (character) Column in \code{L0_flat} containing the
#'   explanation of Quality Control Level.
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
#' @return
#'
#' @examples
#'
#' flat <- hymet_L0_flat
#'
#'   QualityControlLevels <- hymetDP::create_quality_control(
#'     L0_flat = flat,
#'     QualityControlLevelCode = "QualityControlLevelCode",
#'     Definition = "Definition",
#'     Explanation = "Explanation")
#'
#'   QualityControlLevels
#'
#' @export
#'
create_quality_control <- function(
  L0_flat = flat,
  QualityControlLevelCode,
  Definition,
  Explanation) {

  validate_arguments(fun.name = "create_quality_control_levels", fun.args = as.list(environment()))

  cols_to_gather <- c(QualityControlLevelCode,
                      Definition,
                      Explanation)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct()

  # reorder
  res <- res %>%
    dplyr::select(QualityControlLevelCode,
                  Definition,
                  Explanation)

  res <- coerce_table_classes(res, "QualityControlLevels", class(L0_flat))
  return(res)
}

