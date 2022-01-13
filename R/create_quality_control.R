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

