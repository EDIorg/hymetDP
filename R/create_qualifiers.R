create_qualifiers <- function(
  L0_flat = flat,
  QualifierCode,
  QualifierDescription) {

  validate_arguments(fun.name = "create_qualifiers", fun.args = as.list(environment()))

  cols_to_gather <- c(QualifierCode,
                      QualifierDescription)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct()

  # reorder
  res <- res %>%
    dplyr::select(QualifierCode,
                  QualifierDescription)

  res <- coerce_table_classes(res, "Qualifiers", class(L0_flat))
  return(res)
}
