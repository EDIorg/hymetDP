create_series_catalog <- function(
  Sources = Sources,
  Methods = Methods,
  Variables = Variables,
  Sites = Sites,
  QualityControlLevels = QualityControlLevels,
  DataValues = DataValues) {

  validate_arguments(fun.name = "create_variables", fun.args = as.list(environment()))

  # TODO automatically acquires the necessary columns from the mandatory tables

  # TODO provide the list of columns each table should contain

  criteria <- read_criteria()

  # TODO calculate begin/end dates and do the ValueCount

  # TODO coerce classes
}
