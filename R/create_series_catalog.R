create_series_catalog <- function(
  Sources = NULL,
  Methods = NULL,
  Variables = NULL,
  Sites = NULL,
  QualityControlLevels = NULL,
  DataValues = NULL) {

  validate_arguments(fun.name = "create_variables", fun.args = as.list(environment()))

  # TODO automatically acquires the necessary columns from the mandatory tables

  # TODO provide the list of columns each table should contain (this could be moved to validate_arguments)

  criteria <- read_criteria()

  # Filter out non-mandatory tables

  required_tables <- names(formals(create_series_catalog))

  # Confirm that all mandatory tables are present, error message about missing tables

  missing_tables <- c()

  if (is.null(Sources)) missing_tables <- c(missing_tables, "Sources")
  if (is.null(Methods)) missing_tables <- c(missing_tables, "Methods")
  if (is.null(Variables)) missing_tables <- c(missing_tables, "Variables")
  if (is.null(Sites)) missing_tables <- c(missing_tables, "Sites")
  if (is.null(QualityControlLevels)) missing_tables <- c(missing_tables, "QualityControlLevels")
  if (is.null(DataValues)) missing_tables <- c(missing_tables, "DataValues")

  if (!is.null(missing_tables)) stop(paste0("This function requires all mandatory tables to create the SeriesCatalog. The following tables were not found: ", knitr::combine_words(missing_tables), "."))

  # Confirm that all neccessary columns are present

  lapply(required_tables, function(x) {
    if (!all())
  })


  # TODO this could be abstracted out and used as part of validate_tables

  # TODO calculate begin/end dates and do the ValueCount

  # TODO coerce classes
}
