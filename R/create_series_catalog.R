create_series_catalog <- function(
  L0_flat = flat,
  Sources = NULL,
  Methods = NULL,
  Variables = NULL,
  Sites = NULL,
  QualityControlLevels = NULL,
  DataValues = NULL) {

  validate_arguments(fun.name = "create_variables", fun.args = as.list(environment()))

  # TODO require either DataValues table OR flat table...

  # TODO automatically acquires the necessary columns from the mandatory tables

  # TODO provide the list of columns each table should contain (this could be moved to validate_arguments)

  criteria <- read_criteria()

  # Filter out non-mandatory tables

  required_tables <- names(formals(create_series_catalog))

  # Confirm that all mandatory tables are present, error message about missing tables

  missing_tables <- lapply(required_tables, function(x) if(is.null(get(x))) c(missing_tables, x))
  #
  # missing_tables <- c()
  #
  # if (is.null(Sources)) missing_tables <- c(missing_tables, "Sources")
  # if (is.null(Methods)) missing_tables <- c(missing_tables, "Methods")
  # if (is.null(Variables)) missing_tables <- c(missing_tables, "Variables")
  # if (is.null(Sites)) missing_tables <- c(missing_tables, "Sites")
  # if (is.null(QualityControlLevels)) missing_tables <- c(missing_tables, "QualityControlLevels")
  # if (is.null(DataValues)) missing_tables <- c(missing_tables, "DataValues")

  if (!is.null(unlist(missing_tables))) stop(paste0("This function requires all mandatory tables to create the SeriesCatalog. The following tables were not found: ", knitr::combine_words(unlist(missing_tables)), "."))

  # Confirm that all neccessary columns are present

  missing_cols <- lapply(required_tables, function(x) {

    # get required columns from criteria table

    cols <- dplyr::filter(criteria, table == x, catalog == TRUE)$column

    if (!all(cols %in% names(get(x)))) x = cols[!cols %in% names(get(x))]
  })

  if(!is.null(unlist(missing_cols))) {

    errors <- c()

    for (i in seq_along(required_tables)) {

      if (!is.null(missing_cols[[i]])) {

        table <- required_tables[[i]]
        cols <- knitr::combine_words(missing_cols[[i]])

        msg <- paste0("The ", table, " table is missing the following: ", cols, '.\n')

        errors <- c(errors, msg)
      }
    }

    stop(paste0("The following tables are missing columns:\n", stringr::str_flatten(errors)))
  }

  # TODO validate columns before proceeding to building the SeriesCatalog?

  # Create the composite key/ combination table

  composite_key <- c("VariableCode", "MethodCode", "SourceCode", "SiteCode", "QualityControlLevelCode")

  # TODO this should default to DataValues when possible, but also support flat
  combinations <- DataValues %>% select(all_of(x))

  res <- unique(combinations[composite_key]) %>%
    mutate(across(everything(), as.character))

  # TODO Gather columns

  # This looper should go through the required tables list,
  # get the required cols, get the table, extract cols from table, and join to the res

  lapply(
    required_tables,
    function(t) {
     cols <- dplyr::filter(criteria, table == t, catalog == TRUE)$column

     t <- get(t)

     res <- res %>% dplyr::left_join(unique(t[cols]))
   })


  # TODO extract from flat

  cols_to_gather <- c(
    "SiteCode",
    "SiteName",
    "VariableCode",
    "VariableName",
    "VariableUnitsName",
    "SampleMedium",
    "ValueType",
    "TimeSupport",
    "TimeUnitsName",
    "DataType",
    "GeneralCategory",
    "MethodCode",
    "MethodDescription",
    "SourceCode",
    "Organization",
    "SourceDescription",
    "Citation",
    "QualityControlLevelCode")


  # TODO calculate begin/end dates and do the ValueCount



  # TODO coerce classes
}
