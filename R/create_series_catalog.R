create_series_catalog <- function(
  L0_flat = NULL,
  Sources = NULL,
  Methods = NULL,
  Variables = NULL,
  Sites = NULL,
  QualityControlLevels = NULL,
  DataValues = NULL) {

  validate_arguments(fun.name = "create_variables", fun.args = as.list(environment()))

  # TODO if flat is NULL, then ALL tables are required

  # TODO if flat is not NULL and DataValues is NULL,



  # TODO require either DataValues table OR flat table...

  # TODO automatically acquires the necessary columns from the mandatory tables

  # TODO provide the list of columns each table should contain (this could be moved to validate_arguments)

  criteria <- read_criteria()

  # Filter out non-mandatory tables

  required_tables <- c("Sources", "Methods", "Variables", "Sites", "QualityControlLevels", "DataValues")

  # Confirm that all mandatory tables are present, error message about missing tables

  missing_tables <- lapply(required_tables, function(x) if(is.null(get(x))) c(missing_tables, x))

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

  combinations <- DataValues %>% select(all_of(composite_key))

  res <- unique(combinations[composite_key]) %>%
    mutate(across(everything(), as.character))

  # TODO Gather columns

  # This looper should go through the required tables list,
  # get the required cols, get the table, extract cols from table, and join to the res

  res <- lapply(
    required_tables[required_tables != "DataValues"],
    function(t) {
     cols <- dplyr::filter(criteria, table == t, catalog == TRUE)$column

     t <- get(t)

     res %>%
       dplyr::left_join(unique(t[cols]))
   }) %>%
    purrr::reduce(left_join, by = composite_key)


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


  # calculate begin/end dates and do the ValueCount

  # filter data values for each composite key combo in res


  res <- lapply(1:nrow(res),
         function(i) {

           dv_comp <- DataValues %>%
             dplyr::filter(
               VariableCode == res$VariableCode[[i]],
               MethodCode == res$MethodCode[[i]],
               SourceCode == res$SourceCode[[i]],
               SiteCode == res$SiteCode[[i]],
               QualityControlLevelCode == res$QualityControlLevelCode[[i]])

           dplyr::tibble(
             BeginDateTime = min(dv_comp$LocalDateTime),
             EndDateTime = max(dv_comp$LocalDateTime),
             BeginDateTimeUTC = min(dv_comp$DateTimeUTC),
             EndDateTimeUTC = max(dv_comp$DateTimeUTC),
             ValueCount = nrow(dv_comp))

         }) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_cols(res)

  # assign SeriesID

  res$SeriesID <- seq(nrow(res))

  #reorder

  final_cols <- criteria %>%
    dplyr::filter(table == 'SeriesCatalog', !is.na(column)) %>%
    dplyr::pull(column)


  res <- res %>% dplyr::select(final_cols)

  # coerce classes
  res <- coerce_table_classes(res, "SeriesCatalog", class(L0_flat))
  return(res)
}






