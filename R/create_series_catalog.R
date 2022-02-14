#' Create the SeriesCatalog table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param Sources (tbl_df, tbl, data.frame) The Sources table.
#' @param Methods (tbl_df, tbl, data.frame) The Methods table.
#' @param Variables (tbl_df, tbl, data.frame) The Variables table.
#' @param Sites (tbl_df, tbl, data.frame) The Sites table.
#' @param QualityControlLevels (tbl_df, tbl, data.frame) The QualityControlLevels table.
#' @param DataValues (tbl_df, tbl, data.frame) The DataValues table.
#'
#' @details This function appends columns to the \code{L0_flat} table and returns the augmented table.
#'
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 hymetDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#'
#' @return (tbl_df, tbl, data.frame) The SeriesCatalog table.
#'
#' @export
#'
#' @examples
#'
create_series_catalog <- function(
  L0_flat = NULL,
  Sources = NULL,
  Methods = NULL,
  Variables = NULL,
  Sites = NULL,
  QualityControlLevels = NULL,
  DataValues = NULL) {

  validate_arguments(fun.name = "create_series_catalog", fun.args = as.list(environment()))

  # TODO require either DataValues table OR flat table...
  # TODO if flat is NULL, then ALL tables are required
  # All columns come from main tables
  # TODO if flat is not NULL and DataValues is NULL, use flat for:
  # creating combinations
  # creating dv_comp
  # TODO if flat is not NULL and other values are,
  # check that columns from missing tables exist in flat
  # use columns from the flat table

  # TODO move this to validate arguments
  # TODO provide the list of columns each table should contain (this could be moved to validate_arguments)

  criteria <- read_criteria()

  # TODO validate columns before proceeding to building the SeriesCatalog?

  # Create the composite key/ combination table

  composite_key <- c("VariableCode", "MethodCode", "SourceCode", "SiteCode", "QualityControlLevelCode")

  # TODO this should default to DataValues when possible, but also support flat

  # TODO vvvvvv everything below vvvvvv belongs in the "if flat is null OR all columns provided" section
  # TODO still to create is "if flat is not null AND not all columns provided"

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






