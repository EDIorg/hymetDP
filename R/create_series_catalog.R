#' Create the SeriesCatalog table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset,
#'   in "flat" format (see details).
#' @param Sources (tbl_df, tbl, data.frame) The Sources table.
#' @param Methods (tbl_df, tbl, data.frame) The Methods table.
#' @param Variables (tbl_df, tbl, data.frame) The Variables table.
#' @param Sites (tbl_df, tbl, data.frame) The Sites table.
#' @param QualityControlLevels (tbl_df, tbl, data.frame) The
#'   QualityControlLevels table.
#' @param DataValues (tbl_df, tbl, data.frame) The DataValues table.
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
#' @return (tbl_df, tbl, data.frame) The SeriesCatalog table.
#'
#' @export
#'
#' @examples
#'
#' flat <- hymet_L0_flat
#'
#' Sources <- hymetDP::create_sources(
#'   L0_flat = flat,
#'   SourceCode = "SourceCode",
#'   Organization = "Organization",
#'   SourceDescription = "SourceDescription",
#'   SourceLink = "SourceLink",
#'   ContactName = "ContactName",
#'   Phone = "Phone",
#'   Email = "Email",
#'   Address = "Address",
#'   City = "City",
#'   State = "State",
#'   ZipCode = "ZipCode",
#'   Citation = "Citation")
#'
#' Methods <- hymetDP::create_methods(
#'   L0_flat = flat,
#'   MethodCode = "MethodCode",
#'   MethodDescription = "MethodDescription")
#'
#' Variables <- hymetDP::create_variables(
#'   L0_flat = flat,
#'   VariableCode = "VariableCode",
#'   VariableName = "VariableName",
#'   VariableUnitsName = "VariableUnitsName",
#'   SampleMedium = "SampleMedium",
#'   ValueType = "ValueType",
#'   IsRegular = "IsRegular",
#'   TimeSupport = "TimeSupport",
#'   TimeUnitsName = "TimeUnitsName",
#'   DataType = "DataType",
#'   GeneralCategory = "GeneralCategory",
#'   NoDataValue = "NoDataValue")
#'
#' Sites <- hymetDP::create_sites(
#'   L0_flat = flat,
#'   SiteCode = "SiteCode",
#'   SiteName = "SiteName",
#'   Latitude = "Latitude",
#'   Longitude = "Longitude",
#'   LatLongDatumSRSName = NULL,
#'   Elevation_m = NULL,
#'   VerticalDatum = NULL,
#'   LocalX = NULL,
#'   LocalY = NULL,
#'   LocalProjectionSRSName = NULL,
#'   PosAccuracy_m = NULL,
#'   State = NULL,
#'   County = NULL,
#'   Comments = NULL,
#'   SiteType = "SiteType")
#'
#' QualityControlLevels <- hymetDP::create_quality_control(
#'   L0_flat = flat,
#'   QualityControlLevelCode = "QualityControlLevelCode",
#'   Definition = "Definition",
#'   Explanation = "Explanation")
#'
#' DataValues <- hymetDP::create_data_values(
#'   L0_flat = flat,
#'   ValueID = "ValueID",
#'   DataValue = "DataValue",
#'   ValueAccuracy = NULL,
#'   LocalDateTime = "LocalDateTime",
#'   UTCOffset = "UTCOffset",
#'   DateTimeUTC = "DateTimeUTC",
#'   SiteCode = "SiteCode",
#'   VariableCode = "VariableCode",
#'   OffsetValue = NULL,
#'   OffsetTypeCode = NULL,
#'   CensorCode = NULL,
#'   QualifierCode = NULL,
#'   MethodCode = "MethodCode",
#'   QualityControlLevelCode = "QualityControlLevelCode",
#'   SourceCode = "SourceCode",
#'   NoDataValue = "NoDataValue")
#'
#' SeriesCatalog <- hymetDP::create_series_catalog(
#'   Sources = Sources,
#'   Methods = Methods,
#'   Variables = Variables,
#'   Sites = Sites,
#'   QualityControlLevels = QualityControlLevels,
#'   DataValues = DataValues)
#'
#' SeriesCatalog
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

  criteria <- read_criteria()

  # TODO validate columns before proceeding to building the SeriesCatalog?

  # Create the composite key/ combination table

  composite_key <- c("VariableCode", "MethodCode", "SourceCode", "SiteCode", "QualityControlLevelCode")

  # TODO this should default to DataValues when possible, but also support flat

  # TODO vvvvvv everything below vvvvvv belongs in the "if flat is null OR all columns provided" section
  # TODO still to create is "if flat is not null AND not all columns provided"

  combinations <- DataValues %>% dplyr::select(all_of(composite_key))

  res <- kit::funique(combinations[composite_key]) %>%
    dplyr::mutate(across(everything(), as.character))

  #  Gather columns

  # Go through the required tables list, get the required cols
  # get the table, extract cols from table, and join to the res

  res <- lapply(
    c("Sources", "Methods", "Variables", "Sites", "QualityControlLevels"),
    function(t) {
     cols <- dplyr::filter(criteria, table == t, catalog == TRUE)$column

     t <- get(t)

     res %>%
       dplyr::left_join(kit::funique(t[cols]))
   }) %>%
    purrr::reduce(dplyr::left_join, by = composite_key)


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

  # TODO this section should be optimized (filter, maybe bind rows and cols, tibble)

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
             BeginDateTime = min(dv_comp$LocalDateTime, na.rm = T),
             EndDateTime = max(dv_comp$LocalDateTime, na.rm = T),
             BeginDateTimeUTC = min(dv_comp$DateTimeUTC, na.rm = T),
             EndDateTimeUTC = max(dv_comp$DateTimeUTC, na.rm = T),
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


  res <- res %>% dplyr::select(all_of(final_cols))

  # coerce classes
  # accommodate user coming from outside of scripted workflow
  if (exists('L0_flat')) {
    res <- coerce_table_classes(res, "SeriesCatalog", class(L0_flat))
  } else {
    res <- coerce_table_classes(res, "SeriesCatalog", c("tbl_df", "tbl", "data.frame"))
  }
  return(res)
}






