# library(hymetDP)
library(EDIutils)
library(magrittr)
library(dplyr)
library(lubridate)
library(xml2)

create_hymetDP <- function(path,
                           source_id,
                           derived_id,
                           url = NULL) {

  # Read source dataset -------------------------------------------------

  eml <- read_metadata(source_id)

  tables <- read_tables(
    eml = eml,
    strip.white = TRUE,
    na.strings = "",
    convert.missing.value = TRUE,
    add.units = TRUE)

  # Join and flatten the source dataset ---------------------------------------

  # Create a wide table: join all tables and drop unnecessary columns

  wide <-

  # rm('table')

  # Specify timezone/offset

  wide$LocalDateTime <- lubridate::force_tz(wide$"<ENTERDATETIMECOLNAME>", "<ENTERLOCALTIMEZONE>")

  wide$UTCOffset <- lutz::tz_offset(wide$LocalDateTime, "<ENTERLOCALTIMEZONE>")$utc_offset_h

  wide$DateTimeUTC <- lubridate::with_tz(wide$LocalDateTime, "Etc/UTC")

  # Flatten ----------------------------------------------------------------

  # Create a flat table: similar to the wide table but gathered on core variables

  flat <-

  #rm('wide')

  # Add columns for the data values table -----------------------------------

  flat$ValueID <- seq(nrow(flat))

  # Assign variable code, define variable code function ---------------------

  # Define variables using the ODM Controlled vocabularies

  # View(VariableNameCV)              # variable_name
  # View(UnitsCV)                     # variable_units and time_units
  # View(SampleMediumCV)              # sample_medium
  # View(ValueTypeCV)                 # value_type
  # View(DataTypeCV)                  # data_type
  # View(GeneralCategoryCV)           # general_category

  flat <- hymetDP::define_variable(
    L0_flat = flat,
    local_variable_column = "variable_name",
    local_variable = "",
    variable_name = "",
    variable_units = NULL,
    sample_medium = "",
    value_type = "",
    is_regular = TRUE,
    time_support = 10,
    time_units = "minute",
    data_type = "",
    general_category = "",
    no_data = -9999)

  # define_methods ---------------------------------------------------------

  # Define methods and link to specific variables (by code or by name)

  flat <- hymetDP::define_method(
    L0_flat = flat,
    local_variable_column = "variable_name",
    local_variable = NULL,
    VariableCode = 1,
    MethodDescription = "",
    MethodLink = NULL)

  # Add columns for the sites table -----------------------------------

  geo_cov <- eml %>%
    xml2::xml_find_all('.//geographicCoverage')

  site_name <- geo_cov %>%
    xml2::xml_find_all('.//geographicDescription') %>%
    xml2::xml_text()

  bounds <- geo_cov %>%
    xml2::xml_find_all('.//boundingCoordinates')
  lon <- lapply(bounds, function(x) {
    mean(
      xml_double(xml2::xml_find_all(x, './/westBoundingCoordinate')),
      xml_double(xml2::xml_find_all(x, './/eastBoundingCoordinate')))
  })

  lat <- lapply(bounds, function(x) {
    mean(
      xml_double(xml2::xml_find_all(x, './/northBoundingCoordinate')),
      xml_double(xml2::xml_find_all(x, './/southBoundingCoordinate')))
  })


  elev <- lapply(bounds, function(x) {
    mean(
      xml_double(xml2::xml_find_all(x, './/boundingAltitudes/altitudeMinimum')),
      xml_double(xml2::xml_find_all(x, './/boundingAltitudes/altitudeMaximum')))
  })

  if (!is.na(elev) & xml_text(xml2::xml_find_all(bounds, './/boundingAltitudes/altitudeUnits')) != 'meter') warning("Altitude must be converted to meter for hymetDP")

  # create a Sites table

  # View(SpatialReferencesCV) # LatLongDatumSRSName
  # View(SiteTypeCV)          # SiteType

  geo_table <- tibble::tibble(
    SiteCode = unlist(lapply(seq_along(site_name), as.numeric)),
    SiteName = unlist(lapply(site_name, as.character)),
    Latitude = unlist(lapply(lat, as.numeric)),
    Longitude = unlist(lapply(lon, as.numeric)),
    Elevation_m = unlist(lapply(elev, as.numeric)),
    SiteType = "")

  flat <- flat %>% dplyr::left_join(geo_table, by = 'SiteCode')

  flat$SiteCode <- 1
  flat$SiteName <- site_name
  flat$Latitude <- lat
  flat$Longitude <- lon
  flat$LatLongDatumSRSName <- "Unknown"
  flat$SiteType <- ""


  # Add columns for the Sources table ---------------------------------------

  flat$Organization <- ""
  flat$ContactName <- ""
  flat$Email <- ""
  flat$Address <- ""
  flat$City <- ""
  flat$State <- ""
  flat$ZipCode <- ""

  # SourceDescription, SourceLink, Citation can be handled automatically

  flat <- define_source(L0_flat = flat,
                        eml = eml)


  # Add columns for the Quality Control Level table ----------------------------------------

  flat$QualityControlLevelCode <- 1
  flat$Definition <- ""
  flat$Explanation <- ""

  # Add columns for the optional tables -------------------------------------------

  # Odds and ends -------------------------------------------------------------

  # Parse flat into hymetDP required tables -------------------------------------------

  Sources <- hymetDP::create_sources(
    L0_flat = flat,
    SourceCode = "SourceCode",
    Organization = "Organization",
    SourceDescription = "SourceDescription",
    SourceLink = "SourceLink",
    ContactName = "ContactName",
    Phone = "Phone",
    Email = "Email",
    Address = "Address",
    City = "City",
    State = "State",
    ZipCode = "ZipCode",
    Citation = "Citation")

  Methods <- hymetDP::create_methods(
    L0_flat = flat,
    MethodCode = "MethodCode",
    MethodDescription = "MethodDescription")

  Variables <- hymetDP::create_variables(
    L0_flat = flat,
    VariableCode = "VariableCode",
    VariableName = "VariableName",
    VariableUnitsName = "VariableUnitsName",
    SampleMedium = "SampleMedium",
    ValueType = "ValueType",
    IsRegular = "IsRegular",
    TimeSupport = "TimeSupport",
    TimeUnitsName = "TimeUnitsName",
    DataType = "DataType",
    GeneralCategory = "GeneralCategory",
    NoDataValue = "NoDataValue")

  Sites <- hymetDP::create_sites(
    L0_flat = flat,
    SiteCode = "SiteCode",
    SiteName = "SiteName",
    Latitude = "Latitude",
    Longitude = "Longitude",
    LatLongDatumSRSName = "LatLongDatumSRSName",
    Elevation_m = NULL,
    VerticalDatum = NULL,
    LocalX = NULL,
    LocalY = NULL,
    LocalProjectionSRSName = NULL,
    PosAccuracy_m = NULL,
    State = NULL,
    County = NULL,
    Comments = NULL,
    SiteType = "SiteType")

  QualityControlLevels <- hymetDP::create_quality_control(
    L0_flat = flat,
    QualityControlLevelCode = "QualityControlLevelCode",
    Definition = "Definition",
    Explanation = "Explanation"
  )




  # Parse flat into hymetDP optional tables ---------------------------------

  #qualifiers <- hymetDP::create_qualifiers()

  #samples <- hymetDP::create_samples()

  #lab_methods <- hymetDP::create_lab_methods()

  #categories <- hymetDP::create_categories()

  #derived_from <- hymetDP::create_derived_from() # NOT ADDED YET

  #groups <- hymetDP::create_groups() # NOT ADDED YET

  #group_descriptions <- hymetDP::create_group_descriptions() # NOT ADDED YET

  #offset_types <- hymetDP::create_offset_types()


  # Parse flat into the DataValues table ------------------------------------

  DataValues <- hymetDP::create_data_values(
    L0_flat = flat,
    ValueID = "ValueID",
    DataValue = "DataValue",
    ValueAccuracy = NULL,
    LocalDateTime = "LocalDateTime",
    UTCOffset = "UTCOffset",
    DateTimeUTC = "DateTimeUTC",
    SiteCode = "SiteCode",
    VariableCode = "VariableCode",
    OffsetValue = NULL,
    OffsetTypeCode = NULL,
    CensorCode = NULL,
    QualifierCode = NULL,
    MethodCode = "MethodCode",
    QualityControlLevelCode = "QualityControlLevelCode",
    SourceCode = "SourceCode",
    NoDataValue = "NoDataValue")


  # Create the SeriesCatalog table ------------------------------------------

  SeriesCatalog <- hymetDP::create_series_catalog(
    Sources = Sources,
    Methods = Methods,
    Variables = Variables,
    Sites = Sites,
    QualityControlLevels = QualityControlLevels,
    DataValues = DataValues)

  # rm('flat')

  # Write hymetDP tables ----------------------------------------------------

  hymetDP::write_tables(
    path = path,
    DataValues = DataValues,
    Variables = Variables,
    Methods = Methods,
    Sources = Sources,
    Sites = Sites,
    QualityControlLevels = QualityControlLevels,
    SeriesCatalog = SeriesCatalog)

  issues <- hymetDP::validate_data(path = path)

  dataset_annotations <- c()

  # Add contact information for the author of this script and dataset

  additional_contact <- data.frame(
    givenName = '',
    surName = '',
    organizationName = '',
    electronicMailAddress = '',
    stringsAsFactors = FALSE)

  eml <- hymetDP::create_eml(
    path = path,
    source_id = source_id,
    derived_id = derived_id,
    #is_about = dataset_annotations,
    script = "create_hymetDP.R",
    script_description =
      "A function for converting <pid> to hymetDP",
    contact = additional_contact,
    user_id = '',
    user_domain = '')

}
