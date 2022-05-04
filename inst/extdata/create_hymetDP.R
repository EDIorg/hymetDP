# This function creates the example dataset "hymet_L0_flat" from:
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-mcm&identifier=9003&revision=11

# Libraries used by this function

#library(hymetDP)
library(EDIutils)
library(magrittr)
library(dplyr)
library(lubridate)
library(xml2)

create_hymet_L0_flat <- function(path = NULL,
                                 source_id,
                                 derived_id = 'edi.10101.1',
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

  wide <- tables$`mcmlter-strm-h1_andersen-15min-20210303.csv`[50000:60000,]



  # rm('table')

  # Specify timezone/offset

  wide$LocalDateTime <- wide$DATE_TIME %>% strptime(format = "%m/%d/%y %H:%M", tz = "Antarctica/McMurdo") %>% as.character() %>% lubridate::as_datetime(tz = "Antarctica/McMurdo")

  wide$UTCOffset <- lutz::tz_offset(wide$LocalDateTime, "Antarctica/McMurdo")$utc_offset_h

  wide$DateTimeUTC <- lubridate::with_tz(wide$LocalDateTime, "Etc/UTC")

  # Remove unused columns
  wide <- wide %>%
    dplyr::select(
      -DATASET_CODE,
      -STRMGAGEID,
      -DATE_TIME,
      -COMMENTS)

  # Flatten ----------------------------------------------------------------

  # Create a flat table: similar to the wide table but gathered on core variables

  wide <- wide %>%
    dplyr::rename_with(
      ~paste0("DataValue.", .),
      .cols = c("DSCHRGE_RATE","WATER_TEMP","CONDUCTIVITY"))

  wide <- wide %>%
    dplyr::rename(
      Qualifier.DSCHRGE_RATE = DSCHRGE_QLTY,
      Qualifier.WATER_TEMP = WATER_TEMP_QLTY,
      Qualifier.CONDUCTIVITY = CONDUCTIVITY_QLTY,
      unit.DSCHRGE_RATE = unit_DSCHRGE_RATE,
      unit.WATER_TEMP = unit_WATER_TEMP,
      unit.CONDUCTIVITY = unit_CONDUCTIVITY)

  flat <- tidyr::pivot_longer(
    wide,
    cols = dplyr::matches(c("DSCHRGE", "WATER_TEMP", "CONDUCTIVITY")),
    names_to = c(".value", "variable_name"),
    names_sep = "\\.")

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
    local_variable = "DSCHRGE_RATE",
    variable_name = "Discharge",
    variable_units = "liters per second",
    sample_medium = "Surface water",
    value_type = "Derived Value",
    is_regular = TRUE,
    time_support = 15,
    time_units = "minute",
    data_type = "Continuous",
    general_category = "Hydrology",
    no_data = -9999)

  flat <- hymetDP::define_variable(
    L0_flat = flat,
    local_variable_column = "variable_name",
    local_variable = "WATER_TEMP",
    variable_name = "Temperature",
    variable_units = "degree celsius",
    sample_medium = "Surface water",
    value_type = "Field Observation",
    is_regular = TRUE,
    time_support = 15,
    time_units = "minute",
    data_type = "Continuous",
    general_category = "Hydrology",
    no_data = -9999)

  flat <- hymetDP::define_variable(
    L0_flat = flat,
    local_variable_column = "variable_name",
    local_variable = "CONDUCTIVITY",
    variable_name = "Specific conductance",
    variable_units = "microsiemens per centimeter",
    sample_medium = "Surface water",
    value_type = "Field Observation",
    is_regular = TRUE,
    time_support = 15,
    time_units = "minute",
    data_type = "Continuous",
    general_category = "Hydrology",
    no_data = -9999)

  # define_methods ---------------------------------------------------------

  # Define methods and link to specific variables (by code or by name)

  flat <- hymetDP::define_method(
    L0_flat = flat,
    local_variable_column = "variable_name",
    local_variable = "DSCHRGE_RATE",
    VariableCode = c(1,2,3),
    MethodDescription = "Campbell CR10 dataloggers were used to record stream stage, water temperature, and conductivity in a network of stream gages. Stage is monitored with pressure transducers; PSS-1 and PS-2 models form Paroscientific Corporation, and Accubars from Sutron Corporation. The pressure transducers measure the backpressure in orifice lines set into or above controls in the stream channel. In addition, some of the sites monitor water temperature and conductivity with either USGS minimonitor probes, or Campbell temperature/conductivity probes. Ratings are developed for the stage/discharge relationship at each site by measuring streamflow with current meters or portable flumes, according to standard USGS methods. Datum corrections to the stage are determined by periodically surveying the elevation of the orifice line to the control and nearby reference marks. Calibrations for the temperature and conductivity are assessed by measuring these parameters with portable field meters while simultaneously noting the readings from the gage probes. Data is downloaded into Campbell storage modules, and retrieved into pcs. From there, the data is sent to a USGS computer, where time discrepancies are resolved, and the data is loaded into ADAPS, a database system developed in the USGS for maintaining and processing water data. A determination for each site as to when the stream was flowing and when it was not is made. For water temperature and conductivity, bad data is deleted. Variable shifts are determined based on field calibration measurements, and other indicators. The shifts are applied to the remaining good data inside of ADAPS. The data is pulled out of ADAPS, and reformatted for input into ORACLE. Cases of water temperature below reasonable values are set to lower limits. A quality code is assigned to every value. The resulting data is uploaded into the ORACLE and the McMurdo database. Before 2011, For stage/discharge, bad data is deleted. Survey data is reviewed to compute weir elevations an datum corrections. A rating curve is developed graphically, based on available data, and entered into ADAPS. All applicable shifts and datum corrections are entered into ADAPS. All corrections and ratings are run against the good stage data to compute the discharge at each recording interval. The data is pulled out of ADAPS, and reformatted for input into ORACLE. A quality code is assigned to every value. The resulting data is uploaded into ORACLE and the McMurdo database. ADAPS deprecated in favor of Aquarius software in 2012. Similar procedure is used in Aquarius to convert and curate the data. Metadata was enhanced in 2013 and 2014 by Inigo San Gil. In March 2021, data from the 2016-17 field season was replaced to correct a previously published error, in which discharge was reported in cubicFeetPerSecond (CFS) instead of litersPerSecond (l/s).",
    MethodLink = NULL)

  # Add columns for the sites table -----------------------------------

  geo_cov <- eml %>%
    xml2::xml_find_all('.//geographicCoverage')

  site_name <- geo_cov %>%
    xml2::xml_find_all('.//geographicDescription') %>%
    xml2::xml_text() %>%
    stringr::str_replace_all('\\n', '')

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

  flat$SiteCode <- 1
  flat$SiteName <- site_name
  flat$Latitude <- lat
  flat$Longitude <- lon
  flat$Elevation_m <- elev
  flat$SiteType <- "Stream"

  # Add columns for the Sources table ---------------------------------------

  flat$Organization <- xml2::xml_text(xml2::xml_find_first(eml, './/metadataProvider/organizationName'))
  flat$ContactName <- paste0(xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/givenName')), ' ',
                             xml2::xml_text(xml2::xml_find_first(eml, './/contact/individualName/surName')))
  flat$Email <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/electronicMailAddress'))
  # flat$Address <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/deliveryPoint'))
  # flat$City <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/city'))
  # flat$State <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/administrativeArea'))
  # flat$ZipCode <- xml2::xml_text(xml2::xml_find_first(eml, './/contact/address/postalCode'))

  # SourceDescription, SourceLink, Citation can be handled automatically

  flat <- define_source(L0_flat = flat,
                        eml = eml)


  # Add columns for the Quality Control Level table ----------------------------------------

  flat$QualityControlLevelCode <- 1
  flat$Definition <- "Quality controlled data"
  flat$Explanation <- "Quality controlled data that have passed quality assurance procedures such as routine estimation of timing and sensor calibration or visual inspection and removal of obvious errors. An example is USGS published streamflow records following parsing through USGS quality control procedures."

  # Add columns for the optional tables -------------------------------------------
  # Qualifiers

  # Clean up the codes in the table

  flat$Qualifier <- flat$Qualifier %>% tolower()

  flat$Qualifier[grepl("fair", flat$Qualifier, ignore.case=FALSE)] <- "fair"
  flat$Qualifier[grepl("good", flat$Qualifier, ignore.case=FALSE)] <- "good"
  flat$Qualifier[grepl("poor", flat$Qualifier, ignore.case=FALSE)] <- "poor"
  flat$Qualifier[grepl("[^fair|good|missing|poor]", flat$Qualifier, ignore.case=FALSE)] <- NA_character_


  flat <- dplyr::mutate(
    flat,
    QualifierCode = Qualifier
  ) %>%
    dplyr::left_join(
      data.frame(
        "QualifierCode" = c("good", "fair", "poor", "missing"),
        "QualifierDescription" = c('most accurate within 10%',
                                   'most data accurate within 25%',
                                   'significant amounts of data may be >25% off',
                                   "missing")),
      by = 'QualifierCode'
    )
  # Odds and ends -------------------------------------------------------------

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
    LatLongDatumSRSName = NULL,
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

  Qualifiers <- hymetDP::create_qualifiers(
    L0_flat = flat,
    QualifierCode = "QualifierCode",
    QualifierDescription = "QualifierDescription"
  )
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
    Qualifiers = Qualifiers,
    SeriesCatalog = SeriesCatalog)

  issues <- hymetDP::validate_data(path = path)

  dataset_annotations <- c()

  # Add contact information for the author of this script and dataset

  additional_contact <- data.frame(
    givenName = 'Kyle',
    surName = 'Zollo-Venecek',
    organizationName = 'Environmental Data Initiative',
    electronicMailAddress = 'zollovenecek@wisc.edu',
    stringsAsFactors = FALSE)

  eml <- hymetDP::create_eml(
    path = path,
    source_id = source_id,
    derived_id = derived_id,
    #is_about = dataset_annotations,
    script = "create_hymetDP.R",
    script_description =
      "A function for converting knb-lter-mcm.9003 to hymetDP",
    contact = additional_contact,
    user_id = 'kzollovenecek',
    user_domain = 'edi',
    basis_of_record = "MachineObservation")
}

