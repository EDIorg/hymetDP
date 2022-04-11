# This script creates the example dataset "hymet_L1"

#library(hymetDP)

# Read from EDI data repository
flat <- hymet_L0_flat

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

hymet_L1 <- list(
  id = NULL,
  metadata = NULL,
  tables = list(
    Sources = Sources,
    Methods = Methods,
    Variables = Variables,
    Sites = Sites,
    QualityControlLevels = QualityControlLevels,
    Qualifiers = Qualifiers,
    DataValues = DataValues,
    SeriesCatalog = SeriesCatalog)
)

# Save to /data
usethis::use_data(hymet_L1, overwrite = TRUE)




