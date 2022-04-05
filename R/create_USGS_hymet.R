create_USGS_hymet <- function(site, param, start, end, write_file = F, path) {

  if (length(param) != length(site) & length(param) != 1) stop("Parameter list must be same length as site list or singular.")
  if (length(start) != length(site) & length(start) != 1) stop("Start date list must be same length as site list or singular.")
  if (length(end) != length(site) & length(end) != 1) stop("End date list must be same length as site list or singular.")

  # TODO params need to be in site params

  usgs_args <- paste0(site, "_", param, "_", start, "_", end)

  flat_list <- lapply(
    seq_along(usgs_args),
    function(x, i) {
      site <- strsplit(x[[i]], "_")[[1]][1]
      param <- strsplit(x[[i]], "_")[[1]][2]
      start <- strsplit(x[[i]], "_")[[1]][3]
      end <- strsplit(x[[i]], "_")[[1]][4]

      flat_part <- create_usgs_flat_fragment(param, site, start, end, i)

    }, x=usgs_args)

  flat <- dplyr::bind_rows(flat_list)

  flat$ValueID <- seq(nrow(flat))

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
    State = "State",
    County = "County",
    Comments = NULL,
    SiteType = "SiteType")

  QualityControlLevels <- hymetDP::create_quality_control(
    L0_flat = flat,
    QualityControlLevelCode = "QualityControlLevelCode",
    Definition = "Definition",
    Explanation = "Explanation"
  )

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
    CensorCode = "CensorCode",
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
}

create_usgs_flat_fragment <- function(param, site, start, end, index) {

  usgs_data <- dataRetrieval::readNWISdata(sites=site, service="iv", parameterCd=param, startDate=start, endDate=end)

  # Takes the output from usgs data and parses it
  # Each run of usgs_hymet should create exactly one variable and one method

  # TODO For multiple sites, need to add "If not exists 00060..."

  var <- get_usgs_variable(param)

  site_info <- dataRetrieval::readNWISsite(site)

  SiteType <- get_usgs_sitetype(site_info$site_tp_cd)

  tzLib <- setNames(c("America/Puerto_Rico",
                      "America/New_York","America/New_York",
                      "America/Chicago","America/Chicago",
                      "America/Denver","America/Denver",
                      "America/Los_Angeles","America/Los_Angeles",
                      "America/Anchorage","America/Anchorage",
                      "America/Honolulu","America/Honolulu","UTC"),
                    c("AST",
                      "EST","EDT",
                      "CST","CDT",
                      "MST","MDT",
                      "PST","PDT",
                      "AKST","AKDT",
                      "HAST","HST","UTC"))

  tz <- tzLib[attributes(usgs_data)$siteInfo$timeZoneAbbreviation]

  LocalDateTime <- lubridate::with_tz(usgs_data$dateTime, tz)

  UTCOffset <- lutz::tz_offset(LocalDateTime, tz)$utc_offset_h

  DataValue <- usgs_data %>% dplyr::select(DataValue = 4)

  QualityControlLevelCode <- usgs_data %>% dplyr::select(QualityControlLevelCode = 5)

  flat <- data.frame(
    VariableCode = var$usgs_code,
    VariableName = var$VariableName,
    VariableUnitsName = var$VariableUnitsName,
    SampleMedium = var$SampleMedium,
    ValueType = var$ValueType,
    IsRegular = var$IsRegular,
    TimeSupport = var$TimeSupport,
    TimeUnitsName = var$TimeUnitsName,
    DataType = var$DataType,
    GeneralCategory = var$GeneralCategory,
    NoDataValue = var$NoDataValue,
    MethodCode = index, # TODO testing only
    MethodDescription = "Data pulled from USGS National Water Infrastructure Service (NWIS) using the dataRetrieval R package",
    MethodLink = paste0("https://waterdata.usgs.gov/monitoring-location/", site, "/#parameterCode=", param),
    SiteCode = site, # Use the code as-is
    SiteName = site_info$station_nm,
    Latitude = site_info$dec_lat_va,
    Longitude = site_info$dec_long_va,
    LatLongDatumSRSName = site_info$coord_datum_cd,
    Elevation_m = NA_character_,
    VerticalDatum = NA_character_,
    LocalX = NA_character_,
    LocalY = NA_character_,
    LocalProjectionSRSName = NA_character_,
    PosAccuracy_m = NA_character_,
    State = dataRetrieval::stateCd$STATE_NAME[dataRetrieval::stateCd$STATE == site_info$state_cd],
    County = dataRetrieval::countyCd$COUNTY_NAME[dataRetrieval::countyCd$COUNTY == site_info$county_cd & dataRetrieval::countyCd$STATE == site_info$state_cd],
    Comments = NA_character_,
    SiteType = SiteType,
    SourceCode = index, # integer code
    Organization = "USGS", # USGS
    SourceDescription = "Data pulled from USGS National Water Infrastructure Service (NWIS) using the dataRetrieval R package",
    SourceLink = paste0("https://waterdata.usgs.gov/monitoring-location/", site, "/#parameterCode=", param),
    ContactName = "Unknown", # Unknown
    Phone = "Unknown",# Unknown
    Email = "Unknown",# Unknown
    Address = "Unknown",# Unknown
    City = "Unknown",# Unknown
    ZipCode = "Unknown",# Unknown
    Citation = paste0("U.S. Geological Survey, 2001, National Water Information System data available on the World Wide Web (Water Data for the Nation), accessed ", Sys.Date(), ", at URL ", paste0("https://waterdata.usgs.gov/monitoring-location/", site, "/#parameterCode=", param), "."),
    DataValue = DataValue,
    LocalDateTime = LocalDateTime,
    UTCOffset = UTCOffset,
    DateTimeUTC = usgs_data$dateTime,
    CensorCode = "unk",
    QualityControlLevelCode = QualityControlLevelCode
  )

  QualityControlLevels <- data.frame(
    QualityControlLevelCode = c("A", "P", "A e"),
    Definition = c("Approved for publication -- Processing and review completed.",
                   "Provisional data subject to revision.",
                   "Estimated, Approved for publication -- Processing and review completed."),
    Explanation = c("Water-Level Approval-Status Codes",
                    "Provisional data may be inaccurate due to instrument malfunctions or physical changes at the measurement site. Subsequent review based on field inspections and measurements may result in significant revisions to the data.",
                    "Water-Level Approval-Status Codes")
  )

  flat <- flat %>% dplyr::left_join(QualityControlLevels, by = "QualityControlLevelCode")

  return(flat)

}

get_usgs_sitetype <- function(s) {
  t <- rvest::read_html('https://help.waterdata.usgs.gov/site_tp_cd') %>%
    rvest::html_element("table") %>%
    rvest::html_table()

  res <- t$`Long name`[t$Name == s]

  return(res)
}


get_usgs_variable <- function(p) {
  res <- data.table::fread(
    system.file('extdata', 'usgs_parameters.txt', package = 'hymetDP'), colClasses = 'character')

  res <- res[res$usgs_code==p]

  return(res)
}
