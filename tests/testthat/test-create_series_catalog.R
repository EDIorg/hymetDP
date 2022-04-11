testthat::test_that("Standard L1 column inputs", {
  for (i in c("df", "tbbl")) {
    if (i == "df") { # test w/data.frame
      flat <- as.data.frame(hymet_L0_flat)
    } else {      # test w/tibble
      flat <- hymet_L0_flat
    }

    crit <- read_criteria()

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

    Qualifiers <- hymetDP::create_qualifiers(
      L0_flat = flat,
      QualifierCode = "QualifierCode",
      QualifierDescription = "QualifierDescription"
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
      CensorCode = NULL,
      QualifierCode = NULL,
      MethodCode = "MethodCode",
      QualityControlLevelCode = "QualityControlLevelCode",
      SourceCode = "SourceCode",
      NoDataValue = "NoDataValue")

    res <- suppressMessages(hymetDP::create_series_catalog(
      Sources = Sources,
      Methods = Methods,
      Variables = Variables,
      Sites = Sites,
      QualityControlLevels = QualityControlLevels,
      DataValues = DataValues))

    # Has expected columns

    crit_cols <- stats::na.omit(crit$column[crit$table == "SeriesCatalog"])
    expect_true(all(crit_cols %in% colnames(res)))
    # Is not empty
    expect_true(nrow(res) != 0)
  }
})
