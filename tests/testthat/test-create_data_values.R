library(hymetDP)

testthat::test_that("Standard L1 column inputs", {
  for (i in c("df", "tbbl")) {
    if (i == "df") { # test w/data.frame
      flat <- as.data.frame(hymet_L0_flat)
    } else {      # test w/tibble
      flat <- hymet_L0_flat
    }

    crit <- read_criteria()

    res <- hymetDP::create_data_values(
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

    # Has expected classes and columns

    if (i == "df") {
      expect_true(all(class(res) %in% "data.frame"))
    } else {
      expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(res)))
    }
    crit_cols <- stats::na.omit(crit$column[crit$table == "DataValues"])
    expect_true(all(crit_cols %in% colnames(res)))
    # Is not empty
    expect_true(nrow(res) != 0)
  }
})
