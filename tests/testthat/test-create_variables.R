library(hymetDP)

testthat::test_that("Standard L1 column inputs", {
  for (i in c("df", "tbbl")) {
    if (i == "df") { # test w/data.frame
      flat <- as.data.frame(hymet_L0_flat)
    } else {      # test w/tibble
      flat <- hymet_L0_flat
    }

    crit <- read_criteria()

    res <- hymetDP::create_variables(
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

    # Has expected classes and columns

    if (i == "df") {
      expect_true(all(class(res) %in% "data.frame"))
    } else {
      expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(res)))
    }
    crit_cols <- stats::na.omit(crit$column[crit$table == "Variables"])
    expect_true(all(crit_cols %in% colnames(res)))
    # Is not empty
    expect_true(nrow(res) != 0)
  }
})
