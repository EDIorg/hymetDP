library(hymetDP)

path = tempdir()



test_that("non-CV term gets caught", {

  test_tables <- hymet_L1$tables

  test_tables$Variables$VariableName[1] <- "Flow"

  hymetDP::write_tables(
    path = path,
    DataValues = test_tables$DataValues,
    Variables = test_tables$Variables,
    Methods = test_tables$Methods,
    Sources = test_tables$Sources,
    Sites = test_tables$Sites,
    QualityControlLevels = test_tables$QualityControlLevels,
    Qualifiers = test_tables$Qualifiers,
    SeriesCatalog = test_tables$SeriesCatalog)

  expect_warning(issues <- hymetDP::validate_data(path = path))

  expect_equal(unlist(issues), "Controlled Vocabulary terms. The column VariableName in the table Variables contains the terms 'Flow' which are not terms in the VariableNameCV ODM Controlled Vocabulary. Choose terms from the VariableNameCV ODM Controlled Vocabulary.")
})

test_that("multiple non-CV terms get caught", {

  test_tables <- hymet_L1$tables

  test_tables$Variables$VariableName[1] <- "Flow"
  test_tables$Variables$VariableName[2] <- "temp"

  hymetDP::write_tables(
    path = path,
    DataValues = test_tables$DataValues,
    Variables = test_tables$Variables,
    Methods = test_tables$Methods,
    Sources = test_tables$Sources,
    Sites = test_tables$Sites,
    QualityControlLevels = test_tables$QualityControlLevels,
    Qualifiers = test_tables$Qualifiers,
    SeriesCatalog = test_tables$SeriesCatalog)

  expect_warning(issues <- hymetDP::validate_data(path = path))

  expect_equal(unlist(issues), "Controlled Vocabulary terms. The column VariableName in the table Variables contains the terms 'Flow' and 'temp' which are not terms in the VariableNameCV ODM Controlled Vocabulary. Choose terms from the VariableNameCV ODM Controlled Vocabulary.")
})
