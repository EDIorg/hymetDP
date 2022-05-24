library(hymetDP)

skip_on_cran()

eml <- read_metadata('edi.1028.1')

tables <- read_tables(
  eml = eml,
  strip.white = TRUE,
  na.strings = "",
  convert.missing.value = TRUE,
  add.units = TRUE,
  table.names = NULL)

test_that("read_tables downloads a list of tables", {
  expect_true(methods::is(tables, 'list'))
})

test_that("read_tables downloads all tables", {
  expect_equal(length(tables), 3)
})

test_that("all list elements are tables", {
  expect_true(all(unlist(lapply(tables,
                                function(x) methods::is(x, 'data.frame')))))
})

test_that("units are correctly added", {

  units <- stringr::str_detect(names(tables$WildflowerCES_FrenchBroadRiverBasin_site_info.csv), "unit_")

  expect_equal(sum(units), 12)
})

test_that("only selected table is downloaded", {

  tables <- read_tables(
    eml = eml,
    strip.white = TRUE,
    na.strings = "",
    convert.missing.value = TRUE,
    add.units = TRUE,
    table.names = "WildflowerCES_FrenchBroadRiverBasin_site_info.csv")

  expect_equal(length(tables), 1)
})

test_that("only filenames with extensions are accepted", {

  expect_error(
    read_tables(
      eml = eml,
      strip.white = TRUE,
      na.strings = "",
      convert.missing.value = TRUE,
      add.units = TRUE,
      table.names = "WildflowerCES_FrenchBroadRiverBasin_site_info"))
})

test_that("all table.names must be in package", {

  expect_error(
    read_tables(
      eml = eml,
      strip.white = TRUE,
      na.strings = "",
      convert.missing.value = TRUE,
      add.units = TRUE,
      table.names = c("WildflowerCES_FrenchBroadRiverBasin_site_info.csv", "WildflowerNumber2")))
})

