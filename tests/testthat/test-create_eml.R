library(hymetDP)

# create_eml() ----------------------------------------------------------------

testthat::test_that("Creates valid EML", {
  testthat::skip_on_cran()

  # Create directory with ecocomDP tables for create_eml()
  mypath <- paste0(tempdir(), "/data")
  dir.create(mypath)
  inpts <- c(hymet_L1$tables, path = mypath)
  do.call(write_tables, inpts)
  file.copy(system.file("extdata", "create_hymetDP.R", package = "hymetDP"), mypath)

  # Add self as contact information incase questions arise
  additional_contact <- data.frame(
    givenName = 'Kyle',
    surName = 'Zollo-Venecek',
    organizationName = 'Environmental Data Initiative',
    electronicMailAddress = 'hymetdp@gmail.com',
    stringsAsFactors = FALSE)

  # Create EML
  eml <- create_eml(
    path = mypath,
    source_id = "knb-lter-mcm.9003.11",
    derived_id = "edi.10101.1",
    is_about = NULL,
    script = "create_hymetDP.R",
    script_description = "A function for converting knb-lter-mcm.9003 to hymetDP",
    contact = additional_contact,
    user_id = 'kzollovenecek',
    user_domain = 'EDI',
    basis_of_record = "MachineObservation")

  # Test
  expect_true(EML::eml_validate(eml))

  # Clean up
  unlink(mypath, recursive = TRUE)
})
