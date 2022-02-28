test_that("get_eol works", {

  # Create tempfiles

  tmp_newline <- tempfile(fileext = '.txt')
  tmp_carriage <- tempfile(fileext = '.txt')
  tmp_carriage_newline <- tempfile(fileext = '.txt')

  # Put test text into temp files

  writeLines('This is to test that newline characters are read', con = tmp_newline, sep = '\n')
  writeLines('This is to test that carriage returns are read', con = tmp_carriage, sep = '\r')
  writeLines('This is to test that carriage-newline characters are read', con = tmp_carriage_newline, sep = '\r\n')

  expect_equal(
    get_eol(tempdir(), file.name = basename(tmp_newline), detect_os()), "\\n")

  expect_equal(
    get_eol(tempdir(), file.name = basename(tmp_carriage), detect_os()), "\\r")

  expect_equal(
    get_eol(tempdir(), file.name = basename(tmp_carriage_newline), detect_os()), "\\r\\n")

})
