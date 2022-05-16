library(hymetDP)

skip_on_cran()

site <- c("06879650", "50065500")
param <- c("00060")
start <- c("2021-01-01T12:30:00Z")
end <- c("2021-01-03T12:30:00Z")

usgs_hymet <- hymetDP::create_USGS_hymet(site, param, start, end)

test_that("USGS data is returned as a list", {
  expect_true(methods::is(usgs_hymet, "list"))
})

test_that("List contains all correct elements", {
  expect_equal(length(usgs_hymet), 7)
})

test_that("All list elements are data.frames", {
  expect_true(
    all(unlist(lapply(usgs_hymet, function(x) class(x) == 'data.frame')))
  )
})
