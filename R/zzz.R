.onLoad <- function(libname = find.package("hymetDP"), pkgname = "hymetDP") {

  #' @importFrom magrittr "%>%"

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      c(".")
    )


  invisible(NULL)
}
