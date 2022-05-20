#' Search published data
#'
#' @param text (character) Text to search for in dataset titles and abstracts. Datasets matching any exact words or phrase will be returned. Can be a regular expression as used by \code{stringr::str_detect()}. Is not case sensitive. Works with \code{boolean}.
#' @param VariableName (character) VariableName values to search on. VariableName values are from ODM Controlled Vocabulary.
#' @param SampleMedium (character) SampleMedium values to search on. SampleMedium values are from ODM Controlled Vocabulary.
#' @param GeneralCategory (character) GeneralCategory values to search on. GeneralCategory values are from ODM Controlled Vocabulary.
#' @param SiteType (character) SiteType values to search on. SiteType values are from ODM Controlled Vocabulary.
#' @param TimeSupport (numeric) Maximum TimeSupport value to search on. TimeSupport is analogous frequency of measurements.
#' @param starts_before (date) Maximum start date to filter on.
#' @param ends_after (date) Minimum end date to filter on.
#' @param num_years (numeric) Minimum and maximum number of years sampled the dataset should contain. Any datasets within this range will be returned.
#' @param area (numeric) Bounding coordinates within which the data should originate. Accepted values are in decimal degrees and in the order: North, East, South, West. Any datasets with overlapping areas or contained points will be returned.
#' @param boolean (character) Boolean operator to use when searching \code{text}, \code{VariableName}, \code{SampleMedium}, \code{GeneralCategory}, and
#'     \code{SiteType}. Supported operators are: "AND", "OR". Default is "AND".
#'
#' @return (tbl_df, tbl, data.frame) Search results with these fields:
#'     \itemize{
#'         \item source - Source from which the dataset originates. Currently supported are "EDI" and "NEON".
#'         \item id - Identifier of the dataset.
#'         \item title - Title of the dataset.
#'         \item abstract - Abstract of dataset.
#'         \item years - Number of years sampled.
#'         \item url - URL to dataset.
#'         \item source_id - Identifier of source L0 dataset.
#'         \item source_id_url - URL to source L0 dataset.
#'     }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Empty search returns all available datasets
#' search_data()
#'
#' # "text" searches titles, descriptions, and abstracts
#' search_data(text = "barometric")
#'
#' # "VariableName" searches VariableName values for a match
#' search_data(VariableName = "Discharge")
#'
#' # "SampleMedium" searches SampleMedium values for a match
#' search_data(SampleMedium = "Water")
#'
#' # "GeneralCategory" searches GeneralCategory values for a match
#' search_data(GeneralCategory = "Hydrology")
#'
#' # "SiteType" searches SiteType values for a match
#' search_data(SiteType = "Stream")
#'
#' # "TimeSupport" searches TimeSupport values for a match
#' search_data(TimeSupport = 30)
#'
#' # "starts_before" and "ends_after" can be used to filter on a time period
#' search_data(starts_before = '2000-01-01', ends_after = '2010-01-01')
#'
#' # "num_years" searches the number of years sampled
#' search_data(num_years = c(10, 20))
#'
#' # Use any combination of search fields to find the data you're looking for
#' search_data(
#'   text = c("stream", "river"),
#'   VariableName = c("Conductivity", "Discharge"),
#'   SampleMedium = "water",
#'   GeneralCategory = "hydrology"
#'   SiteType = "Stream"
#'   TimeSupport = 30,
#'   starts_before = "2010-01-01",
#'   ends_after = "2015-01-01",
#'   num_years = c(10, 100),
#'   area = c(47.1, -86.7, 42.5, -92),
#'   boolean = "OR")
#' }
#'
search_data <- function(text, VariableName, SampleMedium, GeneralCategory,
                        SiteType, TimeSupport, starts_before, ends_after,
                        num_years, area, boolean = "AND") {

  # Validate arguments --------------------------------------------------------

  validate_arguments(
    fun.name = "search_data",
    fun.args = as.list(environment()))

  ping_edi() # Warn if EDI is down

  # Prepare summary data ------------------------------------------------------

  # Download this object once per session and save to tempdir() for future calls
  if ("hymetDP_search_index.rda" %in% dir(tempdir())) {
    load(paste0(tempdir(), "/hymetDP_search_index.rda"))
    d <- hymetDP_search_index
  } else {
    newrev <- suppressMessages(list_data_package_revisions("edi", "864", filter = "newest", env = 'staging'))
    objurls <- suppressMessages(read_data_package(paste0("edi.864.", newrev), env = 'staging'))
    objurls <- stringr::str_subset(objurls, "/data/")
    objids <- stringr::str_extract(objurls, "(?<=/)[:alnum:]+$")
    objnames <- suppressMessages(
      lapply(objids, read_data_entity_name, packageId = paste0("edi.864.", newrev), env = 'staging'))
    objnames <- unlist(objnames)
    isdata <- !stringr::str_detect(objnames, "Function")
    objurls <- objurls[isdata]
    for (objurl in objurls) {
      load(url(objurl))
    }
    hymetDP_search_index <- c(summary_data_edi)
    save(hymetDP_search_index,
         file = paste0(tempdir(), "/hymetDP_search_index.rda"),
         version = 3)
    d <- hymetDP_search_index
  }
  # Initialize an index of available datasets (use_i) for recording successful
  # search hits, and an index of available sites within each dataset (sites_i)
  # corresponding with taxa, num_taxa, and area search arguments.
  # These are used later to collate the search results. use_i is initialized
  # with logical because it will be used to select the matched datasets.
  # sites_i is initialized with NA_character_ because they will be returned
  # to the user in a data frame.

  use_i <- rep(
    list(
      list(
        text = F,
        VariableName = F,
        SampleMedium = F,
        TimeSupport = F,
        GeneralCategory = F,
        SiteType = F,
        starts_before = F,
        ends_after = F,
        num_years = F,
        area = F)),
    length(d))
  names(use_i) <- names(d)

  # Search --------------------------------------------------------------------
  # Apply user specified search criteria to each dataset while recording
  # successful hits.

  for (i in seq_along(d)) {
    arg_i <- rep(F, length(formals()))

    # Search text

    if (!missing(text)) {
      if (boolean == "AND") {
        use_i[[i]]$text <- all(
          stringr::str_detect(
            tolower(
              paste(
                d[[i]]$title,
                d[[i]]$abstract,
                collapse = ", ")),
            tolower(text)))
      } else if (boolean == "OR") {
        use_i[[i]]$text <- stringr::str_detect(
          tolower(
            paste(
              d[[i]]$title,
              d[[i]]$abstract,
              collapse = ", ")),
          tolower(paste(text, collapse = "|")))
      }
    } else {
      use_i[[i]]$text <- NULL
    }

    # Search VariableName
    if (!missing(VariableName)) {
      var_i <- rep(F, length(d[[i]]$VariableName))
      for (k in 1:length(d[[i]]$VariableName)) {
        if (boolean == "AND") {
          var_i[k] <- try(
            all(
              stringr::str_detect(
                tolower(d[[i]]$VariableName[[k]]),
                tolower(VariableName))),
            silent = TRUE)
          if (methods::is(var_i[k], "try-error")) {
            # if (class(var_i[k]) == "try-error") {
            var_i[k] <- FALSE
          }
        } else if (boolean == "OR") {
          var_i[k] <- try(
            stringr::str_detect(
              tolower(d[[i]]$VariableName[[k]]),
              tolower(paste(VariableName, collapse = "|"))),
            silent = TRUE)
          if (methods::is(var_i[k], "try-error")) {
            var_i[k] <- FALSE
          }
        }
      }
      if (any(var_i, na.rm = T)) {
        use_i[[i]]$VariableName <- T
      }
    } else {
      use_i[[i]]$VariableName <- NULL
    }

    # Search VariableName
    if (!missing(SampleMedium)) {
      var_i <- rep(F, length(d[[i]]$SampleMedium))
      for (k in 1:length(d[[i]]$SampleMedium)) {
        if (boolean == "AND") {
          var_i[k] <- try(
            all(
              stringr::str_detect(
                tolower(d[[i]]$SampleMedium[[k]]),
                tolower(SampleMedium))),
            silent = TRUE)
          if (methods::is(var_i[k], "try-error")) {
            # if (class(var_i[k]) == "try-error") {
            var_i[k] <- FALSE
          }
        } else if (boolean == "OR") {
          var_i[k] <- try(
            stringr::str_detect(
              tolower(d[[i]]$SampleMedium[[k]]),
              tolower(paste(SampleMedium, collapse = "|"))),
            silent = TRUE)
          if (methods::is(var_i[k], "try-error")) {
            var_i[k] <- FALSE
          }
        }
      }
      if (any(var_i, na.rm = T)) {
        use_i[[i]]$SampleMedium <- T
      }
    } else {
      use_i[[i]]$SampleMedium <- NULL
    }

    # Search GeneralCategory
    if (!missing(GeneralCategory)) {
      var_i <- rep(F, length(d[[i]]$GeneralCategory))
      for (k in 1:length(d[[i]]$GeneralCategory)) {
        if (boolean == "AND") {
          var_i[k] <- try(
            all(
              stringr::str_detect(
                tolower(d[[i]]$GeneralCategory[[k]]),
                tolower(GeneralCategory))),
            silent = TRUE)
          if (methods::is(var_i[k], "try-error")) {
            # if (class(var_i[k]) == "try-error") {
            var_i[k] <- FALSE
          }
        } else if (boolean == "OR") {
          var_i[k] <- try(
            stringr::str_detect(
              tolower(d[[i]]$GeneralCategory[[k]]),
              tolower(paste(GeneralCategory, collapse = "|"))),
            silent = TRUE)
          if (methods::is(var_i[k], "try-error")) {
            var_i[k] <- FALSE
          }
        }
      }
      if (any(var_i, na.rm = T)) {
        use_i[[i]]$GeneralCategory <- T
      }
    } else {
      use_i[[i]]$GeneralCategory <- NULL
    }

    # Search SiteType
    if (!missing(SiteType)) {
      var_i <- rep(F, length(d[[i]]$SiteType))
      for (k in 1:length(d[[i]]$SiteType)) {
        if (boolean == "AND") {
          var_i[k] <- try(
            all(
              stringr::str_detect(
                tolower(d[[i]]$SiteType[[k]]),
                tolower(SiteType))),
            silent = TRUE)
          if (methods::is(var_i[k], "try-error")) {
            # if (class(var_i[k]) == "try-error") {
            var_i[k] <- FALSE
          }
        } else if (boolean == "OR") {
          var_i[k] <- try(
            stringr::str_detect(
              tolower(d[[i]]$SiteType[[k]]),
              tolower(paste(SiteType, collapse = "|"))),
            silent = TRUE)
          if (methods::is(var_i[k], "try-error")) {
            var_i[k] <- FALSE
          }
        }
      }
      if (any(var_i, na.rm = T)) {
        use_i[[i]]$SiteType <- T
      }
    } else {
      use_i[[i]]$SiteType <- NULL
    }




    # TODO Time Support (should be similar to num_years)
    use_i[[i]]$TimeSupport <- NULL

    # Search temporal coverage

    if (!missing(num_years)) {
      years_i <- (unname(unlist(d[[i]]$number_of_years_sampled)) >= num_years[1]) &
        (unname(unlist(d[[i]]$number_of_years_sampled)) <= num_years[2])
      if (any(years_i, na.rm = T)) {
        use_i[[i]]$num_years <- T
      }
    } else {
      use_i[[i]]$num_years <- NULL
    }


    # TODO starts_before
    use_i[[i]]$starts_before <- NULL
    # TODO ends_after
    use_i[[i]]$ends_after <- NULL

    # Search geographic coverage - Methods support point locations (location
    # falls within the area defined by area) and areas (overlap
    # between location area and the area defined by area).

    if (!missing(area)) {
      geographic_area_i <- rep(F, length(d[[i]]$coordinates))
      for (k in 1:length(d[[i]]$coordinates)) {
        if (length(unique(unlist(d[[i]]$coordinates[[k]]))) == 2) {
          geographic_area_i[k] <-
            (d[[i]]$coordinates[[k]]$N <= area[1]) &
            (d[[i]]$coordinates[[k]]$S >= area[3]) &
            (d[[i]]$coordinates[[k]]$E <= area[2]) &
            (d[[i]]$coordinates[[k]]$W >= area[4])
        } else if (length(unique(unlist(d[[i]]$coordinates[[k]]))) == 4) {
          geographic_area_i[k] <-
            (d[[i]]$coordinates[[k]]$N <= area[1]) & (d[[i]]$coordinates[[k]]$N >= area[3]) |
            (d[[i]]$coordinates[[k]]$S <= area[1]) & (d[[i]]$coordinates[[k]]$S >= area[3]) |
            (d[[i]]$coordinates[[k]]$W >= area[4]) & (d[[i]]$coordinates[[k]]$W <= area[2]) |
            (d[[i]]$coordinates[[k]]$E >= area[4]) & (d[[i]]$coordinates[[k]]$E <= area[2])

        }
      }
      if (any(geographic_area_i, na.rm = T)) {
        use_i[[i]]$area <- T
      }
    } else {
      use_i[[i]]$area <- NULL
    }

   # print(use_i)
    # Indicate whether all search parameters were met
    use_i[i] <- unlist(
      lapply(
        use_i[i],
        function(x) {
          all(unname(unlist(x)))
        }))

  }

  #print(use_i)

  # Return results ------------------------------------------------------------
  d <- d[unname(unlist(use_i))]
  output <- data.table::rbindlist(
    lapply(
      names(d),
      function(x) {
        # # sites - EDI data are inconsistent in the representation of "sites"
        # # (in contrast to NEON) therefore these values are set to NA
        # if (d[[x]]$source == "EDI") {
        #   sites <- NA_character_
        # } else {
        #   sites <- paste(Reduce(intersect, sites_i[[x]]), collapse = ",")
        # }

        # num_years - Report as a single value (EDI) or range (NEON)
        num_years <- unlist(d[[x]]$number_of_years_sampled)

        # source_id
        if (!is.null(d[[x]]$source_id)) {
          source_id <- d[[x]]$source_id
        } else {
          source_id <- NA_character_
        }

        # source_id_url
        if (!is.null(d[[x]]$source_id_url)) {
          source_id_url <- d[[x]]$source_id_url
        } else {
          source_id_url <- NA_character_
        }

        # Return
        list(
          source = d[[x]]$source,
          id = x,
          title = d[[x]]$title,
          abstract = d[[x]]$abstract,
          years = num_years,
          url = d[[x]]$url,
          source_id = source_id,
          source_id_url = source_id_url)
      }))

  if (nrow(output) == 0) {
    return("No results found.")
  } else {
    # output <- dplyr::distinct(format_search_results(output))
    output <- dplyr::distinct(output)
    output <- tidyr::as_tibble(output)
    return(output)
  }

}


run_category_search <- function(category_name, x, i) {

  category <- try(get(category_name), silent = TRUE)


  if (!methods::is(category, "try-error")) {
    var_i <- rep(F, length(d[[i]][[category_name]]))
    for (k in 1:length(d[[i]][[category_name]])) {
      if (boolean == "AND") {
        var_i[k] <- try(
          all(
            stringr::str_detect(
              tolower(d[[i]][[category_name]][[k]]),
              tolower(category))),
          silent = TRUE)
        if (methods::is(var_i[k], "try-error")) {
          # if (class(var_i[k]) == "try-error") {
          var_i[k] <- FALSE
        }
      } else if (boolean == "OR") {
        var_i[k] <- try(
          stringr::str_detect(
            tolower(d[[i]][[category_name]][[k]]),
            tolower(paste(category, collapse = "|"))),
          silent = TRUE)
        if (methods::is(var_i[k], "try-error")) {
          var_i[k] <- FALSE
        }
      }
    }
    if (any(var_i, na.rm = T)) {
      x[[i]][[category_name]] <- T
    }
  } else {
    x[[i]][[category_name]] <- NULL
  }
  x
}
