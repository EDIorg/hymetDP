search_data <- function(text, VariableName, SampleMedium, TimeSupport,
                        GeneralCategory, SiteType, starts_before, ends_after,
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

  sites_i <- rep(
    list(
      list(
        text = NA_character_,
        VariableName = NA_character_,
        SampleMedium = NA_character_,
        TimeSupport = NA_character_,
        GeneralCategory = NA_character_,
        SiteType = NA_character_,
        starts_before = NA_character_,
        ends_after = NA_character_,
        num_years = NA_character_,
        area = NA_character_)),
    length(d))
  names(sites_i) <- names(d)

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
      if (isTRUE(use_i[[i]]$text)) {
        sites_i[[i]]$text <- names(d[[i]]$coordinates)
      }
    } else {
      use_i[[i]]$text <- NULL
      sites_i[[i]]$text <- NULL
    }

    # Search VariableName
    # if (!missing(VariableName)) {
    #   var_i <- rep(F, length(d[[i]]$VariableName))
    #   for (k in 1:length(d[[i]]$VariableName)) {
    #     if (boolean == "AND") {
    #       var_i[k] <- try(
    #         all(
    #           stringr::str_detect(
    #             tolower(d[[i]]$VariableName[[k]]),
    #             tolower(VariableName))),
    #         silent = TRUE)
    #       if (methods::is(var_i[k], "try-error")) {
    #         # if (class(var_i[k]) == "try-error") {
    #         var_i[k] <- FALSE
    #       }
    #     } else if (boolean == "OR") {
    #       var_i[k] <- try(
    #         stringr::str_detect(
    #           tolower(d[[i]]$VariableName[[k]]),
    #           tolower(paste(VariableName, collapse = "|"))),
    #         silent = TRUE)
    #       if (methods::is(var_i[k], "try-error")) {
    #         var_i[k] <- FALSE
    #       }
    #     }
    #   }
    #   if (any(var_i, na.rm = T)) {
    #     use_i[[i]]$VariableName <- T
    #     sites_i[[i]]$VariableName <- d[[i]]$VariableName[var_i]
    #   }
    # } else {
    #   use_i[[i]]$VariableName <- NULL
    #   sites_i[[i]]$VariableName <- NULL
    # }

    i_list <- run_category_search('VariableName')

    use_i <- return_use_i(i_list)
    sites_i <- return_sites_i(i_list)


    # Search SampleMedium
    i_list <- run_category_search('SampleMedium')

    use_i <- return_use_i(i_list)
    sites_i <- return_sites_i(i_list)

    # Search GeneralCategory
    i_list <- run_category_search('GeneralCategory')

    use_i <- return_use_i(i_list)
    sites_i <- return_sites_i(i_list)

    # Search SiteType
    i_list <- run_category_search('SiteType')

    use_i <- return_use_i(i_list)
    sites_i <- return_sites_i(i_list)


    # TODO Time Support (should be similar to num_years)

    # Search temporal coverage

    if (!missing(num_years)) {
      years_i <- (unname(unlist(d[[i]]$number_of_years_sampled)) >= num_years[1]) &
        (unname(unlist(d[[i]]$number_of_years_sampled)) <= num_years[2])
      if (any(years_i, na.rm = T)) {
        use_i[[i]]$num_years <- T
        sites_i[[i]]$num_years <- names(d[[i]]$number_of_years_sampled)[years_i]
      }
    } else {
      use_i[[i]]$num_years <- NULL
      sites_i[[i]]$num_years <- NULL
    }


    # TODO starts_before
    # TODO ends_after

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
        sites_i[[i]]$area <- names(d[[i]]$coordinates)[geographic_area_i]
      }
    } else {
      use_i[[i]]$area <- NULL
      sites_i[[i]]$area <- NULL
    }

    # If no arguments are specified, then return the full list of sites for
    # each dataset

    if (missing(text) & missing(taxa) & missing(num_taxa) & missing(num_years) &
        missing(sd_years) & missing(area)) {
      sites_i <- lapply(
        names(d),
        function(x) {
          list(all_sites = names(d[[x]]$coordinates))
        })
      names(sites_i) <- names(d)
    }

    # Indicate whether all search parameters were met
    use_i[i] <- unlist(
      lapply(
        use_i[i],
        function(x) {
          all(unname(unlist(x)))
        }))

  }

  # Return results ------------------------------------------------------------

  d <- d[unname(unlist(use_i))]
  sites_i <- sites_i[unname(unlist(use_i))]
  output <- data.table::rbindlist(
    lapply(
      names(d),
      function(x) {
        # sites - EDI data are inconsistent in the representation of "sites"
        # (in contrast to NEON) therefore these values are set to NA
        if (d[[x]]$source == "EDI") {
          sites <- NA_character_
        } else {
          sites <- paste(Reduce(intersect, sites_i[[x]]), collapse = ",")
        }
        # num_years - Report as a single value (EDI) or range (NEON)
        if (length(d[[x]]$number_of_years_sampled) == 1) {
          num_years <- unlist(d[[x]]$number_of_years_sampled)
        } else {
          num_years <- paste0(
            "min = ", min(unlist(d[[x]]$number_of_years_sampled)), ", ",
            "max = ", max(unlist(d[[x]]$number_of_years_sampled)))
        }
        # sampling_interval - Report as a single value (EDI) or range (NEON)
        if (length(d[[x]]$std_dev_interval_betw_years) == 1) {
          sampling_interval <- round(unlist(d[[x]]$std_dev_interval_betw_years), 2)
        } else {
          sampling_interval <- paste0(
            "min = ", min(unlist(d[[x]]$std_dev_interval_betw_years)), ", ",
            "max = ", max(unlist(d[[x]]$std_dev_interval_betw_years)))
        }
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
          description = d[[x]]$description,
          abstract = d[[x]]$abstract,
          years = num_years,
          sampling_interval = sampling_interval,
          sites = sites,
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


run_category_search <- function(category_name) {

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
      use_i[[i]][[category_name]] <- T
      sites_i[[i]][[category_name]] <- d[[i]][[category_name]][var_i]
    }
  } else {
    use_i[[i]][[category_name]] <- NULL
    sites_i[[i]][[category_name]] <- NULL
  }

  return(list(use_i, sites_i))
}



# Return the use_i list
#
# @param l (list) list object returned from \code{run_category_search()}
#
# @details Simple helper function
#
# @return (list) the updated use_i list
#
return_use_i <- function(l) {
  l[[1]]
}



# Return the sites_i list
#
# @param l (list) list object returned from \code{run_category_search()}
#
# @details Simple helper function
#
# @return (list) the updated sites_i list
#
return_sites_i <- function(l) {
  l[[2]]
}
