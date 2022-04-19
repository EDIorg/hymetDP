#' Read published data
#'
#' @param id (character) Identifier of dataset to read. Identifiers are listed in the "id" column of the \code{search_data()} output. Older versions of datasets can be read, but a warning is issued.
#' @param parse_datetime (logical) Parse datetime values if TRUE, otherwise return as character strings.
#' @param unique_keys (logical) Whether to create globally unique primary keys (and associated foreign keys). Useful in maintaining referential integrity when working with multiple datasets. If TRUE, \code{id} is appended to each table's primary key and associated foreign key. Default is FALSE.
#' @param from (character) Full path of file to be read (if .rds), or path to directory containing saved datasets (if .csv).
#' @param format (character) Format of returned object, which can be: "new" (the new implementation) or "old" (the original implementation; deprecated). In the new format, the top most level of nesting containing the "id" field has been moved to the same level as the "tables", "metadata", and "validation_issues" fields.
#'
#' @return (list) A dataset with the structure:
#' \itemize{
#'   \item id - Dataset identifier
#'   \item metadata - List of info about the dataset. NOTE: This object is underdevelopment and content may change in future releases.
#'   \item tables - List of dataset tables as data.frames.
#'   \item validation_issues - List of validation issues. If the dataset fails any validation checks, then descriptions of each issue are listed here.
#' }
#'
#' @note This function may not work between 01:00 - 03:00 UTC on Wednesdays due to regular maintenance of the EDI Data Repository.
#'
#' @details
#'     Validation checks are applied to each dataset ensuring it complies with the hymetDP model. A warning is issued when any validation checks fail. All datasets are returned, even if they fail validation.
#'
#'     Column classes are coerced to those defined in the hymetDP specification.
#'
#'     Validation happens each time files are read, from source APIs or local environments.
#'
#' @export
#'
#' @examples
#'
read_data <- function(id = NULL, parse_datetime = TRUE,
                      unique_keys = FALSE, from = NULL,
                      format = "new") {

  # Validate input arguments --------------------------------------------------

  validate_arguments(fun.name = "read_data", fun.args = as.list(environment()))

  # Parameterize --------------------------------------------------------------

  # Get hymetDP attributes for validation and coercion

  attr_tbl <- read_criteria()
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]

  # Read ----------------------------------------------------------------------

  if (is.null(from)) { # From API
    if (stringr::str_detect(            # EDI
      id,
      "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")) {
      d <- read_data_edi(id, parse_datetime)
    }
    d <- list(d = d)
    names(d) <- id
  } else {                  # From file
    d <- read_from_files(from)
  }

  # Modify --------------------------------------------------------------------

  # TODO how does this apply to hymetDP?

  # Add missing columns

  for (x in names(d)) {
    for (y in names(d[[x]]$tables)) {
      nms <- attr_tbl$column[attr_tbl$table == y]
      tblnms <- names(d[[x]]$tables[[y]])
      use_i <- setdiff(nms, tblnms)
      if (length(use_i) > 0) {
        d[[x]]$tables[[y]][use_i] <- NA
        # There seems to be an incompatibility in the handling of
        # ..nms between Mac and Windows Os
        msg <- try(
          d[[x]]$tables[[y]] <- d[[x]]$tables[[y]][ , ..nms],
          silent = TRUE)
        if (attr(msg, "class") == "try-error") {
          d[[x]]$tables[[y]] <- d[[x]]$tables[[y]][ , nms]
        }
      }
    }
  }

  # Coerce column classes to ecocomDP specifications. NOTE: This same
  # process is applied to read_from_files(). Any update here should be
  # duplicated there. A function is not used in order to minimize data object
  # copies.

  for (x in names(d)) {
    for (y in names(d[[x]]$tables)) {
      for (z in names(d[[x]]$tables[[y]])) {
        detected <- class(d[[x]]$tables[[y]][[z]])
        expected <- attr_tbl$class[(attr_tbl$table == y) & (attr_tbl$column == z)]
        if (any(detected %in% c("POSIXct", "POSIXt", "Date", "IDate"))) {

          detected <- "Date" # so downstream logic doesn't throw length() > 1 warnings
        }
        if (isTRUE(parse_datetime) & (expected == "Date") & (detected == "character" | detected == "logical")) { # NAs should be datetime for consistency
          d[[x]]$tables[[y]][[z]] <- lubridate::as_date(d[[x]]$tables[[y]][[z]])
        }
        if (detected != expected) {
          if (expected == 'character'){
            d[[x]]$tables[[y]][[z]] <- as.character(d[[x]]$tables[[y]][[z]])
          } else if (expected == 'numeric'){
            d[[x]]$tables[[y]][[z]] <- as.numeric(d[[x]]$tables[[y]][[z]])
          }
        }
      }
    }
  }

  # Append package_id to primary keys to ensure referential integrity (except
  # package_id, appending package_id to package_id changes the field definition
  # and shouldn't be necessary as the package_id is very unlikely to be
  # duplicated).

  # TODO come back to this one ... need to determine if dataset summary is a necessary table

  if (isTRUE(unique_keys)) {
    for (x in names(d)) {
      for (y in names(d[[x]]$tables)) {
        for (z in names(d[[x]]$tables[[y]])) {
          if (stringr::str_detect(z, "Code")) {
            if (!(z %in% c("package_id", "original_package_id",
                           "mapped_id", "authority_taxon_id",
                           "parent_location_id"))) {
              d[[x]]$tables[[y]][[z]] <- paste0(
                d[[x]]$tables[[y]][[z]], "_", x)
            } else if (z == "parent_location_id") {
              use_i <- is.na(d[[x]]$tables[[y]][[z]])
              d[[x]]$tables[[y]][[z]] <- paste0(
                d[[x]]$tables[[y]][[z]], "_", x)
              d[[x]]$tables[[y]][[z]][use_i] <- NA_character_
            }
          }
        }
      }
    }
  }

  # Return datetimes as character
  if (!isTRUE(parse_datetime)) {
    for (id in names(d)) {
      for (tbl in names(d[[id]]$tables)) {
        dtcols <- stringr::str_detect(colnames(d[[id]]$tables[[tbl]]), "datetime")
        if (any(dtcols)) {
          colname <- colnames(d[[id]]$tables[[tbl]])[dtcols]
          vals <- as.character(d[[id]]$tables[[tbl]][[colname]])
          d[[id]]$tables[[tbl]][[colname]] <- vals
        }
      }
    }
  }

  # Control returned structure ------------------------------------------------

  if (format == "new") {
    if (suppressWarnings(detect_data_type(d)) == "dataset_old")  {
      d <- c(id = names(d), d[[1]])
    } else if (detect_data_type(d) == "list_of_datasets") {
      for (i in 1:length(d)) {
        d[[i]] <- c(id = names(d[i]), d[[i]])
      }
      d <- unname(d)
    }
  } else if (format == "old") {
    warning('The old format is deprecated. Please use the new format instead.',
            call. = FALSE)
  }

  # Validate ------------------------------------------------------------------

  callstack <- as.character(sys.calls())

  #TODO remove these comments
  #TODO remove these comments
  #TODO remove these comments
  #TODO remove these comments
  #TODO remove these comments
#
#   if (!any(stringr::str_detect(callstack, "validate_data\\("))) { # don't validate if read_data() is called from validate()
#     if (format == "new") {
#       if (detect_data_type(d) == "dataset")  {
#         d$validation_issues <- validate_data(dataset = d)
#       } else if (detect_data_type(d) == "list_of_datasets") {
#         for (i in 1:length(d)) {
#           d[[i]]$validation_issues <- validate_data(dataset = d[[i]])
#         }
#       }
#     } else if (format == "old") {
#       # Validation only runs on the new format, so fake it and assign issues to the return object
#       # TODO: Remove this 2022-10-18
#       if (detect_data_type(d) == "dataset_old")  {
#         mock_new <- d
#         mock_new[[1]]$id <- names(d)
#         mock_new <- mock_new[[1]]
#         d[[1]]$validation_issues <- validate_data(dataset = mock_new)
#       } else if (detect_data_type(d) == "list_of_datasets_old") {
#         for (i in 1:length(d)) {
#           d[[i]]$validation_issues <- validate_data(dataset = d[[i]])
#         }
#       }
#     }
#   }

  # Return --------------------------------------------------------------------

  return(d)

}






read_data_edi <- function(id, parse_datetime = TRUE) {

  message("Reading ", id)

  # Parameterize
  if (exists("config.environment", envir = .GlobalEnv)) {
    config.environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    config.environment <- "production"
  }

  # Get ecocomDP attributes for validation and coercion

  attr_tbl <- read_criteria()

  # Get table metadata for reading

  tbl_attrs <- lapply(
    seq_along(vector('list', length(unique(attr_tbl$table)))),
    function(x){
      list(name = NULL, url = NULL, delimiter = NULL, nrecord = NULL)
    })
  names(tbl_attrs) <- unique(attr_tbl$table)

  xpath <- c(
    name = './/physical/objectName',
    url = './/physical/distribution/online/url',
    delimiter = './/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter',
    nrecord = './/numberOfRecords',
    formatString = './/dateTime/formatString')

  eml <- suppressMessages(
    api_read_metadata(id, environment = config.environment))

  for (x in names(tbl_attrs)) {
    tblnames <- xml2::xml_text(xml2::xml_find_all(eml, './/dataset/dataTable/physical/objectName'))
    use_i <- stringr::str_detect(tblnames, paste0(x, '\\.[:alnum:]*$'))
    if (any(use_i)) {
      for (k in names(xpath)) {
        nodeset <- xml2::xml_find_all(eml, ".//dataset/dataTable")[use_i]
        val <- xml2::xml_text(xml2::xml_find_all(nodeset, xpath[[k]]))
        if (length(val) == 0) {
          val <- NA
        }
        tbl_attrs[[x]][[k]] <- val
      }
    } else {
      tbl_attrs[[x]] <- NULL
    }
  }

  # Read tables
  output <- lapply(
    names(tbl_attrs),
    function(x) {
      res <- data.table::fread(
        tbl_attrs[[x]]$url)
      res <- as.data.frame(res)
      return(res)
    })
  names(output) <- names(tbl_attrs)

  # Parse datetime
  for (tbl in names(output)) {
    frmtstr <- tbl_attrs[[tbl]]$formatString
    if (!is.na(frmtstr)) {
      dtcol <- stringr::str_subset(colnames(output[[tbl]]), "datetime")
      if (isTRUE(parse_datetime)) {
        parsed <- parse_datetime_from_frmt(tbl = tbl, vals = output[[tbl]][[dtcol]], frmt = frmtstr)
        output[[tbl]][[dtcol]] <- parsed
      }
    }
  }

  # Parse metadata
  meta = list(url = paste0("https://portal.edirepository.org/nis/mapbrowse?packageid=", id))

  # Return
  res <- list(metadata = meta, tables = output)
  return(res)
}







read_from_files <- function(data.path) {
  attr_tbl <- read_criteria()
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]
  fileext <- tools::file_ext(data.path)
  if (fileext == "rds") {        # rds
    d <- readRDS(data.path)
  } else if (fileext != "rds") { # dir ... note NEON ids cause file_ext() to produce misleading results
    dirs <- list.dirs(data.path)
    parent_dir_has_tables <- any( # does the parent dir have any L1 tables?
      unlist(
        lapply(
          unique(attr_tbl$table),
          function(x) {
            hymetDP_table <- stringr::str_detect(
              tools::file_path_sans_ext(list.files(dirs[1])),
              paste0("^", x, "$"))
            return(any(hymetDP_table))
          })))
    if (parent_dir_has_tables) {                              # Don't look in subdirs if parent has tables
      dirs <- dirs[1]
      d <- read_dir(dirs)
    } else if (!parent_dir_has_tables & (length(dirs) > 1)) { # Identify subdirs with tables
      dirs <- dirs[-1]
      i <- unlist(
        lapply(
          dirs,
          function(dirc) {
            r <- any(
              unlist(
                lapply(
                  unique(attr_tbl$table),
                  function(x) {
                    hymetDP_table <- stringr::str_detect(
                      tools::file_path_sans_ext(list.files(dirc)),
                      paste0("^", x, "$"))
                    return(any(hymetDP_table))
                  })))
            return(r)
          }))
      dirs_w_tables <- dirs[i]
      d <- read_dir(dirs_w_tables)
      return(d)
    } else {
      stop("No identifiable L1 tables at ", data.path, call. = FALSE)
      d <- NULL
    }
  }

  # Downstream code of read_data() use the "old" format, so need to convert
  # back to it here
  # TODO: Remove this block of code on 2022-10-18
  if (suppressWarnings(detect_data_type(d)) == "dataset") {
    id <- d$id
    d$id <- NULL
    d <- list(d)
    names(d) <- id
  } else if (suppressWarnings(detect_data_type(d)) == "list_of_datasets") {
    ids <- c()
    for (i in 1:length(d)) {
      ids <- c(ids, d[[i]]$id)
      d[[i]]$id <- NULL
    }
    names(d) <- ids
  }

  return(d)
}





read_dir <- function(paths) {
  attr_tbl <- read_criteria()
  d <- lapply(
    paths,
    function(path) {
      res <- lapply(
        unique(attr_tbl$table),
        function(x) {
          hymetDP_table <- stringr::str_detect(
            list.files(path),
            paste0("(?<=.{0,10000})", x, "(?=\\.[:alnum:]*$)"))
          if (any(hymetDP_table)) {

              res <- data.table::fread(
                paste0(path, "/", list.files(path)[hymetDP_table]))
            # parse datetime
            if ("datetime" %in% colnames(res)) {
              frmt <- parse_datetime_frmt_from_vals(res$datetime)
              if (!is.null(frmt)) {
                res$datetime <- parse_datetime_from_frmt(tbl = x,
                                                         vals = res$datetime,
                                                         frmt = frmt)
              }
            }
            res <- as.data.frame(res)
            return(res)
          }
        })
      names(res) <- unique(attr_tbl$table)
      res[sapply(res, is.null)] <- NULL
      res <- list(
        list(metadata = NULL, tables = res))
      return(res)
    })
  d <- unlist(d, recursive = FALSE)
  if (length(paths) != 0){ # Get id from dir if nested (use case of reading from save_data(..., type = .csv))
    names(d) <- basename(paths)
  } else {
    package_id <- d[[1]]$tables$dataset_summary$package_id
    if (is.na(package_id) | (package_id == "")) {
      names(d) <- "unknown"
    } else {
      names(d) <- package_id
    }
  }
  return(d)
}
