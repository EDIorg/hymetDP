create_eml <- function(path,
                       source_id = NULL,
                       derived_id,
                       script,
                       script_description,
                       is_about = NULL,
                       contact,
                       user_id,
                       user_domain,
                       basis_of_record = NULL, # This is a DwC thing... maybe remove
                       url = NULL) {

  message("Creating EML for derived data package (" , derived_id, ")")

  # Load Global Environment config --------------------------------------------

  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }

  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }

  # Validate inputs -----------------------------------------------------------

  validate_arguments(fun.name = "create_eml", fun.args = as.list(environment()))

  # Parameterize --------------------------------------------------------------

  # Read attributes of hymetDP tables for reference

  attr_tbl <- read_criteria()
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]

  # Get table names for this L1 dataset for use in EAL_make_eml()

  data.table <- unlist(
    lapply(
      unique(attr_tbl$table),
      function(x) {
        hymetDP_table <- stringr::str_detect(
          list.files(path),
          paste0("(?<=.{0,10000})", x, "(?=\\.[:alnum:]*$)"))
        if (any(hymetDP_table)) {
          list.files(path)[hymetDP_table]
        }
      }))

  # Arrange tables in the preferred order to be listed in the EML

  fext <- unique(tools::file_ext(data.table))
  preferred_order <- c("Variables", "Methods", "Sites", "Sources",
                       "QualityControlLevels", "DataValues", "SeriesCatalog")

  use_i <- preferred_order %in% tools::file_path_sans_ext(data.table)
  data.table <- paste0(preferred_order[use_i], ".", fext)

  # Match table names of this L1 to their boiler plate descriptions for use in
  # EAL_make_eml()

  descriptions <- data.table::fread(
    system.file("extdata", "table_descriptions.txt", package = "hymetDP"))

  data.table.description <- descriptions$description[
    match(
      stringr::str_remove_all(
        data.table,
        "(\\.[:alnum:]*$)"),
      descriptions$table_name)]

  # Map scripts and their descriptions to their EAL_make_eml()
  # equivalents

  if (!is.null(script)) {
    other.entity <- script
    other.entity.description <- script_description
  } else {
    other.entity <- NULL
    other.entity.description <- NULL
  }

  # Expand url for each data object of this L1 for use in
  # EAL_make_eml()

  if (!is.null(url)) {
    if (!is.null(data.table)) {
      data.table.url <- paste0(url, "/", data.table)
    }
    if (!is.null(other.entity)) {
      other.entity.url <- paste0(url, "/", other.entity)
    }
  } else {
    data.table.url <- NULL
    other.entity.url <- NULL
  }

  # Read L0 EML ---------------------------------------------------------------

  message("Reading EML of L0 data package ", source_id)

  # Create two objects of the same metadata, eml_L0 (emld list object) for
  # editing, and xml_L0 (xml_document) for easy parsing

  if (environment == "production") {
    url_parent <- paste0(
      "https://pasta.lternet.edu/package/metadata/eml/",
      stringr::str_replace_all(source_id, "\\.", "/"))
  } else if (environment == "staging") {
    url_parent <- paste0(
      "https://pasta-s.lternet.edu/package/metadata/eml/",
      stringr::str_replace_all(source_id, "\\.", "/"))
  }

  # TODO accesses EDI
  eml_L0 <- tryCatch({EML::read_eml(url_parent)},
                     error = function(e) {
                       stop("The EDI Repository is down for regular maintenance (Wednesday 01:00",
                            " - 03:00 UTC). If you have reached this message outside maintenance",
                            " hours, then there is an unexpected issue that will be resolved ",
                            "shortly. Our apologies for the inconvenience. Please try again ",
                            "later.", call. = FALSE)
                     })
  xml_L0 <- suppressMessages(read_eml(source_id))

  # Remove L0 elements that should not be inherited by the L1

  eml_L0$dataset$dataTable <- NULL
  eml_L0$dataset$spatialRaster <- NULL
  eml_L0$dataset$spatialVector <- NULL
  eml_L0$dataset$storedProcedure <- NULL
  eml_L0$dataset$view <- NULL
  eml_L0$dataset$otherEntity <- NULL

  eml_L0$additionalMetadata <- NULL

  # Create L1 EML -------------------------------------------------------------

  message("Creating EML of L1 data package ", derived_id)

  # This will not be a full EML record, it will only contain sections of the L1
  # EML to combined with the L0 EML.

  # Create list of inputs to EAL_make_eml()

  eal_inputs <- EAL_template_arguments(
    path = system.file("extdata", "/hymetDP", package = "hymetDP"),
    data.path = path,
    data.table = data.table,
    other.entity = script)

  eal_inputs$path <- system.file("extdata", "/hymetDP", package = "hymetDP")
  eal_inputs$data.path <- path
  eal_inputs$eml.path <- path
  eal_inputs$dataset.title <- "placeholder"
  eal_inputs$data.table <- data.table
  eal_inputs$data.table.name <- tools::file_path_sans_ext(data.table)
  eal_inputs$data.table.description <- data.table.description
  eal_inputs$data.table.url <- data.table.url
  eal_inputs$data.table.quote.character <- rep('"', length(data.table))
  eal_inputs$other.entity <- other.entity
  eal_inputs$other.entity.name <- tools::file_path_sans_ext(other.entity)
  eal_inputs$other.entity.description <- other.entity.description
  eal_inputs$other.entity.url <- other.entity.url
  eal_inputs$package.id <- derived_id
  eal_inputs$user.id <- user_id
  eal_inputs$user.domain <- user_domain
  eal_inputs$return.obj <- TRUE


  # Remove unused data table attributes templates. All boiler plate attributes*
  # files are read in with EAL_template_arguments() above, but
  # only the ones being used should be kept and used in
  # EAL_make_eml().

  all_attribute_templates <- names(eal_inputs$x$template)[
    stringr::str_detect(
      names(eal_inputs$x$template),
      "attributes_")]

  expected_attribute_templates <- paste0(
    "attributes_",
    stringr::str_remove(data.table, "\\.[:alnum:]*$"),
    ".txt")

  unused_attribute_templates <- all_attribute_templates[
    !(all_attribute_templates %in% expected_attribute_templates)]

  eal_inputs$x$template[
    names(eal_inputs$x$template) %in% unused_attribute_templates] <- NULL



# Update attributes_* templates ----------------------------------------------
  # Change unit of TimeSupport in Variables attributes

  # TODO how to handle multiple units? Impossible?

  eal_inputs$x$template$attributes_Variables.txt$content$unit[
    eal_inputs$x$template$attributes_Variables.txt$content$attributeName == "TimeSupport"] <- unique(eal_inputs$x$data.table$Variables.csv$content$TimeUnitsName)

  # Change <NoDataValue> in DataValues Missing value attributes template with
  # the value specified in Variable table column NoDataValue

  # TODO how to handle multiple missing values

  eal_inputs$x$template$attributes_DataValues.txt$content$missingValueCode[
    eal_inputs$x$template$attributes_DataValues.txt$content$missingValueCode == "<NoDataValue>"] <- unique(eal_inputs$x$data.table$Variables.csv$content$NoDataValue)


  # Detect date and time format string directly from each table and add to the
  # corresponding data table attributes template as required by
  # EAL_make_eml().

  for (i in expected_attribute_templates) {

    date_column <- eal_inputs$x$template[[i]]$content$attributeName[
      eal_inputs$x$template[[i]]$content$class == "Date"]

    if (length(date_column) != 0) {

      data_table <- which(
        stringr::str_detect(
          names(eal_inputs$x$data.table),
          paste0(
            "^",
            stringr::str_extract(i, "(?<=attributes_).*(?=\\.txt)"),
            "\\.[:alnum:]*$")))

      for  (j in seq(length(date_column))) {

        datetime <- eal_inputs$x$data.table[[data_table]]$content[[date_column[[j]]]]

        datetime_format <- parse_datetime_frmt_from_vals(datetime)

        eal_inputs$x$template[[i]]$content$dateTimeFormatString[
          eal_inputs$x$template[[i]]$content$attributeName == date_column[[j]]] <-
          datetime_format
      }
    }
  }

  # Get table attributes and definitions from EML then create catvars templates for each data table of this dataset

# Add ODM CV terms as cat vars --------------------------------------------

  cv_tbls <- subset(attr_tbl, !is.na(attr_tbl$cv))

  r <- lapply(
    # for each data table
    unique(cv_tbls$table),
    function(tbl) {

      cont <- lapply(
        cv_tbls$column[cv_tbls$table == tbl],
        function(col) {
          univars <- unique(eal_inputs$x$data.table[[paste0(tbl, '.csv')]]$content[[col]])
          unidefs <- lapply(
            univars,
            function(var) {
              cv <- cv_tbls$cv[cv_tbls$table == tbl & cv_tbls$column == col]

              if (cv == "UnitsCV") {
                t <- get(cv)
                paste0("Unit: ",
                    t$UnitsName[t$UnitsName == var],
                    " (", t$UnitsAbbreviation[t$UnitsName == var], "); UnitType: ",
                    t$UnitsType[t$UnitsName == var]
                    )
              } else if (cv == "SpatialReferencesCV") {
                paste0("Spatial Reference System: ", var)
              } else {
                t <- get(cv)
                t$Definition[t$Term == var]
              }
            })

          catvars_template <- data.frame(
            attributeName = col,
            code = univars,
            definition = unlist(unidefs),
            stringsAsFactors = FALSE)
          return(list(content = catvars_template))
        })

      # Manually add in the isRegular code and definition to catvars_Variables
      if (tbl == 'Variables') {
        cont[[8]] <-  list(content = data.frame(
          attributeName = "IsRegular",
          code = c("TRUE", "FALSE"),
          definition = c("Data values are from a regularly sampled time series", "Data values are not from a regularly sampled time series"),
          stringsAsFactors = FALSE))
      }

      return(dplyr::bind_rows(cont))
    })
  names(r) <- paste0("catvars_", unique(cv_tbls$table), ".txt")
  r <- Filter(Negate(is.null), r)

  eal_inputs$x$template <- c(eal_inputs$x$template, r)

  # The annotations template read in with EAL_template_arguments()
  # serves as a map from tables of this L1 to the boilerplate annotations
  # which are compiled here.

  # TODO everything below involves annotations/variable mapping and should be added back!

  annotations_map <- eal_inputs$x$template$annotations.txt$content
  annotations <- annotations_map[0, ]

  annotations <- rbind(
    annotations,
    annotations_map[annotations_map$context %in% "eml", ])
  if (!is.null(is_about)) {
    additional_dataset_annotations <- data.frame(
      id = "/dataset",
      element = "/dataset",
      context = "eml",
      subject = "dataset",
      predicate_label = "is about",
      predicate_uri = "http://purl.obolibrary.org/obo/IAO_0000136",
      object_label = names(is_about),
      object_uri = unname(is_about),
      stringsAsFactors = FALSE)
    annotations <- rbind(annotations, additional_dataset_annotations)
  }

  # TODO i wonder if there is a better object than analysis code. Processing code?
  other_entity_annotations <- data.frame(
    id = paste0("/", script),
    element = "/otherEntity",
    context = "dataset",
    subject = script,
    predicate_label = "is about",
    predicate_uri = "http://purl.obolibrary.org/obo/IAO_0000136",
    object_label = "analysis code",
    object_uri = "http://purl.dataone.org/odo/ECSO_00002489",
    stringsAsFactors = FALSE)
  annotations <- rbind(annotations, other_entity_annotations)

  for (i in data.table) {
    table <- stringr::str_remove(i, "\\.[:alpha:]*$")
    annotations_subset <- dplyr::filter(
      annotations_map,
      subject %in% table | context %in% table)
    table_annotations <- annotations_subset[
      annotations_subset$subject %in%
        c(colnames(eal_inputs$x$data.table[[i]]$content), table), ]
    table_annotations$id <- stringr::str_replace(
      table_annotations$id,
      paste0("(?<=/)", table, "(?=$|/)"),
      i)
    table_annotations$context <- stringr::str_replace(
      table_annotations$context, table, i)
    table_annotations$subject <- stringr::str_replace(
      table_annotations$subject, paste0("^", table, "$"), i)
    annotations <- rbind(annotations, table_annotations)
  }

  # variable_mapping <- stringr::str_subset(
  #   names(eal_inputs$x$data.table),
  #   "variable_mapping")
  # if (length(variable_mapping) != 0) {
  #   tblnms_varmap <- eal_inputs$x$data.table[[variable_mapping]]$content$table_name # remove missing tables from variable_mapping
  #   tblnms_input <- tools::file_path_sans_ext(data.table)
  #   tbls2keep <- tblnms_varmap %in% tblnms_input
  #   eal_inputs$x$data.table[[variable_mapping]]$content <- eal_inputs$x$data.table[[variable_mapping]]$content[tbls2keep, ]
  #   variable_mappings_annotations <- lapply(
  #     unique(eal_inputs$x$data.table[[variable_mapping]]$content$table_name),
  #     function(table) {
  #       variable_mapping_subset <- dplyr::filter(
  #         eal_inputs$x$data.table[[variable_mapping]]$content,
  #         table_name == table)
  #       file_name <- stringr::str_subset(
  #         names(eal_inputs$x$data.table),
  #         paste0(table, "\\.[:alpha:]*$"))
  #
  #       if (!is.null(variable_mapping_subset$mapped_label)) { # Handle missing columns
  #         objlbl <- variable_mapping_subset$mapped_label
  #       } else {
  #         objlbl <- ""
  #       }
  #       if (!is.null(variable_mapping_subset$mapped_id)) {
  #         objuri <- variable_mapping_subset$mapped_id
  #       } else {
  #         objuri <- ""
  #       }
  #
  #       annotation <- data.frame(
  #         id = paste0("/", file_name, "/variable_name"),
  #         element = "/dataTable/attribute",
  #         context = file_name,
  #         subject = "variable_name",
  #         predicate_label = "is about",
  #         predicate_uri = "http://purl.obolibrary.org/obo/IAO_0000136",
  #         object_label = objlbl,
  #         object_uri = objuri,
  #         stringsAsFactors = FALSE)
  #       # Remove duplicate annotations or the variable_name attribute (a column
  #       # containing multiple variables as apart of a "long" table) will have
  #       # more than one of the same annotation
  #       annotation <- dplyr::distinct(
  #         annotation,
  #         object_label,
  #         object_uri,
  #         .keep_all = TRUE)
  #       return(annotation)
  #     })
  #   annotations <- rbind(
  #     annotations,
  #     data.table::rbindlist(variable_mappings_annotations))
  # }

  annotations[annotations == ""] <- NA_character_
  annotations <- annotations[stats::complete.cases(annotations), ]

  eal_inputs$x$template$annotations.txt$content <- annotations

  # Only include metadata for existing columns (attributes)
  for (i in data.table) {
    table <- stringr::str_remove(i, "\\.[:alpha:]*$")
    tmplt <- paste0("attributes_", table, ".txt")
    attrnms <- eal_inputs$x$template[[tmplt]]$content$attributeName
    colnms <- colnames(eal_inputs$x$data.table[[i]]$content)
    attrs_to_keep <- attrnms %in% colnms
    eal_inputs$x$template[[tmplt]]$content <- eal_inputs$x$template[[tmplt]]$content[attrs_to_keep, ]
  }

  # Call EAL_make_eml()
  eml_L1 <- suppressWarnings(
    suppressMessages(
      do.call(
        EAL_make_eml,
        eal_inputs[
          names(eal_inputs) %in% names(formals(EAL_make_eml))])))

  # Update <eml> --------------------------------------------------------------

  message("Updating:")
  message("<eml>")
  eml_L0$schemaLocation <- paste0(
    "https://eml.ecoinformatics.org/eml-2.2.0  ",
    "https://nis.lternet.edu/schemas/EML/eml-2.2.0/xsd/eml.xsd")
  eml_L0$packageId <- derived_id
  eml_L0$system <- "edi"

  # Update <access> -----------------------------------------------------------

  # Access control rules are used by some repositories to manage
  # editing, viewing, downloading permissions. Adding the user_id and
  # user_domain here expands editing permission to the creator of the DwC-A
  # data package this EML will be apart of.

  eml_L0$access$allow <- unique(
    c(eml_L0$access$allow,
      eml_L1$access$allow))

  # Update <dataset> ----------------------------------------------------------

  # For purposes of annotation references, the <dataset> attribute (which may
  # have been set by the L0 creator) needs to be set to "dataset", which is
  # expected by the L1 dataset annotation.

  eml_L0$dataset$id <- "dataset"

  # Remove <alternateIdentifier> ----------------------------------------------

  # Some repositories assign a DOI to this element. Not removing it here
  # an error when uploading to the repository.

  message("  <dataset>")
  message("    <alternateIdentifier>")
  eml_L0$dataset$alternateIdentifier <- NULL

  # Update <title> ------------------------------------------------------------

  # Add notification the user that this is an hymetDP data package

  message("    <title>")
  eml_L0$dataset$title <- paste(
    eml_L0$dataset$title, "(Reformatted to the hymetDP Design Pattern)")

  # Update <pubDate> ----------------------------------------------------------

  message("    <pubDate>")
  eml_L0$dataset$pubDate <- format(Sys.time(), "%Y-%m-%d")

  # Updating <abstract> -------------------------------------------------------

  # Add link to L0 data packages and combine L0 and L1 abstracts

  eml_L1$dataset$abstract$para[[1]] <- stringr::str_replace(
    eml_L1$dataset$abstract$para[[1]],
    "L0_PACKAGE_URL",
    url_parent)

  L1_para <- eml_L1$dataset$abstract$para[[1]]
  L0_para <- xml2::xml_text(
    xml2::xml_find_all(xml_L0, ".//abstract//para"))
  eml_L0$dataset$abstract <- NULL

  eml_L0$dataset$abstract$para <- c(
    list(L1_para),
    list(L0_para))

  # Update <keywordSet> -------------------------------------------------------

  # Add hymetDP specific keywords to the L0 keywords

  message("    <keywordSet>")
  # Two options for combining keyword sets, because of variation in the return
  # from EML::read_eml() (i.e. lists nodes when length > 1, and unlists when
  # length = 1).
  if (!is.null(names(eml_L0$dataset$keywordSet))) {
    eml_L0$dataset$keywordSet <- c(list(eml_L0$dataset$keywordSet),
                                   eml_L1$dataset$keywordSet)
  } else {
    eml_L0$dataset$keywordSet <- c(eml_L0$dataset$keywordSet,
                                   eml_L1$dataset$keywordSet)
  }

  # Update <intellectualRights> -----------------------------------------------

  # Use parent intellectual rights or CC0 if none exists

  if (is.null(eml_L0$dataset$intellectualRights)) {
    message("    <intellectualRights>")
    eml_L0$dataset$intellectualRights <- eml_L2$dataset$intellectualRights
  }

  # Update <contact> ----------------------------------------------------------

  # Add hymetDP creator to list of contacts

  message("    <contact>")

  eml_L0$dataset$contact <- c(
    list(
      list(
        individualName = list(
          givenName = contact$givenName,
          surName = contact$surName),
        organizationName = contact$organizationName,
        electronicMailAddress = contact$electronicMailAddress)),
    list(eml_L0$dataset$contact))

  # Update <methods> ----------------------------------------------------------

  # Update parent methods with hymetDP creation process and provenance
  # metadata to provide the user with a full understanding of how these data
  # were created

  message("    <methods>")

  # Parse components to be reordered and combined for the L1
  methods_L1 <- eml_L1$dataset$methods$methodStep
  # Get provenance metadata
  r <- suppressMessages(
    api_get_provenance_metadata(
      package.id = source_id,
      environment = environment))
  xml2::xml_set_attrs(xml2::xml_find_all(r, ".//*[@id]"), c(id = NULL)) # Remove attributes to prevent id clashing and schema invalidation
  r <- EML::read_eml(r)
  provenance_L1 <- list(
    dataSource = r$dataSource,
    description = r$description)
  # Remove any provenance nodes from the L0 metadata, otherwise they will be
  # transferred to the L1 metadata, which would be an inaccurate representation
  # of the provenance chain.
  method_steps <- xml2::xml_find_all(xml_L0, "./dataset/methods/methodStep")
  prov <- unlist(lapply(method_steps, is_prov))
  eml_L0$dataset$methods$methodStep <- eml_L0$dataset$methods$methodStep[!prov]
  # Combine L1 methods, L0 methods, and L0 provenance
  eml_L0$dataset$methods$methodStep <- c(
    list(methods_L1),
    list(eml_L0$dataset$methods$methodStep),
    list(provenance_L1))

  # Update <dataTable> --------------------------------------------------------

  message("    <dataTable>")
  eml_L0$dataset$dataTable <- eml_L1$dataset$dataTable

  # Update <otherEntity> ------------------------------------------------------

  message("    <otherEntity>")
  eml_L0$dataset$otherEntity <- eml_L1$dataset$otherEntity

  # Update <annotations> ------------------------------------------------------

  message("    <annotations>")
  eml_L0$annotations <- eml_L1$annotations

  # Write EML -----------------------------------------------------------------

  message("</eml>")
  message("Writing EML")

  emld::eml_version("eml-2.2.0")
  EML::write_eml(
    eml_L0,
    paste0(path, "/", derived_id, ".xml"))

  # Validate EML --------------------------------------------------------------

  message("Validating EML")

  r <- EML::eml_validate(eml_L0)
  if (isTRUE(r)) {
    message("  Validation passed :)")
  } else {
    message("  Validation failed :(")
  }
  message("Done.")

  # Return --------------------------------------------------------------------

  return(eml_L0)

}
