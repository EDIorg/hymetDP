#' Define a hymetDP method
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset,
#'   in "flat" format (see details).
#' @param local_variable_column (character) Column in \code{L0_flat} table
#'   containing the L0 variable name.
#' @param local_variable (character) Reference to a value (or values) in the
#'   \code{local_variable_column} from the \code{L0_flat} table to which the new
#'   hymetDP method refers.
#' @param VariableCode (character) The auto-generated primary key for a variable
#'   (from column \code{VariableCode}). Another way to link a method to a value
#'   (or values). Takes priority over \code{local_variable}. Provide one or
#'   multiple codes.
#' @param MethodDescription (character) Text description of the method.
#' @param MethodLink (character) Optional. Link to additional reference material
#'   on the method. Should be a single valid URL.
#'
#' @details This function appends columns to the \code{L0_flat} table and
#'   returns the augmented table.
#'
#'   "flat" format refers to the fully joined source L0 dataset in "wide" form
#'   with the exception of the core observation variables, which are in "long"
#'   form (i.e. using the variable_name, value, unit columns of the observation
#'   table). This "flat" format is the "widest" an L1 hymetDP dataset can be
#'   consistently spread due to the frequent occurrence of L0 source datasets
#'   with > 1 core observation variable.
#'
#'
#' @return (tbl_df, tbl, data.frame) An augmented version of the original flat
#'   table, with all of the original columns plus one additional column for the
#'   method description (or two additional columns if a method link is defined).
#'   Column name includes the auto-generated MethodCode (i.e.
#'   MethodDescription_1), which will become the the primary key in the Methods
#'   table.
#'
#' @examples
#'
#' flat <- hymet_L0_flat[1:19]
#'
#' flat <- hymetDP::define_method(
#'   L0_flat = flat,
#'   local_variable_column = "variable_name",
#'   local_variable = "DSCHRGE_RATE",
#'   VariableCode = c(1,2,3),
#'   MethodDescription = paste(
#'     "Campbell CR10 dataloggers were used to record stream stage, water",
#'     "temperature, and conductivity in a network of stream gages.",
#'     "Stage is monitored with pressure transducers; PSS-1 and PS-2 models",
#'     "form Paroscientific Corporation, and Accubars from Sutron",
#'     "Corporation. The pressure transducers measure the backpressure in",
#'     "orifice lines set into or above controls in the stream channel. In",
#'     "addition, some of the sites monitor water temperature and",
#'     "conductivity with either USGS minimonitor probes, or Campbell",
#'     "temperature/conductivity probes. Ratings are developed for the",
#'     "stage/discharge relationship at each site by measuring streamflow",
#'     "with current meters or portable flumes, according to standard USGS",
#'     "methods. Datum corrections to the stage are determined by",
#'     "periodically surveying the elevation of the orifice line to the",
#'     "control and nearby reference marks. Calibrations for the",
#'     "temperature and conductivity are assessed by measuring these",
#'     "parameters with portable field meters while simultaneously noting",
#'     "the readings from the gage probes. Data is downloaded into Campbell",
#'     "storage modules, and retrieved into pcs. From there, the data is",
#'     "sent to a USGS computer, where time discrepancies are resolved, and",
#'     "the data is loaded into ADAPS, a database system developed in the",
#'     "USGS for maintaining and processing water data. A determination for",
#'     "each site as to when the stream was flowing and when it was not is",
#'     "made. For water temperature and conductivity, bad data is deleted.",
#'     "Variable shifts are determined based on field calibration",
#'     "measurements, and other indicators. The shifts are applied to the",
#'     "remaining good data inside of ADAPS. The data is pulled out of",
#'     "ADAPS, and reformatted for input into ORACLE. Cases of water",
#'     "temperature below reasonable values are set to lower limits. A",
#'     "quality code is assigned to every value. The resulting data is",
#'     "uploaded into the ORACLE and the McMurdo database. Before 2011, For",
#'     "stage/discharge, bad data is deleted. Survey data is reviewed to",
#'     "compute weir elevations an datum corrections. A rating curve is",
#'     "developed graphically, based on available data, and entered into",
#'     "ADAPS. All applicable shifts and datum corrections are entered into",
#'     "ADAPS. All corrections and ratings are run against the good stage",
#'     "data to compute the discharge at each recording interval. The data",
#'     "is pulled out of ADAPS, and reformatted for input into ORACLE. A",
#'     "quality code is assigned to every value. The resulting data is",
#'     "uploaded into ORACLE and the McMurdo database. ADAPS deprecated in",
#'     "favor of Aquarius software in 2012. Similar procedure is used in",
#'     "Aquarius to convert and curate the data. Metadata was enhanced in",
#'     "2013 and 2014 by Inigo San Gil. In March 2021, data from the",
#'     "2016-17 field season was replaced to correct a previously published",
#'     "error, in which discharge was reported in cubicFeetPerSecond (CFS)",
#'     "instead of litersPerSecond (l/s)."
#'     ),
#'   MethodLink = NULL)
#'
#'
#' @export
#'
define_method <- function(
  L0_flat,
  local_variable_column = "variable_name",
  local_variable = NULL,
  VariableCode = NULL,
  MethodDescription = NULL,
  MethodLink = NULL) {

  validate_arguments(fun.name = "define_method",
                     fun.args = as.list(environment()))

  # Assign the argument for "table" to a variable

  flat_input <- L0_flat


  # TODO what to do if local_variable and variable_code specified?
  # Should only variable code be supported?
  # Currently, both supported. If both are provided, defaults to code over name.

  # This function will add a single column "MethodDescription_<MethodCode>"
  # This is to prevent the (possibly already huge) flat table from
  # doubling or tripling in size with the addition of multiple methods to a
  # single/every variable
  # TODO Is this the best way to do this? or stick to a truly flat table with possibly many rows?
  # TODO I think I've found the problem with this. I want a MethodCode attached to the flat table. This doesnt do that.
  # TODO the problem I was trying to avoid (duplicating observataions to allow for multiple methods assigned to one)
  # TODO might be moot because you can really only have one method per observation.

  # TODO what I want to do is follow a more similar build DF and join to flat as I do in define variable


  # Determine existing methods
#
#   existing_methods <- names(flat_input) %>%
#     stringr::str_detect('MethodDescription_') %>%
#     sum()
#
#     # Define new column names
#
#     method_sym <- rlang::sym(paste0("MethodDescription_", existing_methods + 1))

  if (is.null(VariableCode) & is.null(local_variable)) {

    # Add method to every observation -----------------------------------------
    # Since you can only have one method per variable, if you are adding to the whole table,
    # MethodCode will always be 1

    flat_output <- flat_input %>%
      dplyr::mutate(
        MethodCode = 1,
        MethodDescription = MethodDescription,
        MethodLink = MethodLink
      )

    # TODO below is the old way of handling methods (MethodDescription_1 etc)
    # # Add the method description and code to every observation
    #
    # flat_output <- flat_input %>%
    #   dplyr::mutate(!!method_sym := method_description)
    #
    # # Handle method links
    #
    # if (!is.null(method_link)) {
    #   link_sym <- rlang::sym(paste0("MethodLink_", existing_methods + 1))
    #
    #   flat_output <- flat_output %>%
    #     dplyr::mutate(!!link_sym := method_link)
    # }

  }

  if ("MethodCode" %in% names(flat_input)) {

    MethodCode = max(flat_input$MethodCode, na.rm = TRUE) + 1
  } else {

    MethodCode <- 1
  }

  if (is.null(VariableCode) & !is.null(local_variable)) {

    # Join by variable name ---------------------------------------------------

    # Create method table and join by local_variable

    method_table <- dplyr::tibble(
      local_variable_name = local_variable,
      MethodCode = MethodCode,
      MethodDescription = MethodDescription,
      MethodLink = MethodLink)

    # This is necessary to use setNames() in the by parameter of the join

    lvn = "local_variable_name"

    flat_output <- flat_input %>%
      dplyr::left_join(method_table,
                       by = stats::setNames(lvn, local_variable_column))

  } else if (!is.null(VariableCode)) {

    # Join by variable code ---------------------------------------------------

    # Create a method table and join by VariableCode

    method_table <- dplyr::tibble(
      VariableCode = VariableCode,
      MethodCode = MethodCode,
      MethodDescription = MethodDescription,
      MethodLink = MethodLink)

    flat_output <- flat_input %>%
      dplyr::left_join(method_table, by = "VariableCode")
  }



# Coalesce like columns ---------------------------------------------------


  if (any(stringr::str_detect(names(flat_output),"MethodCode.x"))) {
    flat_output <- flat_output %>%
      dplyr::mutate(
        MethodCode = dplyr::coalesce(MethodCode.x, MethodCode.y)) %>%
      dplyr::select(-MethodCode.x,
                    -MethodCode.y)
  }

  if (any(stringr::str_detect(names(flat_output),"MethodDescription.x"))) {
    flat_output <- flat_output %>%
      dplyr::mutate(
        MethodDescription = dplyr::coalesce(
          MethodDescription.x,MethodDescription.y)) %>%
      dplyr::select(-MethodDescription.x,
                    -MethodDescription.y)
  }

  if (any(stringr::str_detect(names(flat_output), "MethodLink.x"))) {
    flat_output <- flat_output %>%
      dplyr::mutate(
        MethodLink = dplyr::coalesce(MethodLink.x, MethodLink.y)) %>%
      dplyr::select(-MethodLink.x,
                    -MethodLink.y)
  }

  return(flat_output)
}
