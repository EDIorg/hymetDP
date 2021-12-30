#' Define a hymetDP method
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param local_variable_column (character) Column in \code{L0_flat} table containing the L0 variable name.
#' @param local_variable (character) Reference to a value (or values) in the \code{local_variable_column} from the \code{L0_flat} table to which the new hymetDP method refers.
#' @param variable_code (character) The auto-generated primary key for a variable (from column \code{VariableCode}). Another way to link a method to a value (or values). Takes priority over \code{local_variable}.
#' @param method_description (character) Text description of the method.
#' @param method_link (character) Optional. Link to additional reference material on the method. Should be a single valid URL.
#'
#' @details This function appends columns to the \code{L0_flat} table and returns the augmented table.
#'
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 hymetDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#'
#'
#'
#' @return (tbl_df, tbl, data.frame) An augmented version of the original flat table, with all of the original columns plus one additional column for the method description (or two additional columns if a method link is defined). Column name includes the auto-generated MethodCode (i.e. MethodDescription_1), which will become the the primary key in the Methods table.
#'
#' @examples
#'
#' flat <- <insert_test_flat_table_here>
#'
#' flat <- define_variable(
#'   L0_flat = flat,
#'   local_variable_column = "variable_name",
#'   local_variable = NULL,
#'   variable_code = NULL,
#'   method_description = NULL,
#'   method_link = NULL)
#'
#'
#' @export
#'
define_method <- function(
  L0_flat = flat,
  local_variable_column = "variable_name",
  local_variable = NULL,
  variable_code = NULL,
  method_description = NULL,
  method_link = NULL) {

  validate_arguments(fun.name = "define_method", fun.args = as.list(environment()))

  # Assign the argument for "table" to a variable

  flat_input <- L0_flat

  # TODO what to do if local_variable and variable_code specified? Should only variable code be supported?
  ## Currently, both supported. If both are provided, defaults to code over name.

  # This function will add a single column "MethodDescription_<MethodCode>"
  # This is to prevent the (possibly already huge) flat table from
  # doubling or tripling in size with the addition of multiple methods to a single/every variable
  # TODO Is this the best way to do this? or stick to a truly flat table with possibly many rows?

  # Determine existing methods

  existing_methods <- names(flat_input) %>%
    stringr::str_detect('MethodDescription_') %>%
    sum()

    # Define new column names

    method_sym <- rlang::sym(paste0("MethodDescription_", existing_methods + 1))

  # TODO handle when there is methodLink

  if (is.null(variable_code) & is.null(local_variable)) {

    # Add method to every observation -----------------------------------------

    # Add the method description and code to every observation

    flat_output <- flat_input %>%
      dplyr::mutate(!!method_sym := method_description)

    # Handle method links

    if (!is.null(method_link)) {
      link_sym <- rlang::sym(paste0("MethodLink_", existing_methods + 1))

      flat_output <- flat_output %>%
        dplyr::mutate(!!link_sym := method_link)
    }

  } else if (is.null(variable_code) & !is.null(local_variable)) {

    # Join by variable name ---------------------------------------------------

    # Create method table and join by local_variable

    method_table <- dplyr::tibble(
      local_variable_name = local_variable,
      !!method_sym := method_description)

    # Handle method links

    if (!is.null(method_link)) {
      link_sym <- rlang::sym(paste0("MethodLink_", existing_methods + 1))

      method_table <- method_table %>%
        dplyr::mutate(!!link_sym := method_link)
    }

    # This is necessary to use setNames() in the by parameter of the join

    lvn = "local_variable_name"

    flat_output <- flat_input %>%
      dplyr::left_join(method_table, by = setNames(lvn, local_variable_column))

  } else {

    # Join by variable code ---------------------------------------------------

    # Create a method table and join by variable_code

    method_table <- dplyr::tibble(
      VariableCode = variable_code,
      !!method_sym := method_description)

    # Handle method links

    if (!is.null(method_link)) {
      link_sym <- rlang::sym(paste0("MethodLink_", existing_methods + 1))

      method_table <- method_table %>%
        dplyr::mutate(!!link_sym := method_link)
    }

    flat_output <- flat_input %>%
      dplyr::left_join(method_table, by = "VariableCode")
  }

  return(flat_output)
}
