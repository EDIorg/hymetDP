define_method <- function(
  table = "flat", # the name of the flat table as a character
  local_variable_column = "variable_name",
  local_variable = NULL, # Can be a single variable or multiple
  variable_code = NULL, # Can be a single code or multiple. If null, method applies to every observation
  method_description = NULL,
  method_link = NULL) {

  validate_arguments(fun.name = "define_method", fun.args = as.list(environment()))

  # Assign the argument for "table" to a variable

  flat_input <- get(table)

  # TODO what to do if local_variable and variable_code specified? Should only variable code be supported?

  # TODO this function will add a single column "MethodDescription_<MethodCode>"
  ## This is to prevent the (possibly already huge) flat table from
  ## doubling or tripling in size with the addition of multiple methods to a single/every variable

  # Determine existing methods

  existing_methods <- names(flat_input) %>%
    stringr::str_detect('MethodDescription_') %>%
    sum()

  # TODO handle when there is methodLink

  if (is.null(variable_code) & is.null(local_variable)) {

    # Define new column names

    method_sym <- rlang::sym(paste0("MethodDescription_", existing_methods + 1))

    # Add the method description and code to every observation

    flat_output <- flat_input %>%
      dplyr::mutate(!!method_sym := method_description)

  } else if (is.null(variable_code) & !is.null(local_variable)) {

    # TODO Create a single column table and join by local_variable

  } else {

    # TODO Create a single column table and join by variable_code

  }
}
