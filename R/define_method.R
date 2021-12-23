define_method <- function(
  table = "flat", # the name of the flat table as a character
  local_variable_column = "variable_name",
  local_variable = NULL, # Can be a single variable or multiple
  variable_code = NULL, # Can be a single code or multiple. If null, method applies to every observation
  method_description = NULL,
  method_link = NULL) {

  validate_arguments(fun.name = "define_method", fun.args = as.list(environment()))

  # TODO what to do if local_variable and variable_code specified? Should only variable code be supported?

  # TODO this function will add a single column "MethodDescription_<MethodCode>"
  ## This is to prevent the (possibly already huge) flat table from
  ## doubling or tripling in size with the addition of multiple methods to a single/every variable

  if (is.null(variable_code)) {

    # Add the method description and code to every observation


  }
}
