validate_odm_terms <- function(fun.name, fun.args) {


  # define_variable() -------------------------------------------------------

  if (fun.name == 'define_variable') {

    # TODO this should maybe be put in a loop, but its difficult to automate the
    ## translation of object names to text (i.e. getting "VariableNameCV" from the object VariableNameCV)

    # Check variable_name
    if (check_odm_cv(fun.args$variable_name, VariableNameCV$Term)) {
      stop(paste0("The VariableName term \"", fun.args$variable_name, "\" was not found in the VariableNameCV.\n"), call. = FALSE)}

    # Check variable_units
    if (check_odm_cv(fun.args$variable_units, UnitsCV$UnitsName)) {
      stop(paste0("The VariableUnits term \"", fun.args$variable_units, "\" was not found in the UnitsCV.\n"), call. = FALSE)}

    # Check sample_medium
    if (check_odm_cv(fun.args$sample_medium, SampleMediumCV$Term)) {
      stop(paste0("The SampleMedium term \"", fun.args$sample_medium, "\" was not found in the SampleMediumCV.\n"), call. = FALSE)}

    # Check value_type
    if (check_odm_cv(fun.args$value_type, ValueTypeCV$Term)) {
      stop(paste0("The ValueType term \"", fun.args$value_type, "\" was not found in the ValueTypeCV.\n"), call. = FALSE)}

    # Check time_units
    if (check_odm_cv(fun.args$time_units, UnitsCV$UnitsName)) {
      stop(paste0("The TimeUnits term \"", fun.args$time_units, "\" was not found in the UnitsCV.\n"), call. = FALSE)}

    # Check #general_category
    if (check_odm_cv(fun.args$general_category, GeneralCategoryCV$Term)) {
      stop(paste0("The GeneralCategory term \"", fun.args$general_category, "\" was not found in the GeneralCategoryCV.\n"), call. = FALSE)}

  }

}

check_odm_cv <- function(term, cv) !term %in% cv
