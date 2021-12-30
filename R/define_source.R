define_source <- function(
  eml = eml,
  organization = NULL,
  source_description = NULL,
  source_link = NULL,
  ContactName = NULL,
  Phone = NULL,
  Email = NULL,
  Address = NULL,
  City = NULL,
  State = NULL,
  ZipCode = NULL,
  Citation = NULL) {

  validate_arguments(fun.name = "define_source", fun.args = as.list(environment()))

  # TODO try and auto-populate all mandatory fields from EML

  # TODO any fields that can not be populated from EML, assign "Unknown"

  # TODO explicit arguments take preccedence
}
