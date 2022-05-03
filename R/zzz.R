.onLoad <- function(libname = find.package("hymetDP"), pkgname = "hymetDP") {

  #' @importFrom magrittr "%>%"

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      c(".",
        "..nms",
        "DataType.x",
        "DataType.y",
        "DataTypeCV",
        "EAL_read_eml",
        "GeneralCategory.x",
        "GeneralCategory.y",
        "GeneralCategoryCV",
        "IsRegular.x",
        "IsRegular.y",
        "MethodCode",
        "MethodCode.x",
        "MethodCode.y",
        "MethodDescription.x",
        "MethodDescription.y",
        "MethodLink.x",
        "MethodLink.y",
        "NoDataValue.x",
        "NoDataValue.y",
        "QualityControlLevelCode",
        "SampleMedium.x",
        "SampleMedium.y",
        "SampleMediumCV",
        "SiteCode",
        "SourceCode",
        "SpatialReferencesCV",
        "TimeSupport.x",
        "TimeSupport.y",
        "TimeUnitsName.x",
        "TimeUnitsName.y",
        "UnitsCV",
        "ValueType.x",
        "ValueType.y",
        "ValueTypeCV",
        "VariableCode",
        "VariableCode.x",
        "VariableCode.y",
        "VariableName.x",
        "VariableName.y",
        "VariableNameCV",
        "VariableUnitsName.x",
        "VariableUnitsName.y",
        "anno_ls",
        "argument_issues",
        "bad_term",
        "catalog",
        "column",
        "context",
        "eml_L2",
        "parse_datetime_from_frmt",
        "subject",
        "template_issues",
        "txcl_make_taxonomicCoverage",
        "vocab_lter_term",
        "<<-")
    )


  invisible(NULL)
}
