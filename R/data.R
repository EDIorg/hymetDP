#' Joined and flat version of EDI data package knb-lter-mcm.9003.11
#'
#' A fully joined and flat version of EDI data package knb-lter-mcm.9003.11 (Seasonal high-frequency measurements of discharge, water temperature, and specific conductivity from Andersen Creek at H1, McMurdo Dry Valleys, Antarctica (1993-2020, ongoing)) with all relevant hymetDP L1 identifiers and content added. Use this dataset as an input to the \code{L0_flat} argument of the "create" functions.
#'
#' @format A data frame with 30,000 rows and 45 variables:
#' \describe{
#'   \item{LocalDateTime}{Local datetime}
#'   \item{UTCOffset}{Local timezone offset from UTC}
#'   \item{DateTimeUTC}{Datetime in UTC}
#'   \item{variable_name}{Variable name from the L0 dataset}
#'   \item{DataValue}{Numeric value of the observation}
#'   \item{Qualifier}{Code of a qualifier (flag)}
#'   \item{unit}{Unit name from the L0 dataset}
#'   \item{ValueID}{The data value ID}
#'   \item{VariableCode}{The variable ID}
#'   \item{VariableName}{The ODM CV variable name}
#'   \item{VariableUnitsName}{The ODM CV unit name for the variable}
#'   \item{SampleMedium}{The ODM CV sample medium name}
#'   \item{ValueType}{The ODM CV value type}
#'   \item{IsRegular}{Whether the values are from a regularly sampled dataset}
#'   \item{TimeSupport}{Temporal footprint of samples}
#'   \item{TimeUnitsName}{The ODM CV unit name for time support}
#'   \item{DataType}{The ODM CV data type}
#'   \item{GeneralCategory}{The ODM CV general category}
#'   \item{NoDataValue}{The numeric value used to encode not available data}
#'   \item{MethodCode}{The method ID}
#'   \item{MethodDescription}{The description of the method}
#'   \item{SiteCode}{The site ID}
#'   \item{SiteName}{The name of the site}
#'   \item{Latitude}{Latitude of site}
#'   \item{Longitude}{Longitude of site}
#'   \item{Elevation_m}{Elevation of site in meters}
#'   \item{SiteType}{The ODM CV site type}
#'   \item{Organization}{Name of organization that collected the data}
#'   \item{ContactName}{Name of contact person for the data source}
#'   \item{Email}{Email of contact}
#'   \item{SourceCode}{The source ID}
#'   \item{SourceDescription}{The description of the source data}
#'   \item{SourceLink}{URL of source data}
#'   \item{Citation}{Citation for the source data}
#'   \item{Phone}{Contact phone}
#'   \item{Address}{Contact physical address}
#'   \item{City}{Contact city}
#'   \item{State}{Contact state if in US, otherwise full country name}
#'   \item{ZipCode}{Contact postal code}
#'   \item{QualityControlLevelCode}{The quality control ID}
#'   \item{Definition}{Definition of the quality control level}
#'   \item{Explanation}{Explanation of the quality control level}
#'   \item{QualifierCode}{The qualifier ID}
#'   \item{QualifierDescription}{Description of the qualifier}
#' }
#' @source \url{https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-mcm&identifier=9003&revision=11}
"hymet_L0_flat"






#' The hymetDP (L1) version of EDI data package knb-lter-mcm.9003.11
#'
#' The the hymetDP (L1) formatted version of EDI data package knb-lter-mcm.9003.11 (Seasonal high-frequency measurements of discharge, water temperature, and specific conductivity from Andersen Creek at H1, McMurdo Dry Valleys, Antarctica (1993-2020, ongoing)) produced from the table \code{hymet_L0_flat}. Use this dataset as an input to data "use" functions.
#'
#' @format A list of:
#' \describe{
#'   \item{id}{The dataset identifier}
#'   \item{metadata}{See source url for metadata}
#'   \item{tables}{A list of data frames, each an ecocomDP table}
#' }
#' @source \code{hymet_L0_flat}
"hymet_L1"


#' Allowed Censor Codes
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Censor Codes
#'
#' @format A data frame with 6 rows and 2 variables:
#' \describe{
#'   \item{Term}{Controlled Vocabulary term}
#'   \item{Definition}{Definition of Controlled Vocabulary term}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=CensorCodeCV}
"CensorCodeCV"



#' Allowed Data Types
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Data Types
#'
#' @format A data frame with 16 rows and 2 variables:
#' \describe{
#'   \item{Term}{Controlled Vocabulary term}
#'   \item{Definition}{Definition of Controlled Vocabulary term}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=DataTypeCV}
"DataTypeCV"





#' Allowed General Categories
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for General Category
#'
#' @format A data frame with 10 rows and 2 variables:
#' \describe{
#'   \item{Term}{Controlled Vocabulary term}
#'   \item{Definition}{Definition of Controlled Vocabulary term}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=GeneralCategoryCV}
"GeneralCategoryCV"




#' Allowed Sample Media
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Sample Medium
#'
#' @format A data frame with 24 rows and 2 variables:
#' \describe{
#'   \item{Term}{Controlled Vocabulary term}
#'   \item{Definition}{Definition of Controlled Vocabulary term}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=SampleMediumCV}
"SampleMediumCV"


#' Allowed Site Types
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Site Types
#'
#' @format A data frame with 62 rows and 2 variables:
#' \describe{
#'   \item{Term}{Controlled Vocabulary term}
#'   \item{Definition}{Definition of Controlled Vocabulary term}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=SiteTypeCV}
"SiteTypeCV"


#' Allowed Spatial References
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Spatial References
#'
#' @format A data frame with 343 rows and 5 variables:
#' \describe{
#'   \item{SpatialReferenceID}{Integer identifier}
#'   \item{SRSID}{Spatial Reference System identifier}
#'   \item{SRSName}{Spatial Reference System name}
#'   \item{IsGeographic}{Boolean}
#'   \item{Notes}{Spatial Reference System notes}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=SpatialReferences}
"SpatialReferencesCV"



#' Allowed Units
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Units
#'
#' @format A data frame with 394 rows and 4 variables:
#' \describe{
#'   \item{UnitsID}{Unique unit identifier}
#'   \item{UnitsName}{Name of the unit}
#'   \item{UnitsType}{Type of unit}
#'   \item{UnitsAbbreviation}{Standardized abbreviation or symbol of the unit}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=Units}
"UnitsCV"



#' Allowed Value Types
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Value Type
#'
#' @format A data frame with 7 rows and 2 variables:
#' \describe{
#'   \item{Term}{Controlled Vocabulary term}
#'   \item{Definition}{Definition of Controlled Vocabulary term}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=ValueTypeCV}
"ValueTypeCV"




#' Allowed Variable Names
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Variable Name
#'
#' @format A data frame with 925 rows and 2 variables:
#' \describe{
#'   \item{Term}{Controlled Vocabulary term}
#'   \item{Definition}{Definition of Controlled Vocabulary term}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=VariableNameCV}
"VariableNameCV"


#' Allowed Vertical Datum
#'
#' A data frame representation of the CUAHSI ODM 1.1 Controlled Vocabulary for Vertical Datum
#'
#' @format A data frame with 5 rows and 2 variables:
#' \describe{
#'   \item{Term}{Controlled Vocabulary term}
#'   \item{Definition}{Definition of Controlled Vocabulary term}
#' }
#' @source \url{http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=VerticalDatumCV}
"VerticalDatumCV"


