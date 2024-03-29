% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_tables.R
\name{read_tables}
\alias{read_tables}
\title{Read data tables of a data package}
\usage{
read_tables(
  eml,
  strip.white = FALSE,
  na.strings = NULL,
  convert.missing.value = NULL,
  add.units = FALSE,
  table.names = NULL
)
}
\arguments{
\item{eml}{(xml_document, xml_node) EML metadata returned from
\code{read_eml()}.}

\item{strip.white}{(logical) Strips leading and trailing whitespaces of
unquoted fields. Default if FALSE.}

\item{na.strings}{(character) Strings to be interpreted as NA. Setting
\code{na.strings = ""} converts "" to NA. By default, blank strings "" are
read as is.}

\item{convert.missing.value}{(logical) Converts all missing value codes
specified in \code{eml} (e.g. "-99999", "NaN", "Not measured") to NA.
Missing value codes vary across data packages and converting to a consistent
form recognized by R makes downstream use simpler. However, care must be
exercised when using this argument. The author of a dataset described by
\code{eml} may have defined "missing value code" to mean something different
than you expect (e.g. "below detection limit") therefore reviewing the
authors missing value code definitions is a good idea. Default is FALSE.}

\item{add.units}{(logical) If TRUE, a variable's unit of measurement will be
added to the table in a separate column with a column name of the form:
\code{<unit>_<variable_name>}. This argument is useful when gathering
variables into a long (attribute-value) table.}

\item{table.names}{(character) Character vector of one or more table names
(\code{<objectName>} from EML) to selectively download tables.}
}
\value{
(list) List of named data frames
}
\description{
Read data tables of a data package from the EML metadata.
}
\details{
This function uses \code{data.table::fread()} and uses default
argument values if the EML based values return an error.

Default settings preserve the form the data were originally published in.
}
\examples{
\dontrun{
eml <- read_metadata('knb-lter-mcm.9003.11')

tables <- read_tables(
  eml = eml,
  strip.white = TRUE,
  na.strings = "",
  convert.missing.value = TRUE,
  add.units = TRUE)

}



}
