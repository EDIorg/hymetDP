#' Create the Sites table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset,
#'   in "flat" format (see details).
#' @param SiteCode (character) Column in \code{L0_flat} containing the user or
#'   organization-defined code that collects the data to identify the site.
#' @param SiteName (character) Column in \code{L0_flat} containing the full name
#'   of the sampling site.
#' @param Latitude (character) Column in \code{L0_flat} containing the latitude
#'   in decimal degrees.
#' @param Longitude (character) Column in \code{L0_flat} containing the
#'   longitude in decimal degrees.
#' @param LatLongDatumSRSName (character) Column in \code{L0_flat} containing
#'   the The spatial reference system of the latitude and longitude coordinates.
#'   Choose an SRSName where IsGeographic=True from \code{SpatialReferencesCV}.
#'   View possible options with
#'   \code{SpatialReferencesCV$SRSName[SpatialReferencesCV$IsGeographic]}
#' @param Elevation_m (character) Column in \code{L0_flat} containing the
#'   elevation of the sampling location in meters.
#' @param VerticalDatum (character) Column in \code{L0_flat} containing the
#'   Vertical datum of the elevation. Choose a Term from the VerticalDatum
#'   controlled vocabulary.
#' @param LocalX (character) Column in \code{L0_flat} containing the local
#'   projection X coordinate.
#' @param LocalY (character) Column in \code{L0_flat} containing the local
#'   projection Y coordinate.
#' @param LocalProjectionSRSName (character) Column in \code{L0_flat} containing
#'   the full text name of the spatial reference system of the local
#'   coordinates. This field is optional and is only necessary if local
#'   coordinates are given. Choose an SRSName from from
#'   \code{SpatialReferencesCV}.
#' @param PosAccuracy_m (character) Column in \code{L0_flat} containing the
#'   value giving the accuracy with which the positional information is
#'   specified in meters.
#' @param State (character) Column in \code{L0_flat} containing the name of
#'   state in which the monitoring site is located.
#' @param County (character) Column in \code{L0_flat} containing the name of
#'   county in which monitoring site is located.
#' @param Comments (character) Column in \code{L0_flat} containing the comments
#'   related to the site.
#' @param SiteType (character) Column in \code{L0_flat} containing the the type
#'   of site. Choose a Term from \code{SiteTypeCV}.
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
#' @family create required tables
#'
#' @return
#'
#' @examples
#'
#' flat <- hymet_L0_flat
#'
#'   Sites <- hymetDP::create_sites(
#'     L0_flat = flat,
#'     SiteCode = "SiteCode",
#'     SiteName = "SiteName",
#'     Latitude = "Latitude",
#'     Longitude = "Longitude",
#'     LatLongDatumSRSName = NULL,
#'     Elevation_m = NULL,
#'     VerticalDatum = NULL,
#'     LocalX = NULL,
#'     LocalY = NULL,
#'     LocalProjectionSRSName = NULL,
#'     PosAccuracy_m = NULL,
#'     State = NULL,
#'     County = NULL,
#'     Comments = NULL,
#'     SiteType = "SiteType")
#'
#'   Sites
#'
#' @export
#'
create_sites <- function(
  L0_flat = flat,
  SiteCode,
  SiteName,
  Latitude,
  Longitude,
  LatLongDatumSRSName = NULL,
  Elevation_m = NULL,
  VerticalDatum = NULL,
  LocalX = NULL,
  LocalY = NULL,
  LocalProjectionSRSName = NULL,
  PosAccuracy_m = NULL,
  State = NULL,
  County = NULL,
  Comments = NULL,
  SiteType = NULL) {

  validate_arguments(fun.name = "create_sites", fun.args = as.list(environment()))

  cols_to_gather <- c(SiteCode,
                      SiteName,
                      Latitude,
                      Longitude,
                      LatLongDatumSRSName,
                      Elevation_m,
                      VerticalDatum,
                      LocalX,
                      LocalY,
                      LocalProjectionSRSName,
                      PosAccuracy_m,
                      State,
                      County,
                      Comments,
                      SiteType)

  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct()

  # add default values

  if (is.null(LatLongDatumSRSName) | all(res$LatLongDatumSRSName) == "") {
    res$LatLongDatumSRSName <- "Unknown"
  }
  if (is.null(VerticalDatum)) {
    res$VerticalDatum <- "Unknown"
  }
  if (is.null(LocalProjectionSRSName)) {
    res$LocalProjectionSRSName <- "Unknown"
  }
  if (is.null(SiteType)) {
    res$SiteType <- "Unknown"
  }

  # add missing columns

  if (is.null(Elevation_m)) {
    res$Elevation_m <- NA_real_
  }
  if (is.null(LocalX)) {
    res$LocalX <- NA_real_
  }
  if (is.null(LocalY)) {
    res$LocalY <- NA_real_
  }
  if (is.null(PosAccuracy_m)) {
    res$PosAccuracy_m <- NA_character_
  }
  if (is.null(State)) {
    res$State <- NA_character_
  }
  if (is.null(County)) {
    res$County <- NA_character_
  }
  if (is.null(Comments)) {
    res$Comments <- NA_character_
  }

  # reorder
  res <- res %>%
    dplyr::select(SiteCode,
                  SiteName,
                  Latitude,
                  Longitude,
                  LatLongDatumSRSName,
                  Elevation_m,
                  VerticalDatum,
                  LocalX,
                  LocalY,
                  LocalProjectionSRSName,
                  PosAccuracy_m,
                  State,
                  County,
                  Comments,
                  SiteType)

  # coerce classes
  res <- coerce_table_classes(res, "Sites", class(L0_flat))
  return(res)

}
