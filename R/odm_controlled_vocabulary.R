#' View one of CUAHSI's ODM Controlled Vocabularies
#'
#' @description This function returns a user-selected ODM controlled vocabulary to the console. The returned object may be useful for searching for appropriate terms.
#'
#' @param vocab (character) The controlled vocabulary to be returned.
#'
#' @return (tbl_df, tbl, data.frame) The controlled vocabulary formatted as a tibble.
#'
#' @details Use the search field to find suitable terms for creating hymetDP tables.
#'
#' @examples
#'
#' \dontrun{
#' # View the ODM VariableNameCV
#'
#' View(odm_controlled_vocabulary('VariableName'))
#' }
#'
#' @export
#'
#'
odm_controlled_vocabulary <- function(vocab = 'Units') {

  # TODO support a character vector to return multiple CVs

  odmcv <- get_odm_controlled_vocabulary(vocab = vocab)

  message(paste0("Viewing the ODM 1.1 ", vocab, " controlled vocabulary"))

  odmcv[[vocab]]

}
