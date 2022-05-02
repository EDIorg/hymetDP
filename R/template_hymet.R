#' Template create_hymetDP.R script
#'
#' @param path (character) Path to where a new directory should be created
#' @param dir.name (character) Name of the new directory which will contain the create_hymetDP.R script
#'
#' @return
#'
#' Empty create_hymetDP.R script.
#'
#' @examples
#' \dontrun{
#' template_hymet(tempdir(), 'my_hymet')
#' }
#'
#' @export
#'
template_hymet <- function(path, dir.name) {
  # Stop if directory exists

  if (dir.exists(paste0(path, '/', dir.name))){
    stop(paste0(path, '/', dir.name, ' already exists!'))
  }

  # Create parent directory ---------------------------------------------------

  message(
    paste0(
      'Templating ',
      path,
      '/',
      dir.name
    )
  )

  dir.create(
    path = paste0(
      path,
      '/',
      dir.name
    )
  )


  # Create create_hymetDP R script -------------------------------------------

  value <- file.copy(
    from = system.file(
      'extdata/create_template.R',
      package = 'hymetDP'
    ),
    to = paste0(
      path,
      '/',
      dir.name,
      '/create_hymetDP.R'
    )
  )

  message('Done.')
}
