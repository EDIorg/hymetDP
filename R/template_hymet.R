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
