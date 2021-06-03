#' Config Finder for Complex Themes
#
#' Some themes, like the popular wowchemy academic themes, place the config.toml or
#' config.yaml to unusual paths. This helper copies the config to the project root.
#' @param config_dir Defaults to the wowchemy template defaults \code{ file.path( "config", "_default")}
#' @export

config_helper <- function( config_dir =  file.path( "config", "_default") ) {

  if ( ! dir.exists ( config_dir) ) {
    warning( config_dir, " does not exist.")
    return(NULL)
  }

  potential_config_files <- file.exists (file.path(config_dir, c('config.toml', 'config.yaml')))

  if (! any(potential_config_files))  {
    warning("No config.toml or config.yaml was found at config_dir='", config_dir, "'")
    return(NULL)
  }

  actual_config_files <- file.path( config_dir, c('config.toml', 'config.yaml') )[potential_config_files]
  message ( "Copying ", actual_config_files, " to the root directory of the project.")

  file.copy (
    from = actual_config_files,
    to = c('config.toml', 'config.yaml')[potential_config_files],
    overwrite = TRUE
  )

}
