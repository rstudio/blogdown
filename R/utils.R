#' Live preview a site
#'
#' Calls \code{servr::\link{httw}()} to watch for changes in the site, rebuild
#' the site if necessary, and refresh the page automatically.
#' @param ... Arguments passed to \code{servr::httw()}.
#' @export
serve_site = function(...) {
  render_pages()
  servr::httw(site.dir = publish_dir(), handler = function(...) {
    # re-generate only if Rmd/md or config files were updated
    if (length(grep('[.](R?md|toml|yaml)$', c(...)))) render_pages()
  }, ...)
}

pkg_file = function(..., mustWork = TRUE) {
  system.file(..., package = 'blogdown', mustWork = mustWork)
}

# only copy files/dirs if they exist
file.copy2 = function(from, to, ...) {
  i = file.exists(from); from = from[i]
  if (length(from) == 0) return()
  if (length(to) > 1) {
    to = to[i]
    if (length(unique(to)) == 1) to = unique(to)
  }
  if (length(to) == 1) {
    file.copy(from, to, ...)
  } else mapply(file.copy, from, to, ...)
}

is_windows = function() .Platform$OS.type == 'windows'
is_osx = function() Sys.info()[['sysname']] == 'Darwin'
is_linux = function() Sys.info()[['sysname']] == 'Linux'

# adapted from webshot:::download_no_libcurl due to the fact that
# download.file() cannot download Github release assets:
# https://stat.ethz.ch/pipermail/r-devel/2016-June/072852.html
download2 = function(url, ...) {
  if (is_windows())
    return(download.file(url, method = 'wininet', ...))

  # if non-Windows, check for libcurl/curl/wget/lynx, call download.file with
  # appropriate method
  if (Sys.which('curl') != '') {
    method = 'curl'
    # curl needs to add a -L option to follow redirects
    opts = options(download.file.extra = paste('-L', getOption('download.file.extra')))
    on.exit(options(opts), add = TRUE)
  } else if (Sys.which('wget') != '') {
    method = 'wget'
  } else if (Sys.which('lynx') != '') {
    method = 'lynx'
  } else {
    stop('no download method found (wget/curl/lynx)')
  }

  download.file(url, method = method, ...)
}

opts = knitr:::new_defaults()

load_config = function() {
  config = opts$get('config')

  # read config only if it has been updated
  read_config = function(f, parser) {
    if (!is.null(time <- attr(config, 'config_time')) &&
        time == file.info(f)[, 'mtime']) return(config)
    config = parser(f)
    attr(config, 'config_time') = file.info(f)[, 'mtime']
    opts$set(config = config)
    config
  }

  if (file.exists('config.yaml'))
    return(read_config('config.yaml', yaml::yaml.load_file))

  if (file.exists('config.toml'))
    return(read_config('config.toml', parse_toml))

  stop(
    'Cannot find the configuration file config.yaml or config.toml of the website'
  )
}

# not TOML parser in R yet, so a simple version that only reads top-level options
parse_toml = function(f, x = readUTF8(f)) {
  z = list()
  # strings
  r = '^([[:alnum:]]+?)\\s*=\\s*"([^"]*?)"\\s*$'
  y = grep(r, x, value = TRUE)
  z[gsub(r, '\\1', y)] = as.list(gsub(r, '\\2', y))
  # boolean
  r = '^([[:alnum:]]+?)\\s*=\\s*(true|false)\\s*$'
  y = grep(r, x, value = TRUE)
  z[gsub(r, '\\1', y)] = as.list(as.logical(gsub(r, '\\2', y)))
  # numbers
  r = '^([[:alnum:]]+?)\\s*=\\s*([0-9.]+)\\s*$'
  y = grep(r, x, value = TRUE)
  z[gsub(r, '\\1', y)] = as.list(as.numeric(gsub(r, '\\2', y)))
  z
}

get_config = function(field, default, config = load_config()) {
  config[[field]] %n% default
}

publish_dir = function(config = load_config()) {
  get_config('publishdir', 'public', config)
}

# use RStudio to open the file if possible
open_file = function(x) {
  tryCatch(rstudioapi::navigateToFile(x), error = function(e) file.edit(x))
}
