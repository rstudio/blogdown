pkg_file = function(..., mustWork = TRUE) {
  system.file(..., package = 'blogdown', mustWork = mustWork)
}

# only copy files/dirs if they exist
file.copy2 = function(from, to, ...) {
  i = file.exists(from)
  file.copy(from[i], if (length(to) == 1) to else to[i], ...)
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
  if (Sys.which('wget') != '') {
    method = 'wget'
  } else if (Sys.which('curl') != '') {
    method = 'curl'
    # curl needs to add a -L option to follow redirects
    opts = options(download.file.extra = paste('-L', getOption('download.file.extra')))
    on.exit(options(opts), add = TRUE)
  } else if (Sys.which('lynx') != '') {
    method = 'lynx'
  } else {
    stop('no download method found (wget/curl/lynx)')
  }

  download.file(url, method = method, ...)
}
