#' Install Hugo
#'
#' Download the appropriate Hugo executable for your platform from Github and
#' try to copy it to a system directory so \pkg{blogdown} can run the
#' \command{hugo} command to build a site.
#'
#' This function tries to install Hugo to \code{Sys.getenv('APPDATA')} on
#' Windows, \file{~/Library/Application Support} on macOS, and \file{~/bin/} on
#' other platforms (such as Linux). If these directories are not writable, the
#' package directory \file{Hugo} of \pkg{blogdown} will be used. If it still
#' fails, you have to install Hugo by yourself and make sure it can be found via
#' the environment variable \code{PATH}.
#'
#' This is just a helper function and may fail to choose the correct Hugo
#' executable for your operating system, especially if you are not on Windows or
#' Mac or a major Linux distribution. When in doubt, read the Hugo documentation
#' and install it by yourself: \url{https://gohugo.io}.
#' @param version The Hugo version number, e.g., \code{0.17}; the special value
#'   \code{latest} means the latest version (fetched from Github releases).
#' @param use_brew Whether to use Homebrew (\url{https://brew.sh}) on macOS to
#'   install Hugo (recommended because it is much easier to manage packages).
#'   Note Homebrew will be automatically installed if it has not been installed.
#' @param force Whether to install Hugo even if it has already been installed.
#'   This may be useful when upgrading Hugo (if you use Homebrew, run the
#'   command \command{brew update && brew upgrade} instead).
#' @export
install_hugo = function(version = 'latest', use_brew = TRUE, force = FALSE) {

  if (Sys.which('hugo') != '' && !force) return(invisible())

  # in theory, should access the Github API using httr/jsonlite but this
  # poor-man's version may work as well
  if (version == 'latest') {
    json = readLines(
      'https://api.github.com/repos/spf13/hugo/releases/latest', warn = FALSE
    )
    r = '^.*?"tag_name":\\s*"([^"]+)",.*'
    version = gsub(r, '\\1', grep(r, json, value = TRUE)[1])
  }
  version = gsub('^[vV]', '', version)  # pure version number
  version2 = as.numeric_version(version)
  bit = if (grepl('64', Sys.info()[['machine']])) '64bit' else '32bit'
  base = sprintf('https://github.com/spf13/hugo/releases/download/v%s/', version)
  owd = setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)
  unlink(sprintf('hugo_%s*', version), recursive = TRUE)

  download_zip = function(OS, type = 'zip') {
    zipfile = sprintf('hugo_%s_%s-%s.%s', version, OS, bit, type)
    download2(paste0(base, zipfile), zipfile, mode = 'wb')
    switch(type, zip = utils::unzip(zipfile), tar.gz = {
      files = utils::untar(zipfile, list = TRUE)
      utils::untar(zipfile)
      files
    })
  }

  files = if (is_windows()) {
    download_zip('Windows')
  } else if (is_osx()) {
    if (use_brew) {
      if (brew_hugo() == 0) return()
      warning(
        'Failed to use Homebrew to install Hugo. ',
        'I will try to download the Hugo binary directly and install it.'
      )
    }
    download_zip(
      if (version2 >= '0.18') 'macOS' else 'MacOS',
      if (version2 >= '0.20.3') 'tar.gz' else 'zip'
    )
  } else {
    download_zip('Linux', 'tar.gz')  # _might_ be Linux; good luck
  }
  exec = files[grep(sprintf('^hugo_%s.+', version), basename(files))][1]
  if (is_windows()) {
    file.rename(exec, 'hugo.exe')
    exec = 'hugo.exe'
  } else {
    file.rename(exec, 'hugo')
    exec = 'hugo'
    Sys.chmod(exec, '0755')  # chmod +x
  }

  success = FALSE
  dirs = bin_paths()
  for (destdir in dirs) {
    dir.create(destdir, showWarnings = FALSE)
    success = file.copy(exec, destdir, overwrite = TRUE)
    if (success) break
  }
  file.remove(exec)
  if (!success) stop(
    'Unable to install Hugo to any of these dirs: ',
    paste(dirs, collapse = ', ')
  )
  message('Hugo has been installed to ', normalizePath(destdir))
}

brew_hugo = function() {
  install = function() system('brew update && brew reinstall hugo')
  status = 1  # reinstall Homebrew if `brew install hugo` failed
  if (Sys.which('brew') == '' || (status <- install()) != 0) system2(
    '/usr/bin/ruby',
    '-e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"'
  )
  if (status == 0) status else install()
}

# possible locations of the Hugo executable
bin_paths = function(dir = 'Hugo', extra_path = getOption('blogdown.hugo.dir')) {
  if (is_windows()) {
    path = Sys.getenv('APPDATA', '')
    path = if (dir_exists(path)) file.path(path, dir)
  } else if (is_osx()) {
    path = '~/Library/Application Support'
    path = if (dir_exists(path)) file.path(path, dir)
  } else {
    path = '~/bin'
  }
  path = c(extra_path, path, pkg_file(dir, mustWork = FALSE))
  path
}

# find an executable from PATH, APPDATA, system.file(), ~/bin, etc
find_exec = function(cmd, dir, info = '') {
  for (d in bin_paths(dir)) {
    exec = if (is_windows()) paste0(cmd, ".exe") else cmd
    path = file.path(d, exec)
    if (utils::file_test("-x", path)) break else path = ''
  }
  if (path == '') {
    path = Sys.which(cmd)
    if (path == '') stop(
      cmd, ' not found. ', info, call. = FALSE
    )
    return(cmd)  # do not use the full path of the command
  }
  normalizePath(path)
}

find_hugo = local({
  path = NULL  # cache the path to hugo
  function() {
    if (is.null(path)) {
      path <<- find_exec(
        'hugo', 'Hugo', 'You can install it via blogdown::install_hugo()'
      )
      ver = hugo_version()
      if (is.numeric_version(ver) && ver < '0.18') stop(
        'Found Hugo at ', path, ' but the version is too low (', ver, '). ',
        'You may try blogdown::install_hugo(force = TRUE).'
      )
    }
    path
  }
})
