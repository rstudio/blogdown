#' Install Hugo
#'
#' Download the appropriate Hugo executable for your platform from Github and
#' try to copy it to a system directory so \pkg{blogdown} can run the
#' \command{hugo} command to build a site. \code{update_hugo()} is a wrapper of
#' \code{install_hugo(force = TRUE)}.
#'
#' This function tries to install Hugo to \code{Sys.getenv('APPDATA')} on
#' Windows, \file{~/Library/Application Support} on macOS, and
#' \file{~/.local/share} on other platforms (such as Linux). The \command{hugo}
#' executable is installed to a subdirectory with the Hugo version number being
#' its name, e.g., \file{~/Library/Application Support/Hugo/0.76.5}. If these
#' directories are not writable, the R package directory \file{Hugo} of
#' \pkg{blogdown} will be used. If it still fails, you have to install Hugo by
#' yourself and make sure it can be found via the environment variable
#' \code{PATH}.
#'
#' This is just a helper function and may fail to choose the correct Hugo
#' executable for your operating system, especially if you are not on Windows or
#' macOS or a major Linux distribution. When in doubt, read the Hugo
#' documentation and install it by yourself: \url{https://gohugo.io}.
#'
#' If you want to install Hugo to a custom path, you can set the global option
#' \code{blogdown.hugo.dir} to a directory to store the Hugo executable before
#' you call \code{install_hugo()}, e.g., \code{options(blogdown.hugo.dir =
#' '~/Downloads/Hugo')}. This may be useful for you to use a specific version of
#' Hugo for a specific website. You can set this option per project. See
#' \href{https://bookdown.org/yihui/blogdown/global-options.html}{Section 1.4
#' Global options} for details, or store a copy of Hugo on a USB Flash drive
#' along with your website.
#' @param version The Hugo version number, e.g., \code{0.26}; the special value
#'   \code{latest} means the latest version (fetched from Github releases).
#'   Alternatively, this argument can take a file path of the zip archive or
#'   tarball of the Hugo installer that has already been downloaded from Github,
#'   in which case it will not be downloaded again.
#' @param use_brew Whether to use Homebrew (\url{https://brew.sh}) on macOS to
#'   install Hugo. This argument has been deprecated. You are not recommended to
#'   install Hugo via Homebrew, because you may accidentally update it to the
#'   latest version, which might break your existing sites.
#' @param force Whether to install Hugo even if it has already been installed.
#'   This may be useful when upgrading Hugo.
#' @param extended Whether to use extended version of Hugo that has SCSS/SASS
#'   support. You only need the extended version if you want to edit SCSS/SASS.
#' @export
install_hugo = function(
  version = 'latest', use_brew = FALSE, force = FALSE, extended = TRUE
) {

  if (!missing(use_brew)) message(
    "The argument 'use_brew' has been deprecated in install_hugo(). If you want to ",
    "install Hugo via Homebrew, please use the command line instead: brew install hugo"
  )

  if (Sys.which('hugo') != '' && !force) {
    message('It seems Hugo has been installed. Use force = TRUE to reinstall or upgrade.')
    return(invisible())
  }

  local_file = if (grepl('[.](zip|tar[.]gz)$', version) && file.exists(version))
    normalizePath(version)

  # in theory, should access the Github API using httr/jsonlite but this
  # poor-man's version may work as well
  if (version == 'latest') {
    version = xfun::github_releases('gohugoio/hugo', version)[1]
    message('The latest Hugo version is ', version)
  } else if (use_brew) {
    if (is.null(local_file)) warning(
      'when use_brew = TRUE, only the latest version of Hugo can be installed'
    ) else {
      warning(
        "A local installer was provided through version='", local_file, "', ",
        'so use_brew = TRUE was ignored.'
      )
      use_brew = FALSE
    }
  }

  if (!is.null(local_file)) version = gsub('^hugo_([0-9.]+)_.*', '\\1', basename(local_file))

  version = gsub('^[vV]', '', version)  # pure version number
  if (!is.null(ver <- getOption('blogdown.hugo.version')) && ver != version) message2(
    "You have set the option 'blogdown.hugo.version' to '", ver, "' (perhaps in .Rprofile), ",
    "but you are installing the Hugo version '", version, "' now. You may want to update ",
    "the option 'blogdown.hugo.version' accordingly.", files = existing_files('.Rprofile')
  )
  version2 = as.numeric_version(version)
  bit = if (is_64bit()) '64bit' else '32bit'
  if (extended) {
    if (bit != '64bit') stop('The extended version of Hugo is only available on 64-bit platforms')
    if (version2 < '0.43') {
      if (!missing(extended)) stop('Only Hugo >= v0.43 provides the extended version')
      extended = FALSE
    }
  }
  base = sprintf('https://github.com/gohugoio/hugo/releases/download/v%s/', version)
  owd = setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)
  unlink(sprintf('hugo_%s*', version), recursive = TRUE)

  download_zip = function(OS, type = 'zip') {
    if (is.null(local_file)) {
      zipfile = sprintf(
        'hugo_%s%s_%s-%s.%s', ifelse(extended, 'extended_', ''), version, OS, bit, type
      )
      xfun::download_file(paste0(base, zipfile), zipfile, mode = 'wb')
    } else {
      zipfile = local_file
      type = xfun::file_ext(local_file)
    }
    switch(type, zip = utils::unzip(zipfile), tar.gz = {
      files = utils::untar(zipfile, list = TRUE)
      utils::untar(zipfile)
      files
    })
  }

  files = if (is_windows()) {
    download_zip('Windows')
  } else if (is_macos()) {
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
  # from a certain version of Hugo, the executable is no longer named
  # hugo_x.y.z, so exec could be NA here, but file.rename(NA_character) is fine
  exec = files[grep(sprintf('^hugo_%s.+', version), basename(files))][1]
  if (is_windows()) {
    file.rename(exec, 'hugo.exe')
    exec = 'hugo.exe'
  } else {
    file.rename(exec, 'hugo')
    exec = 'hugo'
    Sys.chmod(exec, '0755')  # chmod +x
  }

  install_hugo_bin(exec, version)
}

install_hugo_bin = function(exec, version) {
  success = FALSE
  dirs = file.path(bin_paths(), version)
  for (destdir in dirs) {
    dir_create(destdir)
    success = file.copy(exec, destdir, overwrite = TRUE)
    if (success) break
  }
  if (!success) stop(
    'Unable to install Hugo to any of these dirs: ',
    paste(dirs, collapse = ', ')
  )
  message(
    'Hugo has been installed to "', normalizePath(destdir), '". ',
    if (is.null(getOption('blogdown.hugo.version'))) c(
      'You are recommended to set options(blogdown.hugo.version = "', version,
      '") in the .Rprofile file in your website project. See the blogdown book ',
      'for more info on .Rprofile: https://bookdown.org/yihui/blogdown/global-options.html'
    )
  )
}

#' @export
#' @rdname install_hugo
update_hugo = function() install_hugo(force = TRUE)

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
  path = file.path(if (is_windows()) {
    Sys.getenv('APPDATA', '')
  } else if (is_macos()) {
    '~/Library/Application Support'
  } else {
    '~/.local/share'
  }, dir)
  path = c(extra_path, path, pkg_file(dir, mustWork = FALSE))
  path.expand(path[path != ''])
}

# find an executable from PATH, APPDATA, system.file(), ~/bin, etc
find_exec = function(cmd, dir, version = NULL, info = '') {
  path = ''
  for (d in bin_paths(dir)) {
    if (!dir_exists(d)) next
    exec = if (is_windows()) paste0(cmd, '.exe') else cmd
    move_exec(file.path(d, exec))
    # if version = NULL, find the max version; if not found, don't use the version dir
    if (is.null(v <- version) || version == 'all') {
      v = list.files(d, '^[0-9.]+$')
      v = v[executable(file.path(d, v, exec))]
      v = as.numeric_version(v)
    }
    if (length(v) == 0) next
    if (is.null(version)) v = max(v)
    path = file.path(d, v, exec)
    path = path[executable(path)]
    if (length(path)) break else path = ''
  }

  if (identical(version, 'all')) return(if (!identical(path, '')) path)

  path2 = Sys.which(cmd)
  if (path == '' || xfun::same_path(path, path2)) {
    if (path2 == '') stop(
      cmd, if (!is.null(version)) c(' ', version), ' not found. ', info, call. = FALSE
    )
    if (!is.null(version) && (v <- .hugo_version(path2)) != version) stop(
      "Found '", cmd, "' at '", path2, "' but its version is ", v, " instead of ",
      "the requested version ", version, ". ", info, call. = FALSE
    )
    return(basename(path2))  # do not use the full path of the command
  } else {
    if (path2 != '') warning2(
      'Found ', cmd, ' at "', path, '" and "', path2, '". The former will be used. ',
      "If you don't need both copies, you may delete/uninstall the latter",
      uninstall_tip(path2), "."
    )
  }
  normalizePath(path)
}

uninstall_tip = function(p) {
  if (is_macos()) {
    p = Sys.readlink(p)
    # check if it is installed via Homebrew
    if (!grepl(r <- '^([.][.]/Cellar/)([^/]+)(/.+)$', p)) return()
    sprintf(' with system("brew uninstall %s") in R', gsub(r, '\\2', p))
  } else if (is_windows()) {
    m = if (grepl('scoop', p)) 'scoop' else if (grepl('choco', p, ignore.case = TRUE)) 'choco'
    if (!is.null(m)) sprintf(
      ' perhaps with system2("%s", c("uninstall", "%s"))', m, xfun::sans_ext(basename(p))
    )
  } else {
    ' with your system package manager such as apt or yum'
  }
}

#' Find the Hugo executable
#'
#' Search for Hugo in a series of possible installation directories (see
#' \code{\link{install_hugo}()} for these directories).
#'
#' If your website depends on a specific version of Hugo, we strongly recommend
#' that you set \code{options(blogdown.hugo.version = )} to the version number
#' you desire in the file \code{.Rprofile} in the root directory of the website
#' project, so that \pkg{blogdown} can try to find the right version of Hugo
#' before it builds or serves the website. You can use the function
#' \code{\link{config_Rprofile}()} to do this automatically.
#' @param version The expected version number, e.g., \code{'0.25.1'}. If
#'   \code{NULL}, it will try to find the maximum possible version. If
#'   \code{'all'}, find all possible versions.
#' @export
#' @return The path to the Hugo executable if found, otherwise it will signal an
#'   error, with a hint on how to install (the required version of) Hugo. If
#'   \code{version = 'all'}, return the paths of all versions of Hugo installed.
find_hugo = local({
  paths = list()  # cache the paths to hugo (there might be multiple versions)
  function(version = getOption('blogdown.hugo.version')) {
    i = if (is.null(version)) 'default' else as.character(version)
    p = paths[[i]]
    if (!is.null(p) && file.exists(exec_path(p))) return(p)
    # if path not found, find it again
    p = find_exec(
      'hugo', 'Hugo', version,
      c('You may try blogdown::install_hugo(', sprintf('"%s"', version), ').')
    )
    if (!identical(version, 'all')) paths[[i]] <<- p
    p
  }
})

executable = function(path) utils::file_test('-x', path)

# resolve a command to the actual path of the executable
exec_path = function(p) {
  if (file.exists(p)) p else Sys.which(p)
}

# move an executable to a subdirectory with the dir name being the version number
move_exec = function(cmd) {
  if (!executable(cmd)) return()
  ver = tryCatch(.hugo_version(cmd), error = function(e) NULL)
  if (is.null(ver)) return()
  xfun::in_dir(dirname(cmd), {
    ver = as.character(ver); cmd = basename(cmd)
    dir_create(ver); file.rename(cmd, file.path(ver, cmd))
  })
}
