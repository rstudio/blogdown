#' Install Hugo
#'
#' Download the appropriate Hugo executable for your platform from Github and
#' try to copy it to a system directory so \pkg{blogdown} can run the
#' \command{hugo} command to build a site.
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
#' @param extended Whether to use extended version of Hugo that has SCSS/SASS
#'   support. You only need the extended version if you want to edit SCSS/SASS.
#'   Note that this feature is not available to Hugo version lower than v0.43.
#'   It also requires a 64-bit system; if the system is based on the ARM
#'   architecture, only macOS is supported at the moment.
#' @param arch,os The architecture and operating system name. These arguments,
#'   along with \code{version} and \code{extended}, determines the filename of
#'   the Hugo installer. See \url{https://github.com/gohugoio/hugo/releases} for
#'   all of Hugo's installers. By default, the argument values are automatically
#'   detected. In case the detection should fail, you can provide the values
#'   manually, e.g., \code{extended = FALSE}, \code{arch = 'ARM64'}, and
#'   \code{os = 'FreeBSD'} would install \file{hugo_*_FreeBSD-ARM.tar.gz}.
#' @param force Whether to reinstall Hugo if the specified version has been
#'   installed.
#' @param ... Ignored.
#' @note For macOS users, you are not recommended to install Hugo via Homebrew,
#'   because you may accidentally update it to the latest version, which might
#'   break your existing sites.
#' @seealso \code{\link{remove_hugo}()} to remove Hugo.
#' @export
install_hugo = function(
  version = 'latest', extended = TRUE, arch = 'auto', os = 'auto', force = FALSE, ...
) {
  local_file = if (grepl('[.](zip|tar[.]gz)$', version) && file.exists(version))
    normalizePath(version)

  # get the latest version of Hugo
  if (version == 'latest') {
    version = xfun::github_releases('gohugoio/hugo', version)[1]
    message('The latest Hugo version is ', version)
  } else {
    if (!is.null(local_file)) version = gsub(
      '^hugo(_extended)(_withdeploy)?_([0-9.]+)_.*', '\\3', basename(local_file)
    )
  }

  version = gsub('^[vV]', '', version)  # pure version number
  if (!force && hugo_available(version, exact = TRUE)) return(message(
    'Hugo ', version, ' has already been installed. To reinstall, use the ',
    'argument force = TRUE.'
  ))
  if (is.null(local_file)) {
    if (grepl('^\\d+\\.\\d+$', version) && as.numeric_version(version) >= '0.54') {
      # Hugo started to to use version number x.y.0 instead of x.y since 0.54.0
      version3 = paste0(version, '.0')
      xfun::try_silent(if (length(xfun::github_releases('gohugoio/hugo', paste0('v', version3)))) {
        message('The version ', version, ' was automatically corrected to ', version3, '.')
        version = version3
      })
    }
    ver = tryCatch(
      xfun::github_releases('gohugoio/hugo', sub('^[vV]?', 'v', version)),
      error = function(e) NULL
    )
    if (length(ver) == 0) stop('The Hugo version ', version, ' does not seem to exist.')
  }
  if (!is.null(ver <- get_option('blogdown.hugo.version')) && ver != version) message2(
    "You have set the option 'blogdown.hugo.version' to '", ver, "' (perhaps in .Rprofile), ",
    "but you are installing the Hugo version '", version, "' now. You may want to update ",
    "the option 'blogdown.hugo.version' accordingly.", files = existing_files('.Rprofile')
  )
  version2 = as.numeric_version(version)
  # hugo merged ARM64 and 64bit in 0.89.0 (#664) and >= 0.102.0 (#727) releases for macOS
  if (arch == 'auto' && is_macos()) {
    if (version2 == '0.89.0') arch = 'all'
    if (version2 > '0.101.0') arch = 'universal'
  }
  if (arch == 'auto') arch = detect_arch()
  # the extended version is only available for Hugo >= 0.43 and (64bit OS or non-32bit macOS)
  if (missing(extended)) extended = (version2 >= '0.43') &&
    (arch == '64bit' || (is_macos() && arch != '32bit'))
  base = sprintf('https://github.com/gohugoio/hugo/releases/download/v%s/', version)
  owd = setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)
  unlink(sprintf('hugo_%s*', version), recursive = TRUE)

  v103 = version2 >= '0.103.0'
  download_zip = function(OS, type = 'zip') {
    if (os == 'auto') {
      if (v103) OS = tolower(OS)
    } else OS = os  # if user has provided the OS name, use that one
    if (is.null(local_file)) {
      if (grepl('^arm', arch)) arch = toupper(arch)  # arm(64) -> ARM(64)
      if (v103) {
        arch = tolower(arch)
        if (arch == '64bit') arch = 'amd64'
      }
      # v0.20.3 is a special case: it has 'v' in the filename
      zipfile = sprintf(
        'hugo_%s%s_%s-%s.%s', ifelse(extended, 'extended_', ''),
        paste0(if (version == '0.20.3') 'v', version), OS, arch, type
      )
      xfun::download_file(paste0(base, zipfile), zipfile, mode = 'wb', .error = c(
        'Failed to download ', zipfile, ' from https://github.com/gohugoio/hugo/releases/tag/v',
        version, '. Please check blogdown::hugo_installers("', version, '") for ',
        'available Hugo installers, and use the appropriate os/arch/extended arguments ',
        'for blogdown::install_hugo().'
      ))
    } else {
      zipfile = local_file
      type = xfun::file_ext(local_file)
    }
    switch(type, zip = utils::unzip(zipfile), tar.gz = {
      files = utils::untar(zipfile, list = TRUE)
      utils::untar(zipfile)
      files
    }, pkg = {
      if (interactive()) system2('open', shQuote(zipfile)) else {
        if (system2('sudo', c('installer', '-pkg', zipfile, '-target', '/')) != 0) stop(
          'The *.pkg installer requires password input. Please run this function ',
          'in the terminal or in an interactive R session and input password.'
        )
      }
      NULL
    })
  }

  files = if (is_windows()) {
    download_zip('Windows')
  } else if (is_macos()) {
    download_zip(
      if (v103) 'darwin' else if (version2 >= '0.18') 'macOS' else 'MacOS',
      if (version2 >= '0.153.0') 'pkg' else if (version2 >= '0.20.3') 'tar.gz' else 'zip'
    )
  } else {
    download_zip('Linux', 'tar.gz')  # _might_ be Linux; good luck
  }
  # can't tell if .pkg was successfully installed at this point
  if (is.null(files)) return(invisible(NULL))
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
    if (is.null(get_option('blogdown.hugo.version'))) c(
      'You are recommended to set options(blogdown.hugo.version = "', version,
      '") in the .Rprofile file in your website project. See the blogdown book ',
      'for more info on .Rprofile: https://bookdown.org/yihui/blogdown/global-options.html'
    )
  )
}

#' Available Hugo installers of a version
#'
#' Given a version number, return the information of available installers. If
#' \code{\link{install_hugo}()} fails, you may run this function to check the
#' available installers and obtain their \code{os}/\code{arch} info.
#' @param version A version number. The default is to automatically detect the
#'   latest version. Versions before v0.17 are not supported.
#' @return A data frame containing columns \code{os} (operating system),
#'   \code{arch} (architecture), \code{extended} (extended version or not),
#'   \code{withdeploy} (with the deploy feature or not). If your R version is
#'   lower than 4.1.0, a character vector of the installer filenames will be
#'   returned instead.
#' @export
#' @examplesIf interactive()
#' blogdown::hugo_installers()
#' blogdown::hugo_installers('0.89.0')
#' blogdown::hugo_installers('0.17')
hugo_installers = function(version = 'latest') {
  repo = 'gohugoio/hugo'
  if (version == 'latest') version = xfun::github_releases(repo, 'latest')[1]
  version = sub('^[vV]?', 'v', version)
  json = xfun::loadable('jsonlite')
  res = xfun::github_api(sprintf('/repos/%s/releases/tags/%s', repo, version), raw = !json)
  res = if (json) {
    lapply(res$assets, `[[`, 'browser_download_url')
  } else {
    res = strsplit(res, '"browser_download_url":"')
    xfun::grep_sub('^(https://[^"]+)".*', '\\1', unlist(res))
  }
  res = grep('[.](zip|tar[.]gz|pkg)$', unlist(res), value = TRUE)
  res = basename(res)
  if (!'gregexec' %in% ls(baseenv())) {
    warning('Your R version is too low (< 4.1.0) and does not have gregexec().')
    return(res)
    gregexec = regexec  # a hack to pass R CMD check without NOTE
  }
  m = gregexec('^hugo_(extended_)?(withdeploy_)?([^_]+)_([^-]+)-([^.]+)[.]([a-z.]+)$', res)
  res = lapply(regmatches(res, m), function(x) if (length(x) >= 6) x[c(1, 4, 5, 6, 2, 3)])
  res = do.call(rbind, res)
  colnames(res) = c('installer', 'version', 'os', 'arch', 'extended', 'withdeploy')
  res = as.data.frame(res)
  res$extended = res$extended == 'extended_'
  res$withdeploy = res$withdeploy == 'withdeploy_'
  rownames(res) = res$installer
  res = res[, -1]
  res
}

#' @export
#' @rdname install_hugo
update_hugo = function() {
  stop('blogdown::update_hugo() is defunct. Please use blogdown::install_hugo() instead.')
}

# possible locations of the Hugo executable
bin_paths = function(dir = 'Hugo', extra_path = get_option('blogdown.hugo.dir')) {
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
find_exec = function(cmd, dir, version = NULL, info = '', quiet = FALSE) {
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
  path2 = Sys.which(cmd)

  if (identical(version, 'all')) {
    path = c(path, path2)
    if (is_windows()) path = xfun::normalize_path(path)
    return(unname(unique(path[path != ''])))
  }

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
    if (!quiet && path2 != '') msg2(
      'Found ', cmd, ' at "', path, '" and "', path2, '". The former will be used. ',
      "If you don't need both copies, you may delete/uninstall the latter",
      uninstall_tip(path2), "."
    )
  }
  xfun::normalize_path(path)
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

#' Find or remove the Hugo executable
#'
#' Search for Hugo in a series of possible installation directories (see
#' \code{\link{install_hugo}()} for these directories) with \code{find_hugo()},
#' or remove the Hugo executable(s) found with \code{remove_hugo()}.
#'
#' If your website depends on a specific version of Hugo, we strongly recommend
#' that you set \code{options(blogdown.hugo.version = )} to the version number
#' you desire in the file \code{.Rprofile} in the root directory of the website
#' project, so that \pkg{blogdown} can try to find the right version of Hugo
#' before it builds or serves the website. You can use the function
#' \code{\link{config_Rprofile}()} to do this automatically.
#' @param version The expected version number, e.g., \code{'0.25.1'}. If
#'   \code{NULL}, it will try to find/remove the maximum possible version. If
#'   \code{'all'}, find/remove all possible versions. In an interactive R
#'   session when \code{version} is not provided, \code{remove_hugo()} will list
#'   all installed versions of Hugo, and you can select which versions to
#'   remove.
#' @param quiet Whether to signal a message when two versions of Hugo are found:
#'   one is found on the system \var{PATH} variable, and one is installed by
#'   \code{\link{install_hugo}()}.
#' @export
#' @return For \code{find_hugo()}, it returns the path to the Hugo executable if
#'   found, otherwise it will signal an error, with a hint on how to install
#'   (the required version of) Hugo. If Hugo is found via the environment
#'   variable \var{PATH}, only the base name of the path is returned (you may
#'   use \code{\link{Sys.which}('hugo')} to obtain the full path).
#'
#'   If \code{version = 'all'}, return the paths of all versions of Hugo
#'   installed.
find_hugo = local({
  paths = list()  # cache the paths to hugo (there might be multiple versions)
  function(version = getOption('blogdown.hugo.version'), quiet = FALSE) {
    i = if (is.null(version <- na2null(version))) 'default' else {
      # in case the version number starts with v, remove it
      version = sub('^[vV]([0-9.]+)$', '\\1', as.character(version))
    }
    p = paths[[i]]
    if (!is.null(p) && file.exists(exec_path(p))) return(p)
    # if path not found, find it again
    p = find_exec(
      'hugo', 'Hugo', version,
      c('You may try blogdown::install_hugo(', sprintf('"%s"', version), ').'),
      quiet
    )
    if (!identical(version, 'all')) paths[[i]] <<- p
    p
  }
})

#' @param force By default, \code{remove_hugo()} only removes Hugo installed via
#'   \code{\link{install_hugo}()}. For \code{force = TRUE}, it will try to
#'   remove any Hugo executables found via \code{find_hugo()}.
#' @export
#' @rdname find_hugo
remove_hugo = function(version = getOption('blogdown.hugo.version'), force = FALSE) {
  if (length(vers <- find_hugo('all')) == 0) return(msg_cat('Hugo not found.\n'))
  ver = find_hugo(version, TRUE)  # the version currently used
  if (interactive() && missing(version)) {
    title = paste0(
      hrule(), '\n', n <- length(vers), ' Hugo version', if (n > 1) 's',
      ' found and listed below',
      sprintf(' (#%d on the list is currently used)', which(vers == ver)),
      '. Which version(s) would you like to remove?\n', hrule()
    )
    ver = select.list(vers, title = title, multiple = TRUE, graphics = FALSE)
  }
  for (f in ver) {
    if (!file_exists(f)) f = Sys.which(f)  # e.g., hugo.exe returned from find_hugo()
    d = dirname(f)
    # the parent folder name must be a version number
    if (force || is_version(basename(d))) {
      message("Removing '", f, "'...")
      unlink(f)
      del_empty_dir(d)
    } else warning2(
      "'", f, "' does not seem to be installed via blogdown::install_hugo(), ",
      "and I will not remove it. If you are sure it can be removed, you may ",
      "delete it with blogdown::remove_hugo(force = TRUE)",
      sprintf(', or%s (the latter is recommended)', uninstall_tip(f)), "."
    )
  }
}

is_version = function(x) grepl('^([0-9]+)([.][0-9]+)*$', x)

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
