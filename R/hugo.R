#' @param ... Arguments to be passed to \code{system2('hugo', ...)}, e.g.
#'   \code{new_content(path)} is basically \code{hugo_cmd(c('new', path))} (i.e.
#'   run the command \command{hugo new path}).
#' @export
#' @describeIn hugo_cmd Run an arbitrary Hugo command.
hugo_cmd = function(...) {
  on.exit(clean_hugo_cache(), add = TRUE)
  system2(find_hugo(), ...)
}

#' @export
#' @describeIn hugo_cmd Return the version number of Hugo if possible, which is
#'   extracted from the output of \code{hugo_cmd('version')}.
hugo_version = local({
  time = NULL  # last modification time of the executable
  ver  = NULL  # cache the version
  function() {
    time2 = file.mtime(exec_path(cmd <- find_hugo(quiet = TRUE)))
    if (!is.null(ver) && identical(time2, time)) return(ver)
    time <<- time2
    ver  <<- .hugo_version(cmd)
    ver
  }
})

.hugo_version = function(cmd) {
  x = system2(cmd, 'version', stdout = TRUE)
  r = '^.* v([0-9.]{2,}).*$'
  if (!isTRUE(grepl(r, x))) stop(paste(
    c("Cannot extract the version number from '", cmd, "':\n", x), collapse = '\n'
  ))
  as.numeric_version(gsub(r, '\\1', x))
}

#' @param version A version number.
#' @param exact If \code{FALSE}, check if the current Hugo version is equal to
#'   or higher than the specified \code{version}. If \code{TRUE}, check if the
#'   exact version is available.
#' @export
#' @describeIn hugo_cmd Check if Hugo of a certain version (or above if
#'   \code{exact = FALSE}) is available.
#' @examples blogdown::hugo_available('1.2.3')
hugo_available = function(version = '0.0.0', exact = FALSE) {
  tryCatch((if (exact) `==` else `>=`)(hugo_version(), version), error = function(e) FALSE)
}

#' @param local Whether to build the site for local preview (if \code{TRUE}, all
#'   drafts and future posts will also be built).
#' @param args A character vector of command-line arguments to be passed to
#'   \command{hugo}, e.g., \code{c("--minify", "--quiet")}.
#' @param baseURL,relativeURLs Custom values of \code{baseURL} and
#'   \code{relativeURLs} to override Hugo's default and the settings in the
#'   site's config file.
#' @export
#' @describeIn hugo_cmd Build a plain Hugo website. Note that the function
#'   \code{\link{build_site}()} first compiles Rmd files, and then calls Hugo
#'   via \code{hugo_build()} to build the site.
hugo_build = function(
  local = FALSE, args = getOption('blogdown.hugo.args'),
  baseURL = NULL, relativeURLs = NULL
) {
  config = load_config()
  # Hugo 0.48 generates an ugly empty resources/ dir in the root dir
  on.exit(del_empty_dir('resources'), add = TRUE)
  tweak_hugo_env(baseURL, relativeURLs)

  hugo_cmd(c(
    if (local) c('-D', '-F'), na2null(args),
    '-d', shQuote(publish_dir(config)), theme_flag(config)
  ))
}

theme_flag = function(config = load_config()) {
  if (generator() != 'hugo') return()
  d = getOption('blogdown.themesDir', get_config('themesDir', 'themes', config))
  t = list.files(d)
  t = if (length(t) > 0) t[1]
  t = get_config('theme', t, config)
  a = c('--themesDir', d)
  if (length(t) == 1) a = c(a, '-t', t)
  a
}

theme_dir = function(...) {
  if (length(x <- theme_flag()) == 4) file.path(x[2], x[4])
}

change_config = function(name, value) {
  f = find_config()
  x = read_utf8(f)
  if (f == 'config.toml') {
    r = sprintf('^%s\\s*=.+', name)
    v = if (!is.na(value)) paste(name, value, sep = ' = ')
  } else if (f == 'config.yaml') {
    r = sprintf('^%s\\s*:.+', name)
    v = if (!is.na(value)) paste(name, value, sep = ': ')
  }
  i = grep(r, x, ignore.case = TRUE)
  if (length(i) > 1) stop("Duplicated configuration for '", name, "' in ", f)
  x0 = x
  if (length(i) == 1) {
    if (is.null(v)) x = x[-i] else x[i] = v  # replace old config
  } else {
    x = c(v, x)  # append new config and write out
  }
  write_utf8(x, f)
  invisible(list(text = x0, file = f))
}

#' Run Hugo commands
#'
#' Wrapper functions to run Hugo commands via \code{\link{system2}('hugo',
#' ...)}.
#' @param dir The directory of the new site.
#' @param force Whether to create the site in a directory even if it is not
#'   empty. By default, \code{force = TRUE} when the directory only contains
#'   hidden, RStudio project (\file{*.Rproj}), \file{LICENSE}, and/or
#'   \file{README} files.
#' @param install_hugo Whether to install Hugo automatically if it is not found.
#' @param format The format of the configuration file, e.g., \code{'yaml'} or
#'   \code{'toml'} (the value \code{TRUE} will be treated as \code{'yaml'}, and
#'   \code{FALSE} means \code{'toml'}). Note that the frontmatter of the new (R)
#'   Markdown file created by \code{new_content()} always uses YAML instead of
#'   TOML or JSON.
#' @param sample Whether to add sample content. Hugo creates an empty site by
#'   default, but this function adds sample content by default.
#' @param theme A Hugo theme on Github (a character string of the form
#'   \code{user/repo}, and you can optionally specify a GIT branch or tag name
#'   after \code{@@}, i.e. \code{theme} can be of the form
#'   \code{user/repo@@branch}). You can also specify a full URL to the zip file
#'   or tarball of the theme. If \code{theme = NA}, no themes will be installed,
#'   and you have to manually install a theme.
#' @param hostname Where to find the theme. Defaults to \code{github.com};
#'   specify if you wish to use an instance of GitHub Enterprise. You can also
#'   specify the full URL of the zip file or tarball in \code{theme}, in which
#'   case this argument is ignored.
#' @param theme_example Whether to copy the example in the \file{exampleSite}
#'   directory if it exists in the theme. Not all themes provide example sites.
#' @param empty_dirs Whether to keep the empty directories generated by Hugo.
#' @param to_yaml Whether to convert the metadata of all posts to YAML.
#' @param netlify Whether to create a Netlify config file \file{netlify.toml}.
#' @param .Rprofile Whether to create a \file{.Rprofile} file. If \code{TRUE}, a
#'   sample \file{.Rprofile} will be created. It contains some global options,
#'   such as \code{options(blogdown.hugo.version)}, which makes sure you will
#'   use a specific version of Hugo for this site in the future.
#' @param serve Whether to start a local server to serve the site. By default,
#'   this function will ask you in an interactive R session if you want to serve
#'   the site.
#' @references The full list of Hugo commands: \url{https://gohugo.io/commands},
#'   and themes: \url{https://themes.gohugo.io}.
#' @export
#' @describeIn hugo_cmd Create a new site (skeleton) via \command{hugo new
#'   site}. The directory of the new site should be empty,
#' @examples
#' if (interactive()) blogdown::new_site()
new_site = function(
  dir = '.', force = NA, install_hugo = TRUE, format = 'yaml', sample = TRUE,
  theme = 'yihui/hugo-lithium', hostname = 'github.com', theme_example = TRUE,
  empty_dirs = FALSE, to_yaml = TRUE, netlify = TRUE, .Rprofile = TRUE,
  serve = if (interactive()) 'ask' else FALSE
) {
  msg_init('Creating your new site')
  if (is.na(force)) {
    files = grep(
      '([.]Rproj|/(LICENSE|README)([.][a-z]+)?)$', list_files(dir),
      invert = TRUE, value = TRUE
    )
    force = length(files) == 0
    if (!force) {
      force = yes_no(sprintf("The directory '%s' is not empty. Create the site anyway?", dir))
      if (!force) stop(
        'The dir is not empty and Hugo might override existing files. If you are ',
        'sure the site can be created in this dir, use new_site(force = TRUE).'
      )
    }
  }
  if (install_hugo && !hugo_available()) {
    msg_next('Installing Hugo'); install_hugo()
  }
  if (is.logical(format)) format = if (format) 'yaml' else 'toml'
  if (hugo_cmd(
    c('new', 'site', shQuote(path.expand(dir)), if (force) '--force', '-f', format),
    stdout = FALSE
  ) != 0) return(invisible())

  owd = setwd(dir); opt = opts$get(); opts$restore()
  on.exit({opts$restore(opt); setwd(owd)}, add = TRUE)

  # remove Hugo's default archetype (I think draft: true is a confusing default)
  unlink(file.path('archetypes', 'default.md'))
  # remove empty dirs
  if (!empty_dirs) for (d in list.dirs(recursive = FALSE)) del_empty_dir(d)
  if (is.character(theme) && length(theme) == 1 && !is.na(theme)) {
    msg_next('Installing the theme ', theme, ' from ', hostname)
    install_theme(theme, theme_example, hostname = hostname)
  }
  # remove the .gitignore that ignores everything under static/:
  # https://github.com/rstudio/blogdown/issues/320
  if (file.exists(gitignore <- file.path('static', '.gitignore'))) {
    if (any(xfun::read_utf8(gitignore) == '*')) unlink(gitignore)
  }

  if (sample) {
    lang = get_lang()
    d = file.path('content', c('blog', 'posts', 'post'))
    d = c(file.path('content', lang, basename(d)), d)
    for (i in d) if (dir_exists(i)) break
    d = i
    f1 = pkg_file('resources', '2020-12-01-r-rmarkdown.Rmd')
    if (use_bundle()) d = file.path(d, basename(xfun::sans_ext(f1)))
    f2 = file.path(d, if (use_bundle()) 'index.Rmd' else basename(f1))
    # for a multilingual site, create the sample post via new_content() because
    # the post may need to be under a language dir (#537)
    if (length(lang)) {
      f2 = sub(sprintf('^content/(%s/)?', lang), '', f2)
      f2 = sub('^(.+[.])', sprintf('\\1%s.', lang), f2)
      f2 = new_content(f2, open = FALSE)
      file.remove(f2)
    } else {
      dir_create(d)
    }
    msg_next('Adding the sample post to ', f2)
    file.copy(f1, f2)
    if (getOption('blogdown.open_sample', TRUE)) open_file(f2)
  }
  if (!file.exists('index.Rmd')) {
    writeLines(c('---', 'site: blogdown:::blogdown_site', '---'), 'index.Rmd')
    Sys.chmod('index.Rmd', '444')
  }

  if (to_yaml) {
    msg_next('Converting all metadata to the YAML format')
    hugo_convert('YAML', unsafe = TRUE)
  }
  if (format == 'yaml' && file.exists(cfg <- 'config.toml')) {
    toml2yaml(cfg, 'config.yaml'); unlink(cfg)
  }
  if (format == 'toml' && file.exists(cfg <- 'config.yaml')) {
    yaml2toml(cfg, 'config.toml'); unlink(cfg)
  }
  if (netlify) {
    msg_next('Adding netlify.toml in case you want to deploy the site to Netlify')
    if (!file.exists('netlify.toml')) config_netlify('netlify.toml') else {
      msg_todo(
        "The file 'netlify.toml' exists, and I will not overwrite it. If you want ",
        "to overwrite it, you may call blogdown::config_netlify() by yourself."
      )
    }
  }
  if (.Rprofile) {
    msg_next('Adding .Rprofile to set options() for blogdown')
    config_Rprofile()
  }
  dir_create('R')
  add_build_script = function(x, f) {
    write_utf8(c(
      sprintf('# An optional custom script to run %s Hugo builds your site.', x),
      '# You can delete it if you do not need it.'
    ), f)
  }
  add_build_script('before', 'R/build.R')
  add_build_script('after', 'R/build2.R')
  msg_init('The new site is ready')
  msg_okay(
    'To start a local preview: use blogdown::serve_site()',
    if (is_rstudio()) ', or the RStudio add-in "Serve Site"'
  )
  msg_okay('To stop a local preview: use blogdown::stop_server(), or restart your R session')
  if (identical(serve, 'ask')) serve = yes_no('Want to serve and preview the site now?')
  if (serve) serve_site()
  if (length(list.files('.', '[.]Rproj$')) == 0) {
    xfun::try_silent(rstudioapi::initializeProject())
  }
  invisible(getwd())
}

#' Install a Hugo theme from Github
#'
#' Download the specified theme from Github and install to the \file{themes}
#' directory. Available themes are listed at \url{https://themes.gohugo.io}.
#' @inheritParams new_site
#' @param force Whether to override the existing theme of the same name. If you
#'   have made changes to this existing theme, your changes will be lost when
#'   \code{force = TRUE}! Please consider backing up the theme by renaming it
#'   before you try \code{force = TRUE}.
#' @param update_config Whether to update the \code{theme} option in the site
#'   configurations.
#' @param update_hugo Whether to automatically update Hugo if the theme requires
#'   a higher version of Hugo than the existing version in your system.
#' @export
install_theme = function(
  theme, hostname = 'github.com', theme_example = FALSE, update_config = TRUE,
  force = FALSE, update_hugo = TRUE
) {
  theme = trim_ws(theme)
  r = '^([^/]+/[^/@]+)(@.+)?$'
  theme_is_url = grepl('[.](zip|tar[.]gz)$', theme)
  if (!is.character(theme) || length(theme) != 1 || (!grepl(r, theme) & !theme_is_url)) {
    warning(
      "'theme' must be a character string of the form 'user/repo' or ",
      "'user/repo@branch', or a full URL to the .zip or .tar.gz file"
    )
    return(invisible())
  }
  if (theme_is_url) {
    branch = xfun::sans_ext(basename(theme))
  } else {
    theme = gsub('\\s*/\\s*', '/', theme)  # remove spaces, e.g., user / repo -> user/repo
    branch = sub('^@', '', gsub(r, '\\2', theme))
    theme = gsub(r, '\\1', theme)
    # the hugo-academic theme has moved
    if (theme == 'gcushen/hugo-academic') theme = 'wowchemy/starter-academic'
    if (branch == '') branch = default_branch(theme, hostname)
  }

  dir_create('themes')
  is_theme = FALSE
  in_dir('themes', {
    url = if (theme_is_url) theme else {
      sprintf('https://%s/%s/archive/%s.tar.gz', hostname, theme, branch)
    }
    zipfile = wd_tempfile(basename(url))
    xfun::download_file(url, zipfile, mode = 'wb')
    tmpdir = wd_tempfile()
    on.exit(in_dir('themes', unlink(tmpdir, recursive = TRUE)), add = TRUE)
    if (grepl('[.]zip$', zipfile)) {
      files = utils::unzip(zipfile, exdir = tmpdir)
    } else {
      utils::untar(zipfile, exdir = tmpdir)
      files = list_files(tmpdir, all.files = TRUE)
    }
    zipdir = dirname(files)
    zipdir = zipdir[which.min(nchar(zipdir))]
    expdir = file.path(zipdir, 'exampleSite')
    is_theme = file.exists(theme_cfg <- file.path(zipdir, 'theme.toml'))
    if (dir_exists(expdir)) if (theme_example) {
      # post-process go.mod so that users don't need to install Go (it sounds
      # unbelievable that a user needs to install Go just to use a Hugo theme)
      download_modules(file.path(expdir, 'go.mod'))
      # delete figure shortcode that uses http resources on Windows:
      # https://github.com/rstudio/blogdown/issues/546#issuecomment-788253660
      if (is_windows()) xfun::gsub_dir(
        '\\{\\{< figure src="https?://.+ >}}', '', dir = expdir, ext = 'md'
      )
      file.copy(list.files(expdir, full.names = TRUE), '../', recursive = TRUE)
    } else warning(
      "The theme has provided an example site. You should read the theme's documentation ",
      'and at least take a look at the config file config.toml (or .yaml) of the example site, ',
      'because not all Hugo themes work with any config files.', call. = FALSE
    )
    # delete the images dir that contains thumbnail and screenshot of the theme
    # (because they are only useful to themes.gohugo.io and not to users)
    if (dir_exists(thndir <- file.path(zipdir, 'images'))) {
      unlink(file.path(thndir, c('tn.png', 'screenshot.png')))
      del_empty_dir(thndir)
    }
    # delete the .Rprofile and .github folder if they exist, since they are unlikely to be useful
    unlink(file.path(zipdir, c('.Rprofile', '.github')), recursive = TRUE)
    # check the minimal version of Hugo required by the theme
    if (update_hugo && is_theme) {
      if (!is.null(minver <- read_toml(theme_cfg)[['min_version']])) {
        if (!hugo_available(minver)) install_hugo()
      }
    }
    newdir = sub(tmpdir, '.', zipdir, fixed = TRUE)
    newdir = gsub('-[a-f0-9]{12,40}$', '', newdir)
    newdir = gsub(sprintf('-%s$', branch), '', newdir)
    # if tmpdir and zipdir are identical, that often means the archive was
    # extracted to the root dir of tmpdir (i.e. the theme files are compressed
    # directly into an archive, instead of being placed into a folder and that
    # folder is compressed), in which case we let newdir be the theme name; one
    # example is https://stackoverflow.com/q/65702805/559676
    if (newdir == '.') {
      newdir = if (theme_is_url) branch else basename(theme)
    }
    if (!force && dir_exists(newdir)) stop(
      'The theme already exists. Try install_theme("', theme, '", force = TRUE) ',
      'after you read the help page ?blogdown::install_theme.', call. = FALSE
    )
    unlink(newdir, recursive = TRUE)
    file.rename(zipdir, newdir)
    unlink(c(zipfile, file.path(newdir, '*.Rproj')))
    theme = gsub('^[.][\\/]+', '', newdir)
    if (is_theme) {
      # we don't need the content/ directory from the theme or config/ if it
      # already exists in root dir
      unlink(
        file.path(theme, c('content', if (dir_exists('../config')) 'config')),
        recursive = TRUE
      )
    } else {
      # download modules if not a theme, and copy "theme" content to root dir
      download_modules(file.path(theme, 'go.mod'))
      file.copy(list.files(theme, full.names = TRUE), '../', recursive = TRUE)
      unlink(theme, recursive = TRUE)
    }
    in_dir('..', {
      # move the possible config/_default/config.toml to the root dir
      move_config()
      # remove the themesDir setting; it is unlikely that you need it
      change_config('themesDir', NA)
    })
  })
  if (is_theme) if (update_config) {
    change_config('theme', sprintf('"%s"', theme))
  } else message(
    "Do not forget to change the 'theme' option in '", find_config(), "' to \"", theme, '"'
  )
}

# obtain the default branch of a repo from the API
default_branch = function(repo, hostname = 'github.com') {
  fallback = function(...) {
    message(
      'Unable to obtain the default branch of the repo "', repo,
      '". Trying the branch "master", which may be inaccurate. You are recommended ',
      'to specify the branch name after the repo name, e.g., user/repo@branch.'
    )
    'master'
  }
  tryCatch({
    x = read_utf8(sprintf('https://api.%s/repos/%s', hostname, repo))
    x = xfun::grep_sub('.*?\\s*"default_branch":\\s*"([^"]+)",.*', '\\1', x)
    if (length(x) > 0) x[1] else fallback()
  }, error = fallback)
}

# download Hugo modules with R, instead of Go/GIT, so users won't need to
# install Go or GIT
download_modules = function(mod) {
  if (!file.exists(mod)) return()
  x = read_utf8(mod)
  r = '.*?\\b(github.com/([^/]+/[^/]+))/?([^[:space:]]*)\\s+(v[^-]+)-?([^[:space:]]*?-([[:xdigit:]]{12,}))?\\s*.*'
  gzs = NULL; tmps = NULL  # gz files and temp dirs
  on.exit(unlink(c(gzs, tmps), recursive = TRUE), add = TRUE)
  # x is of the form: github.com/user/repo/folder v0.0.0-2020-e58ee0ffc576;
  # elements matched by the regex above: 1. whole; 2. github.com/user/repo; 3.
  # user/repo; 4. subfolder; 5. version (tag/branch); 6. date+sha; 7. sha
  lapply(regmatches(x, regexec(r, x)), function(v) {
    if (length(v) < 7) return()
    url = sprintf('https://%s/archive/%s.tar.gz', v[2], if (v[7] == '') v[5] else v[7])
    gz = paste0(gsub('/', '-', v[3]), '-', basename(url))
    if (!file.exists(gz)) {
      gzs <<- c(gzs, gz)
      xfun::download_file(url, gz, mode = 'wb')
    }
    files = utils::untar(gz, list = TRUE)
    if (length(files) == 0) return()
    tmps <<- c(tmps, tmp <- wd_tempfile())
    utils::untar(gz, exdir = tmp)
    root = file.path(tmp, files[1])
    # see if a module contains a replace directive
    r2 = '^replace\\s+(github[.]com/[^[:space:]]+)\\s+=>\\s+(.+?)\\s*$'
    if (file_exists(f <- file.path(root, 'go.mod')) && length(grep(r2, x2 <- read_utf8(f)))) {
      lapply(regmatches(x2, regexec(r2, x2)), function(v2) {
        if (length(v2) < 3) return()
        dir_create(v2[2])
        file.copy(list.files(file.path(root, v2[3]), full.names = TRUE), v2[2], recursive = TRUE)
      })
    } else {
      if (v[4] != '') {
        root = file.path(root, v[4])
        v[2] = file.path(v[2], v[4])
      }
      dir_create(v[2])
      file.copy(list.files(root, full.names = TRUE), v[2], recursive = TRUE)
    }
  })
  unlink(with_ext(mod, c('.mod', '.sum')))
}

# themes may use config/_default/config.toml, e.g. hugo-academic; we need to
# move this config to the root dir, because blogdown assumes the config file
# is under the root dir
move_config = function() {
  f1 = config_files()
  f2 = file.path('config', '_default', f1)
  if (!any(i <- file_exists(f2))) return()
  file.rename(f2[i], f1[i])
  # delete config.yaml if config.toml exists
  if (length(f1) >= 2 && file_exists(f1[1])) unlink(f1[2])
}

#' @param path The path to the new file under the \file{content} directory.
#' @param kind The content type to create, i.e., the Hugo archetype. If the
#'   archetype is a page bundle archetype, it should end with a slash, e.g.,
#'   \code{post/}.
#' @param open Whether to open the new file after creating it. By default, it is
#'   opened in an interactive R session.
#' @export
#' @describeIn hugo_cmd Create a new (R) Markdown file via \command{hugo new}
#'   (e.g. a post or a page).
new_content = function(path, kind = '', open = interactive()) {
  if (missing(kind)) kind = default_kind(path)
  path2 = with_ext(path, '.md')
  # for a new content file to be created with a bundle archetype, its path
  # should not contain index.md but only the dir name, otherwise the archetype
  # will not be used
  if (grepl('/$', kind)) {
    path2 = dirname(path2)
    kind  = sub('/$', '', kind)
  }
  files = list_mds()
  file2 = hugo_cmd(
    c('new', shQuote(path2), if (kind != '') c('-k', kind), theme_flag()),
    stdout = TRUE
  )
  if (length(i <- grep(r <- ' created$', file2)) == 1) {
    file2 = sub(r, '', file2[i])
    if (!grepl('[.]md$', file2)) file2 = file.path(file2, 'index.md')
  } else {
    # should the above method fail to identify the newly created .md, search for
    # the new file with brute force
    files = setdiff(list_mds(), files)  # new file(s) created
    file2 = files[basename(files) == basename(path2)]
  }
  if (length(file2) != 1) stop("Failed to create the file '", path, "'.")
  hugo_convert_one(file2)
  file = with_ext(file2, file_ext(path))
  if (file != file2) file.rename(file2, file)
  open_file(file, open)
  file
}

default_kind = function(path) {
  path = normalizePath(path, '/', mustWork = FALSE)
  if (!grepl('/', path)) return('')
  atype = gsub('/.*', '.md', path)
  if (!file.exists(file.path('archetypes', atype))) return('')
  gsub('/.*', '', path)
}

# a hack to convert the metadata of a .md post to YAML/TOML/JSON, since Hugo
# cannot convert a single file: https://github.com/gohugoio/hugo/issues/3632
hugo_convert_one = function(file, to = c('YAML', 'TOML', 'JSON')) {
  if (length(x <- trim_ws(readLines(file, 1))) == 0 || all(x == '')) {
    warning('The file ', file, ' seems to be empty')
    return()
  }
  x = x[x != ''][1]
  to = match.arg(to)
  if (x == c(YAML = '---', TOML = '+++', JSON = '{')[to]) return()
  file = normalizePath(file)
  tmp = tempfile(); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  file2 = file.path('content', basename(file))
  in_dir(tmp, {
    dir.create('content'); file.copy(file, file2)
    writeLines(c('baseurl = "/"', 'builddrafts = true'), 'config.toml')
    if (hugo_convert(to, unsafe = TRUE) == 0) file.copy(file2, file, overwrite = TRUE)
  })
}

content_file = function(...) file.path(
  switch(
    generator(),
    hugo = get_config('contentDir', 'content'),
    hexo = get_config2('source_dir', 'source'),
    '.'
  ), ...
)

#' @param title The title of the post.
#' @param author The author of the post.
#' @param categories A character vector of category names.
#' @param tags A character vector of tag names.
#' @param date The date of the post.
#' @param time Whether to include the time of the day in the \code{date} field
#'   of the post. If \code{TRUE}, the \code{date} will be of the format
#'   \samp{\%Y-\%m-\%dT\%H:\%M:\%S\%z} (e.g., \samp{2001-02-03T04:05:06-0700}).
#'   Alternatively, it can take a character string to be appended to the
#'   \code{date}. It can be important and helpful to include the time in the
#'   date of a post. For example, if your website is built on a server (such as
#'   Netlify or Vercel) and your local timezone is ahead of UTC, your local date
#'   may be a \emph{future} date on the server, and Hugo will not build future
#'   posts by default (unless you use the \command{-F} flag).
#' @param file The filename of the post. By default, the filename will be
#'   automatically generated from the title by replacing non-alphanumeric
#'   characters with dashes, e.g. \code{title = 'Hello World'} may create a file
#'   \file{content/post/2016-12-28-hello-world.md}. The date of the form
#'   \code{YYYY-mm-dd} will be prepended if the filename does not start with a
#'   date.
#' @param slug The slug of the post. By default (\code{NULL}), the slug is
#'   generated from the filename by removing the date and filename extension,
#'   e.g., if \code{file = 'post/2020-07-23-hi-there.md'}, \code{slug} will be
#'   \code{hi-there}. Set \code{slug = ''} if you do not want it.
#' @param title_case A function to convert the title to title case. If
#'   \code{TRUE}, the function is \code{tools::\link[tools]{toTitleCase}()}).
#'   This argument is not limited to title case conversion. You can provide an
#'   arbitrary R function to convert the title.
#' @param subdir If specified (not \code{NULL}), the post will be generated
#'   under a subdirectory under \file{content/}. It can be a nested subdirectory
#'   like \file{post/joe/}.
#' @param ext The filename extension (e.g., \file{.md}, \file{.Rmd}, or
#'   \file{.Rmarkdown}). Ignored if \code{file} has been specified.
#' @export
#' @describeIn hugo_cmd A wrapper function to create a new post under the
#'   \file{content/post/} directory via \code{new_content()}. If your post will
#'   use R code chunks, you can set \code{ext = '.Rmd'} or the global option
#'   \code{options(blogdown.ext = '.Rmd')} in your \file{~/.Rprofile}.
#'   Similarly, you can set \code{options(blogdown.author = 'Your Name')} so
#'   that the author field is automatically filled out when creating a new post.
new_post = function(
  title, kind = '', open = interactive(), author = getOption('blogdown.author'),
  categories = NULL, tags = NULL, date = Sys.Date(), time = getOption('blogdown.time', FALSE),
  file = NULL, slug = NULL, title_case = getOption('blogdown.title_case'),
  subdir = getOption('blogdown.subdir', 'post'), ext = getOption('blogdown.ext', '.md')
) {
  if (is.null(file)) file = post_filename(title, subdir, ext, date)
  file = trim_ws(file)  # trim (accidental) white spaces
  if (missing(kind)) kind = default_kind(file)
  if (is.null(slug) && auto_slug()) slug = post_slug(file)
  slug = trim_ws(slug)
  if (generator() == 'hugo') file = new_content(file, kind, FALSE) else {
    writeLines(c('---', '', '---'), file)
  }
  if (isTRUE(title_case)) title_case = tools::toTitleCase
  if (is.function(title_case)) title = title_case(title)
  if (get_option('blogdown.warn.future', TRUE)) {
    if (isTRUE(tryCatch(date > Sys.Date(), error = function(e) FALSE))) warning(
      'The date of the post is in the future: ', date, '. See ',
      'https://github.com/rstudio/blogdown/issues/377 for consequences, ',
      'and see https://alison.rbind.io/post/2019-03-04-hugo-troubleshooting/#dates ',
      'for a workaround by distinguishing date and publishDate in YAML header. ',
      'To turn off this warning, set options(blogdown.warn.future = FALSE).'
    )
  }

  # for categories/tags, use new values if they are not empty, otherwise use old
  # values in the post if they are non-empty (respect archetypes)
  modify_field = function(val) {
    val
    function(old, yaml) {
      as.list(if (length(val) > 0) val else if (length(old) > 0) old)
    }
  }

  do.call(modify_yaml, c(list(
    file, title = title, author = author, date = format_datetime(date, time),
    slug = slug, categories = modify_field(categories), tags = modify_field(tags)
  ), if (kind == '' && !file.exists('archetypes/default.md')) list(draft = NULL)
  ))
  open_file(file, open)
  file
}

#' @param to A format to convert to.
#' @param unsafe Whether to enable unsafe operations, such as overwriting
#'   Markdown source documents. If you have backed up the website, or the
#'   website is under version control, you may try \code{unsafe = TRUE}.
#' @export
#' @describeIn hugo_cmd A wrapper function to convert source content to
#'   different formats via \command{hugo convert}.
hugo_convert = function(to = c('YAML', 'TOML', 'JSON'), unsafe = FALSE, ...) {
  to = match.arg(to)
  hugo_cmd(c('convert', paste0('to', to), if (unsafe) '--unsafe', ...), stdout = FALSE)
}

#' @param host,port The host IP address and port; see
#'   \code{servr::\link{server_config}()}.
#' @export
#' @describeIn hugo_cmd Start a Hugo server.
hugo_server = function(host, port) {
  hugo_cmd(hugo_server_args(host, port))
}

hugo_server_args = function(host, port) {
  c(
    'server', '--bind', host, '-p', port, theme_flag(), get_option('blogdown.hugo.server', c(
      '-D', '-F', if (hugo_available('0.25')) '--navigateToChanged'
    ))
  )
}

#' Helper functions to write Hugo shortcodes using the R syntax
#'
#' These functions return Hugo shortcodes with the shortcode name and arguments
#' you specify. The closing shortcode will be added only if the inner content is
#' not empty. The function \code{shortcode_html()} is essentially
#' \code{shortcode(.type = 'html')}. The function \code{shortcodes()} is a
#' vectorized version of \code{shortcode()}. The paired functions
#' \code{shortcode_open()} and \code{shortcode_close()} provide an alternative
#' method to open and close shortcodes, which allows inner content be processed
#' safely by Pandoc (e.g., citation keys in the content).
#'
#' These functions can be used in either \pkg{knitr} inline R expressions or
#' code chunks. The returned character string is wrapped in
#' \code{htmltools::\link[htmltools]{HTML}()}, so  \pkg{rmarkdown} will protect
#' it from the Pandoc conversion. You cannot simply write \code{{{< shortcode
#' >}}} in R Markdown, because Pandoc is not aware of Hugo shortcodes, and may
#' convert special characters so that Hugo can no longer recognize the
#' shortcodes (e.g. \code{<} will be converted to \code{&lt;}).
#'
#' If your document is pure Markdown, you can use the Hugo syntax to write
#' shortcodes, and there is no need to call these R functions.
#' @param .name The name of the shortcode.
#' @param ... All arguments of the shortcode (either all named, or all unnamed).
#'   The \code{...} arguments of all other functions are passed to
#'   \code{shortcode()}.
#' @param .content The inner content for the shortcode.
#' @param .type The type of the shortcode: \code{markdown} or \code{html}.
#' @return A character string wrapped in \code{htmltools::HTML()};
#'   \code{shortcode()} returns a string of the form \code{{{\% name args \%}}},
#'   and \code{shortcode_html()} returns \code{{{< name args >}}}.
#' @note Since Hugo v0.60, Hugo has switched its default Markdown rendering
#'   engine to Goldmark. One consequence is that shortcodes may fail to render.
#'   You may enable the \code{unsafe} option in the configuration file:
#'   \url{https://gohugo.io/getting-started/configuration-markup/#goldmark}.
#' @references \url{https://gohugo.io/extras/shortcodes/}
#' @export
#' @examples library(blogdown)
#'
#' shortcode('tweet', '1234567')
#' shortcodes('tweet', as.character(1:5))  # multiple tweets
#' shortcode('figure', src='/images/foo.png', alt='A nice figure')
#' shortcode('highlight', 'bash', .content = 'echo hello world;')
#'
#' shortcode_html('myshortcode', .content='My <strong>shortcode</strong>.')
#'
#' shortcode_open('figure', src='/images/foo.png')
#' # This inner text will be *processed* by Pandoc, @Smith2006
#' shortcode_close('figure')
shortcode = function(.name, ..., .content = NULL, .type = 'markdown') {
  res = shortcode_vector(.name, ..., .content = .content, .type = .type)
  res = if (res[2] == '') res[1] else paste(res, collapse = '\n')
  htmltools::HTML(res)
}

#' @export
#' @rdname shortcode
shortcode_html = function(...) {
  shortcode(..., .type = 'html')
}

#' @param .sep The separator between two shortcodes (by default, a newline).
#' @export
#' @rdname shortcode
shortcodes = function(..., .sep = '\n') {
  htmltools::HTML(paste(mapply(shortcode, ...), collapse = .sep))
}

shortcode_vector = function(.name, ..., .content = NULL, .type = 'markdown') {
  is_html = match.arg(.type, c('markdown', 'html')) == 'html'
  m = .name; x = paste(.content, collapse = '\n'); a = args_string(...)
  if (a != '') a = paste('', a)
  if (is_html) {
    s1 = sprintf('{{< %s%s >}}', m, a)
    s2 = sprintf('{{< /%s >}}', m)
  } else {
    s1 = sprintf('{{%% %s%s %%}}', m, a)
    s2 = sprintf('{{%% /%s %%}}', m)
  }
  c(s1, x, s2)
}

shortcode_tag = function(..., .index = 1) {
  htmltools::HTML(shortcode_vector(...)[.index])
}

#' @export
#' @rdname shortcode
shortcode_open <- function(...) {
  shortcode_tag(..., .index = 1)
}

#' @export
#' @rdname shortcode
shortcode_close <- function (...) {
  shortcode_tag(..., .index = 3)
}

#' Convert post files to leaf bundles
#'
#' For a post with the path \file{content/path/to/my-post.md}, it will be moved
#' to \file{content/path/to/my-post/index.md}, so it becomes the index file of a
#' leaf bundle of Hugo. This also applies to files with extensions \file{.Rmd}
#' and \file{.Rmarkdown}.
#' @param dir The root directory of the website project (should contain a
#'   \file{content/} folder).
#' @param output The output directory. If not provided, a suffix \file{-bundle}
#'   is added to the website root directory name. For example, the default
#'   output directory for the site under \file{~/Documents/test} is
#'   \file{~/Documents/test-bundle}. You can specify the output directory to be
#'   identical to the website root directory, so files will be moved within the
#'   same directory, but please remember that you will not be able to undo
#'   \code{bundle_site()}. You should modify the website in place \emph{only if
#'   you have a backup for this directory or it is under version control}.
#' @note This function only moves (R) Markdown source files. If these files use
#'   resource files under the \file{static/} folder, these resources will not be
#'   moved into the \file{content/} folder. You need to manually move them, and
#'   adjust their paths in the (R) Markdown source files accordingly.
#' @references Learn more about Hugo's leaf bundles at
#'   \url{https://gohugo.io/content-management/page-bundles/}.
#' @export
#' @examples
#' \dontrun{
#' blogdown::bundle_site('.', '../new-site/')
#' blogdown::bundle_site('.', '.')  # move files within the current working directory
#' }
bundle_site = function(dir = site_root(), output) {
  if (!dir_exists(file.path(dir, 'content'))) stop(
    "There must exist a 'content' directory under the website root directory."
  )
  dir = normalizePath(dir)
  if (missing(output)) output = file.path(
    dirname(dir), paste0(basename(dir), '-bundle')
  )
  if (!xfun::same_path(dir, output)) {
    dir_create(output)
    file.copy(
      list_files(dir, recursive = FALSE, all.files = TRUE), output,
      recursive = TRUE, overwrite = FALSE
    )
  }
  files = list_files(file.path(output, 'content'), md_pattern)
  # if .Rmd has .html output, also move .html
  files2 = with_ext(files, 'html')
  files = c(files, files2[file_exists(files2)])
  bases = xfun::sans_ext(files)
  i = !basename(bases) %in% c('index', '_index')
  files = files[i]; bases = bases[i]
  for (b in unique(bases)) dir_create(b)
  files2 = file.path(bases, paste('index', xfun::file_ext(files), sep = '.'))
  # also move *_files/ under static/ and *_cache/ under blogdown/
  f1 = paste0(sub('^(.*)?/content/', '\\1/static/', bases), '_files')
  f2 = paste0(sub('^(.*)?/content/', '\\1/blogdown/', bases), '_cache')
  f3 = unique(c(f1, f2))
  f4 = file.path(bases, gsub('.*_', 'index_', f3))
  i = dir_exists(f3)
  files = c(files, f3[i]); files2 = c(files2, f4[i])
  # rename foo.Rmd to foo/index.Rmd; foo_files/ to foo/index_files; etc.
  i = file.rename(files, files2)
  if (any(i)) {
    message(
      'Moved these files into leaf bundles:\n\n',
      paste('*', files[i], '->', files2[i], collapse = '\n')
    )
  }
}
