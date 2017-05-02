#' @param ... Arguments to be passed to \code{system2('hugo', ...)}, e.g.
#'   \code{new_content(path)} is basically \code{hugo_cmd(c('new', path))} (i.e.
#'   run the command \command{hugo new path}).
#' @export
#' @describeIn hugo_cmd Run an arbitrary Hugo command.
hugo_cmd = function(...) {
  system2(find_hugo(), ...)
}

#' @export
#' @describeIn hugo_cmd Return the version number of Hugo if possible, which is
#'   extracted from the output of \code{hugo_cmd('version')}.
hugo_version = function() {
  x = hugo_cmd('version', stdout = TRUE)
  r = '^.* v([0-9.]{2,}) .*$'
  if (grepl(r, x)) return(as.numeric_version(gsub(r, '\\1', x)))
  warning('Cannot extract the version number from Hugo:')
  cat(x, sep = '\n')
}

#' @param local Whether to build the site for local preview (if \code{TRUE}, all
#'   drafts and future posts will also be built, and the site configuration
#'   \code{baseurl} will be set to \code{/} temporarily).
#' @param config A list of the site configurations (by default, read from
#'   \file{config.toml} or \file{config.yaml}).
#' @export
#' @describeIn hugo_cmd Build a plain Hugo website. Note that the function
#'   \code{\link{build_site}()} first compiles Rmd files, and then calls Hugo
#'   via \code{hugo_build()} to build the site.
hugo_build = function(local = FALSE, config = load_config()) {
  if (FALSE) {
    oconf = change_config('relativeurls', 'true')
    on.exit(writeUTF8(oconf$text, oconf$file), add = TRUE)
  }
  hugo_cmd(c(
    if (local) c('-b', site_base_dir(), '-D', '-F'),
    '-d', shQuote(publish_dir(config)), theme_flag(config)
  ))
}

theme_flag = function(config) {
  d = list.files(get_config('themesDir', 'themes', config))
  d = if (length(d) > 0) d[1]
  theme = get_config('theme', d, config)
  if (length(theme) == 1) c('-t', theme)
}

# in theory, we should use environment variables HUGO_FOO, but it does seem to
# really work (e.g. HUGO_RELATIVEURLS does not work), so we have to physically
# write the config into config.toml/yaml using change_config() below
reset_env = function(name, value) {
  if (is.na(value)) Sys.unsetenv(name) else Sys.setenv(name, value)
}

change_config = function(name, value) {
  f = find_config()
  x = readUTF8(f)
  if (f == 'config.toml') {
    r = sprintf('^%s\\s*=.+', name)
    v = paste(name, value, sep = ' = ')
  } else if (f == 'config.yaml') {
    r = sprintf('^%s\\s*:.+', name)
    v = paste(name, value, sep = ': ')
  }
  i = grep(r, x)
  if (length(i) > 1) stop("Duplicate configuration for '", name, "' in ", f)
  x0 = x
  if (length(i) == 1) {
    x[i] = v     # replace old config and write out
  } else {
    x = c(v, x)  # append new config and write out
  }
  writeUTF8(x, f)
  invisible(list(text = x0, file = f))
}

#' Run Hugo commands
#'
#' Wrapper functions to run Hugo commands via \code{\link{system2}('hugo',
#' ...)}.
#' @param dir The directory of the new site. It should be empty or only contain
#'   hidden files, RStudio project (\file{*.Rproj}) files, \file{LICENSE},
#'   and/or \file{README}/\file{README.md}.
#' @param install_hugo Whether to install Hugo automatically if it is not found.
#' @param format The format of the configuration file. Note that the frontmatter
#'   of the new (R) Markdown file created by \code{new_content()} always uses
#'   YAML instead of TOML.
#' @param sample Whether to add sample content. Hugo creates an empty site by
#'   default, but this function adds sample content by default).
#' @param theme A Hugo theme on Github (a chararacter string of the form
#'   \code{user/repo}, and you can optionally sepecify a GIT branch or tag name
#'   after \code{@@}, i.e. \code{theme} can be of the form
#'   \code{user/repo@@branch}).
#' @param theme_example Whether to copy the example in the \file{exampleSite}
#'   directory if it exists in the theme. Not all themes provide example sites.
#' @param serve Whether to start a local server to serve the site.
#' @references The full list of Hugo commands: \url{https://gohugo.io/commands},
#'   and themes: \url{http://themes.gohugo.io}.
#' @export
#' @describeIn hugo_cmd Create a new site (skeleton) via \command{hugo new
#'   site}. The directory of the new site should be empty,
new_site = function(
  dir = '.', install_hugo = TRUE, format = 'toml', sample = TRUE,
  theme = 'yihui/hugo-lithium-theme', theme_example = TRUE, serve = TRUE
) {
  files = grep('[.]Rproj$', list.files(dir), invert = TRUE, value = TRUE)
  files = setdiff(files, c('LICENSE', 'README', 'README.md'))
  force = length(files) == 0
  if (!force) warning("The directory '", dir, "' is not empty")
  if (install_hugo) tryCatch(find_hugo(), error = function(e) install_hugo())
  if (hugo_cmd(
    c('new site', shQuote(dir), if (force) '--force', '-f', format),
    stdout = FALSE
  ) != 0) return(invisible())

  owd = setwd(dir); on.exit(setwd(owd), add = TRUE)
  install_theme(theme, theme_example)

  if (sample) {
    dir_create(file.path('content', 'post'))
    file.copy(pkg_file('resources', '2015-07-23-r-rmarkdown.Rmd'), 'content/post/')
    if (interactive() && getOption('blogdown.open_sample', TRUE))
      open_file('content/post/2015-07-23-r-rmarkdown.Rmd')
  }
  if (!file.exists('index.Rmd'))
    writeLines(c('---', 'site: blogdown:::blogdown_site', '---'), 'index.Rmd')
  if (serve) serve_site()
}

#' Install a Hugo theme from Github
#'
#' Download the specified theme from Github and install to the \file{themes}
#' directory. Available themes are listed at \url{http://themes.gohugo.io}.
#' @inheritParams new_site
#' @param update_config Whether to update the \code{theme} option in the site
#'   configurations.
#' @export
install_theme = function(theme, theme_example = FALSE, update_config = TRUE) {
  r = '^([^/]+/[^/@]+)(@.+)?$'
  if (!is.character(theme) || length(theme) != 1 || !grepl(r, theme)) {
    warning("'theme' must be a character string of the form 'user/repo' or 'user/repo@branch'")
    return(invisible())
  }
  branch = sub('^@', '', gsub(r, '\\2', theme))
  if (branch == '') branch = 'master'
  theme = gsub(r, '\\1', theme)
  dir_create('themes')
  in_dir('themes', {
    zipfile = sprintf('%s.zip', basename(theme))
    download2(
      sprintf('https://github.com/%s/archive/%s.zip', theme, branch), zipfile, mode = 'wb'
    )
    files = utils::unzip(zipfile)
    zipdir = dirname(files[1])
    expdir = file.path(zipdir, 'exampleSite')
    if (theme_example && dir_exists(expdir)) {
      file.copy(list.files(expdir, full.names = TRUE), '../', recursive = TRUE)
    }
    file.rename(zipdir, gsub(sprintf('-%s$', branch), '', zipdir))
    unlink(zipfile)
  })
  if (update_config) return(change_config('theme', sprintf('"%s"', basename(theme))))
  message(
    "Do not forget to change the 'theme' option in '",
    find_config(), "' to \"", basename(theme), '"'
  )
}


#' @param path The path to the new file under the \file{content} directory.
#' @param kind The content type to create.
#' @param open Whether to open the new file after creating it. By default, it is
#'   opened in an interactive R session.
#' @export
#' @describeIn hugo_cmd Create a new (R) Markdown file via \command{hugo new}
#'   (e.g. a post or a page).
new_content = function(path, kind = 'default', open = interactive()) {
  hugo_cmd(c('new', shQuote(path), '-f yaml', c('-k', kind)))
  if (open) open_file(content_file(path))
}

content_file = function(path) file.path(get_config('contentDir', 'content'), path)

#' @param title The title of the post.
#' @param author The author of the post.
#' @param categories A character vector of category names.
#' @param tags A character vector of tag names.
#' @param date The date of the post.
#' @param file The filename of the post. By default, the filename will be
#'   automatically generated from the title by replacing non-alphanumeric
#'   characters with dashes, e.g. \code{title = 'Hello World'} may create a file
#'   \file{content/post/2016-12-28-hello-world.md}. The date of the form
#'   \code{YYYY-mm-dd} will be prepended if the filename does not start with a
#'   date.
#' @param slug The slug of the post.
#' @param subdir If specified (not \code{NULL}), the post will be generated
#'   under a subdirectory under \file{content/}. It can be a nested subdirectory
#'   like \file{post/joe/}.
#' @param rmd Whether to create an R Markdown (.Rmd) or plain Markdown (.md)
#'   file. Ignored if \code{file} has been specified.
#' @export
#' @describeIn hugo_cmd A wrapper function to create a new (R) Markdown post
#'   under the \file{content/post/} directory via \code{new_content()}. If your
#'   post will use R code chunks, you can set \code{rmd = TRUE} or the global
#'   option \code{options(blogdown.rmd = TRUE)} in your \file{~/.Rprofile}.
#'   Similarly, you can set \code{options(blogdown.author = 'Your Name')} so
#'   that the author field is automatically filled out when creating a new post.
new_post = function(
  title, kind = 'default', open = interactive(), author = getOption('blogdown.author'),
  categories = NULL, tags = NULL, date = Sys.Date(), file = NULL, slug = '',
  subdir = getOption('blogdown.subdir', 'post'), rmd = getOption('blogdown.rmd', FALSE)
) {
  if (is.null(file)) file = post_filename(title, subdir, rmd, date)
  file = trim_ws(file)  # trim (accidental) white spaces
  slug = trim_ws(slug)
  new_content(file, kind, FALSE)

  file = content_file(file)
  x = readUTF8(file)
  res = split_yaml_body(x)
  if ((n <- length(yml <- res$yaml)) > 2) {
    meta1 = yaml::yaml.load(paste(yml[-c(1, n)], collapse = '\n'))
    meta2 = list(
      title = title, author = author, date = format(date), slug = slug,
      categories = as.list(categories), tags = as.list(tags)
    )
    meta1 = c(meta2, meta1[setdiff(names(meta1), names(meta2))])
    if (!getOption('blogdown.yaml.empty', TRUE)) meta1 = filter_list(meta1)
    yml = as.yaml(meta1)
    writeUTF8(c('---', yml, '---', res$body), file)
  } else warning("Could not detect YAML metadata in the post '", file, "'")

  if (open) open_file(file)
}
