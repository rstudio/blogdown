#' Provide diagnostics for a website project
#'
#' The function \code{check_site()} runs a series of checks against a website
#' project (see \sQuote{Details}).
#' @export
check_site = function() in_root({
  msg1('Checking the website project for possible problems')
  check_config()
  check_gitignore()
  check_hugo()
  check_netlify()
  check_content()
})


#' @details \code{check_config()} checks the configuration file
#'   (\file{config.yaml} or \file{config.toml}) for settings such as
#'   \code{baseURL} and \code{ignoreFiles}.
#' @rdname check_site
#' @export
check_config = function() {
  config = load_config()
  f = find_config()
  msg1('Checking ', f)
  open_file(f)
  msg1("Checking the 'baseURL' setting")
  base = index_ci(config, 'baseurl')
  if (is_example_url(base)) msg2(
    "You should change the 'baseURL' option in '", f, "' from '", base,
    "' to your actual domain; if you do not have a domain, set 'baseURL' to '/'"
  )
  msg1('Checking the ignoreFiles setting')
  ignore = c('\\.Rmd$', '\\.Rmarkdown$', '_cache$', '\\.knit\\.md$', '\\.utf8\\.md$')
  if (is.null(s <- config[['ignoreFiles']])) msg2(
    "You are recommended to set the 'ignoreFiles' field in ", f, ' to: ',
    xfun::tojson(ignore)
  ) else if (!all(ignore %in% s)) msg2(
    "You are recommended to ignore more items in the 'ignoreFiles' field in ", f, ": ",
    gsub('^\\[|\\]$', '', xfun::tojson(I(setdiff(ignore, s))))
  )
  if ('_files$' %in% s) msg2(
    "You are recommended to remove the item '_files$' in the 'ignoreFiles' field in ", f, '.'
  )
  msg1("Checking the 'unsafe' option for goldmark (Hugo's Markdown renderer)")
  if (is.null(s <- config$markup$goldmark$renderer$unsafe) && hugo_available('0.60')) {
    h = config$markup$defaultMarkdownHandler
    if (is.null(h) || h == 'goldmark') config_goldmark(f)
  }
}

is_example_url = function(url) {
  is.character(url) && grepl(
    '^https?://(www[.])?(example.(org|com)|replace-this-with-your-hugo-site.com)/?', url
  )
}

#' @details \code{check_gitignore()} checks if necessary files are incorrectly
#'   ignored in GIT.
#' @rdname check_site
#' @export
check_gitignore = function() {
  f = '.gitignore'
  msg1('Checking ', f)
  if (!file_exists(f)) return(msg1(f, ' was not found'))
  x = read_utf8(f)
  y = c('*.html', '*.md', '*.markdown', 'static', 'config.toml', 'config.yaml')
  if (any(i <- x %in% y)) msg2(
    'These items should probably not be ignored: ', paste(x[i], collapse = ', ')
  )
}

config_goldmark = function(f, silent = FALSE) {
  x = switch(
    xfun::file_ext(f),
    yaml = '
markup:
  goldmark:
    renderer:
      unsafe: true
',
    toml = '
[markup]
  [markup.goldmark]
    [markup.goldmark.renderer]
      unsafe = true
'
  )
  if (is.null(x)) return()
  if (!silent) msg2(
    "You are recommended to set the option 'unsafe' to true for goldmark in '", f,
    "' (see https://github.com/rstudio/blogdown/issues/447 for more info). "
  )
  if (silent || yes_no("==> Do you want blogdown to automatically set it for you?")) {
    cat(x, file = f, append = TRUE)
  } else msg2(c(
    "To set the option manually, add the following settings to '", f, "':\n", x
  ))
}

#' @details \code{check_hugo()} checks possible problems with the Hugo
#'   installation and version.
#' @rdname check_site
#' @export
check_hugo = function() {
  if (generator() != 'hugo') return()
  msg1('Checking Hugo')
  if (!hugo_available()) return(msg2(
    'Hugo not found; you may install it via blogdown::install_hugo()'
  ))
  msg1('Using Hugo ', v <- hugo_version())
  if (is.null(getOption('blogdown.hugo.version'))) msg2(
    'You are recommended to set the version of Hugo via ',
    'options(blogdown.hugo.version = "VERSION") in .Rprofile, where VERSION is ',
    'the version number like ', v, '.'
  )
}

#' @details \code{check_netlify()} checks the Hugo version specification and the
#'   publish directory in the Netlify config file \file{netlify.toml}.
#'   Specifically, it will check if the local Hugo version matches the version
#'   specified in \file{netlify.toml} (in the environment variable
#'   \var{HUGO_VERSION}), and if the \var{publish} setting in
#'   \file{netlify.toml} matches the \var{publishDir} setting in Hugo's config
#'   file (if it is set).
#' @rdname check_site
#' @export
check_netlify = function() {
  msg1('Checking netlify.toml')
  if (!file.exists(f <- 'netlify.toml')) return(msg1(f, ' was not found'))
  cfg = find_config()
  open_file(f)
  x = read_toml(f)
  v = x$context$production$environment$HUGO_VERSION
  if (is.null(v)) v = x$build$environment$HUGO_VERSION

  if (is.null(v)) msg2(
    "You are recommended to specify the Hugo version in the file '", f, "'. ",
    "If you are not sure how to do it, see the help page ?blogdown::config_netlify."
  ) else if ((v2 <- hugo_version()) != v) msg2(
    'Your local Hugo version is ', v2, ' but the Hugo version specified in the ',
    "'", f, "' file is ", v, '. You are recommended to use the same version ',
    'locally and on Netlify. You may either blogdown::install_hugo("', v, '") or ',
    'set HUGO_VERSION to ', v2, ' in ', f, '.'
  )

  if (!is.null(p1 <- x$build$publish)) {
    p2 = publish_dir(tmp = FALSE, default = NULL)
    if (p3 <- is.null(p2)) p2 = 'public'
    if (!identical(p2, gsub('/$', '', p1))) msg2(
      "The 'publish' setting in '", f, "' is '", p1, "' but the 'publishDir' setting for ",
      "Hugo is '", p2, "' (", if (p3) "Hugo's default" else c("as set in ", cfg),
      '). We recommend that you set publish = "', p2, '" in ', f, '.'
    )
  }
}

#' @details \code{check_content()} checks for possible problems in the content
#'   files. It searches for posts with future dates and draft posts, and lists
#'   them if found (such posts appear in the local preview by default, but will
#'   be ignored by default when building the site). Then it checks for R
#'   Markdown posts that have not been rendered, or have output files older than
#'   the source files, and plain Markdown posts that have \file{.html} output
#'   files (which they should not have). At last it detects \file{.html} files
#'   that seem to be generated by clicking the Knit button in RStudio with
#'   \pkg{blogdown} < v0.21. Such \file{.html} files should be deleted, since
#'   the Knit button only works with \pkg{blogdown} >= v0.21.
#' @rdname check_site
#' @export
check_content = function() {
  msg1('Checking content files')
  meta = scan_yaml()
  detect = function(field, fun) names(unlist(lapply(
    meta, function(m) fun(m[[field]])
  )))
  msg1('Checking for posts with future publish dates')
  files = detect('date', function(d) tryCatch(
    if (isTRUE(as.Date(d) > Sys.Date())) TRUE, error = function(e) NULL
  ))
  if (length(files)) msg2(
    'The dates of the following posts are in the future:\n\n', indent_list(files)
  )
  msg1('Checking for draft posts')
  files = detect('draft', function(d) if (isTRUE(d)) TRUE)
  if (length(files)) msg2(
    'Found the following draft posts:\n\n', indent_list(files)
  )
  msg1('Checking for R Markdown posts that have not been rendered')
  rmds = list_rmds()
  if (length(files <- filter_newfile(rmds))) msg2(
    'You may want to render the following posts with blogdown::build_site(build_rmd = "newfile"):\n\n',
    indent_list(files)
  )
  msg1('Checking for R Markdown posts with out-of-date output files')
  files = setdiff(rmds, files)
  files = files[require_rebuild(output_file(files), files)]
  if (length(files)) msg2(
    'You may want to re-render these posts with blogdown::build_site(build_rmd = "timestamp"):\n\n',
    indent_list(files)
  )
  msg1('Checking for plain Markdown posts that have unnecessary .html output files')
  files = with_ext(list_rmds(pattern = '[.](md|markdown)$'), 'html')
  files = files[file_exists(files)]
  if (length(files)) msg2(
    'You may want to delete these .html output files:\n\n', remove_list(files)
  )
  check_garbage_html()
}

check_garbage_html = function() {
  msg1('Checking for bad .html files generated with old versions of blogdown')
  res = unlist(lapply(list_files(content_file(), '[.]html$'), function(f) {
    if (file.size(f) < 200000) return()
    x = readLines(f, n = 15)
    if (any(x == '<meta name="generator" content="pandoc" />')) return(f)
  }))
  if (length(res)) msg2(
    'You may want to delete these files and rebuild their source files with blogdown::build_site(build_rmd = "newfile"):\n\n',
    remove_list(res)
  )
}
