#' Provide diagnostics for a website project
#'
#' The function \code{check_site()} runs a series of checks against a website
#' project (see \sQuote{Details}).
#' @export
check_site = function() in_root({
  check_init('Running checks for the blogdown website project', '\n')
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
  check_init('Checking ', f)
  open_file(f)
  check_progress('Checking "baseURL" setting for Hugo...')
  base = index_ci(config, 'baseurl')
  check_progress('Found "baseURL = ', base, '"')
  if (is_example_url(base)) check_todo(
    'Set "baseURL = /" if you do not yet have a domain.'
    )
  else if (is_slash_url(base))
    check_todo('Update "baseURL" to your actual URL when ready to publish.')
  else check_success('"baseURL" set- nothing to do here!')
  check_progress('Checking "ignoreFiles" setting for Hugo...')
  ignore = c('\\.Rmd$', '\\.Rmarkdown$', '_cache$', '\\.knit\\.md$', '\\.utf8\\.md$')
  if (is.null(s <- config[['ignoreFiles']])) check_todo(
    'Add "ignoreFiles:"', xfun::tojson(ignore))
  else if (!all(ignore %in% s) & (!'_files$' %in% s))
    check_success('Found all recommended "ignoreFiles":',
                  '\n',
                  '',
                  gsub('^\\[|\\]$', '', xfun::tojson(I(setdiff(ignore, s))))
                  )
  else if (!all(ignore %in% s) & ('_files$' %in% s))
    check_todo('Remove "_files$" from "ignoreFiles":',
               '\n',
               '',
               gsub('^\\[|\\]$', '', xfun::tojson(I(setdiff(ignore, s))))
               )
  else check_success('"ignoreFiles" looks good- nothing to do here!')
  check_progress('Checking setting for Hugo markdown renderer...')
  if (is.null(s <- config$markup$goldmark$renderer$unsafe) && hugo_available('0.60')) {
    h = config$markup$defaultMarkdownHandler
    if (is.null(h) || h == 'goldmark') {
      check_progress('You are using the goldmark markdown renderer.')
      config_goldmark(f)
    }
    else if (h == 'blackfriday')
      check_progress('You are using the ', h, ' markdown renderer.')
      check_success('No todos now. If you install a new Hugo version, re-run this check.')
  }
  else if (!is.null(s) && hugo_available('0.60'))
    check_progress('You are using the goldmark markdown renderer.')
    check_success('All set! Found "unsafe" setting - Hugo will render raw HTML.')
  check_done(f)
}

is_example_url = function(url) {
  is.character(url) && grepl(
    '^https?://(www[.])?(example.(org|com)|replace-this-with-your-hugo-site.com)/?', url
  )
}

is_slash_url = function(url) {
  is.character(url) && grepl(
    '^/$', url
  )
}

#' @details \code{check_gitignore()} checks if necessary files are incorrectly
#'   ignored in GIT.
#' @rdname check_site
#' @export
check_gitignore = function() {
  f = '.gitignore'
  check_init('Checking ', f)
  if (!file_exists(f)) return(check_todo(f, ' was not found. You may want to add this.'))
  x = read_utf8(f)
  check_progress('Checking for items to remove...')
  no_ignore = c('*.html', '*.md', '*.markdown', 'static', 'config.toml', 'config.yaml')
  if (any(i <- x %in% no_ignore)) check_todo(
    'Remove items from', f, ':', paste(x[i], collapse = ', ')
  )
  else check_success('Nothing to see here - found no items to remove.')
  check_progress('Checking for items you can safely ignore...')
  yes_ignore = c('blogdown', '.DS_Store', 'Thumbs.db')
  if (any(i <- x %in% yes_ignore))
    check_success('Found! You have safely ignored:', paste(x[i], collapse = ', '))
  ignore_missing = setdiff(yes_ignore, x[i])
  if (length(ignore_missing) >= 1)
    check_todo('You can safely add to', f, ':',
               paste(ignore_missing, collapse = ', '))
  check_progress('Checking for items to ignore if Netlify builds your site...')
  if (!file_exists('netlify.toml')) return(check_progress(f, " was not found. Use blogdown::config_netlify() to set one up."))
  netlify_ignore = c('public', 'resources')
  if (any(i <- x %in% netlify_ignore))
    check_success('Found! You have safely ignored:', paste(x[i], collapse = ', '))
  netlify_missing = setdiff(netlify_ignore, x[i])
  if (length(netlify_missing) >= 1)
    check_todo('When Netlify builds your site, you can safely add to', f, ':',
               paste(netlify_missing, collapse = ', '))
  else check_todo('Almost clear for takeoff - use blogdown::check_netlify() too.')
  check_done(f)
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
  if (!silent) check_todo(
    'Allow goldmark to render raw HTML by adding this setting to', f, ':\n', x
  )
  if (silent || yes_no("==> Do you want blogdown to set this for you?")) {
    cat(x, file = f, append = TRUE)
  }
}

#' @details \code{check_hugo()} checks possible problems with the Hugo
#'   installation and version.
#' @rdname check_site
#' @export
check_hugo = function() {
  if (generator() != 'hugo') return()
  check_init('Checking Hugo')
  check_progress('Checking Hugo version...')
  # variables
  v  = format(hugo_version(), decimal.mark='.')
  av = find_hugo("all", quiet = TRUE) # save all versions installed
  nv = vapply(av, .hugo_version, as.numeric_version("0.78.2")) # numeric versions
  mv = max(as.numeric_version(nv)) # max numeric version installed

  # if no Hugo versions are installed
  if (is.null(av)) return(check_todo(
    'Hugo not found - use blogdown::install_hugo() to install.'
  ))

  # if Hugo version is available (either set in .Rprofile or default)
  if (hugo_available())
    check_success('Found Hugo! You are using Hugo ', v, '.')

  check_progress('Checking .Rprofile for Hugo version used by blogdown...')

  # .Rprofile exists + most recent Hugo
  if (hugo_available() && !(is.null(v_set <- getOption('blogdown.hugo.version'))))
    check_success('Blogdown is using Hugo ', format(v_set, decimal.mark = '.'), ' to build site locally.')

  # If no Hugo version set in .Rprofile
  if (hugo_available() && is.null(v_set)) {
    check_progress('Hugo version not set in .Rprofile.')
    check_todo('Use blogdown::config_Rprofile() to create project .Rprofile.')
    check_todo('Set options(blogdown.hugo.version = "', v, '")', ' in .Rprofile to use current Hugo version.')
  }

  check_progress('Checking for more recently installed Hugo versions...')
  # More recent Hugo version is available than in .Rprofile
  if (hugo_available() && (v_set < mv)) {
    check_progress('Found Hugo version ', format(v_set, decimal.mark='.'), ' in .Rprofile, but version ', format(mv, decimal.mark='.'), ' is more recent.')
    check_todo('Set options(blogdown.hugo.version = "',
               format(mv, decimal.mark='.'), '")', ' in .Rprofile to use newest installed Hugo version.')
  }
  else if (hugo_available() && (v_set == mv))
    check_success('Blogdown is using the most up-to-date Hugo version installed (', format(v_set, decimal.mark = '.'), ') to build site locally.')

  check_done('Hugo')
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
  check_init('Checking netlify.toml...')
  if (!file.exists(f <- 'netlify.toml')) return(
    check_todo(f, ' was not found. Use blogdown::config_netlify() to create file.')
    )
  cfg = find_config()
  open_file(f)
  x = read_toml(f)
  v = x$context$production$environment$HUGO_VERSION
  if (is.null(v)) v = x$build$environment$HUGO_VERSION

  check_progress('Checking HUGO_VERSION setting in ', f, '...')
  if (is.null(v)) {
    check_progress('HUGO_VERSION not found in ', f, '.')
    check_todo("Set the Hugo version in ", f, ".")
  }

  else if (!is.null(v)) {
    check_success('Found HUGO_VERSION = ', v, ' in ', f, '.')
  }

  check_progress('Checking that Netlify & local Hugo versions match...')
  if ((v2 <- hugo_version()) == v) {
    check_success("It's a match! Blogdown and Netlify are using the same Hugo version (",
                  format(v2, decimal.mark('.')),").")
  }

  else if ((v2 <- hugo_version()) != v) {
    check_progress('Mismatch found:')
    check_progress('blogdown is using Hugo version (', format(v2, decimal.mark('.')),
                   ') to build site locally.')
    check_progress('Netlify is using Hugo version (', v, ') to build site.')
    check_todo('Option 1: Change HUGO_VERSION = ', format(v2, decimal.mark('.')), ' in ', f, ' to match local version.')
    check_todo('Option 2: Use blogdown::install_hugo("', v, '") to match Netlify version.')
    check_todo('If Option 2: Set options(blogdown.hugo.version = "', v, '")', ' in .Rprofile to pin this Hugo version.')
  }

  check_progress('Checking that Netlify & local Hugo publish directories match...')
  if (!is.null(p1 <- x$build$publish)) {
    p2 = publish_dir(tmp = FALSE, default = NULL)
    if (p3 <- is.null(p2)) p2 = 'public'
    if (!identical(p2, gsub('/$', '', p1))) {
      check_progress('Mismatch found:')
      check_progress("The Netlify 'publish' directory in '", f, "' is '", p1, "'.")
      check_progress("The local Hugo 'publishDir' directory is '", p2,
                     "' (", if (p3) "Hugo's default" else c("as set in ", cfg),
                     ')')
      check_todo('Open ', f, ' and under [build] set publish = "', p2, '".')
    }
    else check_success('Good to go - blogdown and Netlify are using the same publish directory (', p2, ').')
  }

  check_done(f)
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
