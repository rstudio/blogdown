#' Provide diagnostics for a website project
#'
#' The function \code{check_site()} runs a series of checks against a website
#' project (see \sQuote{Details}).
#' @export
check_site = function() in_root({
  check_init('Running a series of automated checks for your blogdown website project...')

  message(hrule())
  check_success('A successful check looks like this.')
  check_todo('A check that needs your attention looks like this.')
  check_progress("Let's check out your blogdown site!")
  message(hrule())

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
  if (is_example_url(base)) {
    check_todo('Set "baseURL" to "/" if you do not yet have a domain.')
  } else if (identical(base, '/')) {
    check_todo('Update "baseURL" to your actual URL when ready to publish.')
  } else {
    check_success('Found baseURL = "', base, '"; nothing to do here!')
  }

  check_progress('Checking "ignoreFiles" setting for Hugo...')
  ignore = c('\\.Rmd$', '\\.Rmarkdown$', '_cache$', '\\.knit\\.md$', '\\.utf8\\.md$')
  if (is.null(s <- config[['ignoreFiles']])) {
    check_todo('Set "ignoreFiles" to ', xfun::tojson(ignore))
  } else if (!all(ignore %in% s)) {
    check_todo(
      'Add these items to the "ignoreFiles" setting: ',
      gsub('^\\[|\\]$', '', xfun::tojson(I(setdiff(ignore, s))))
    )
  } else if ('_files$' %in% s) {
    check_todo('Remove "_files$" from "ignoreFiles"')
  } else {
    check_success('"ignoreFiles" looks good - nothing to do here!')
  }

  check_progress("Checking setting for Hugo's Markdown renderer...")
  if (is.null(s <- config$markup$goldmark$renderer$unsafe) && hugo_available('0.60')) {
    h = config$markup$defaultMarkdownHandler
    if (is.null(h) || h == 'goldmark') {
      check_progress("You are using the Markdown renderer 'goldmark'.")
      config_goldmark(f)
    } else if (!is.null(h)) {
      check_progress("You are using the Markdown renderer '", h, "'.")
      check_success('No todos now. If you install a new Hugo version, re-run this check.')
    }
  } else {
    check_success('All set!', if (!is.null(s)) ' Found the "unsafe" setting for goldmark.')
  }
  check_done(f)
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
  cv  = format(hugo_version(), decimal.mark='.') # current version
  av = find_hugo("all", quiet = TRUE) # save all versions installed
  nv = vapply(av, .hugo_version, as.numeric_version("0.78.2")) # numeric versions
  mv = max(as.numeric_version(nv)) # max numeric version installed

  # if no Hugo versions are installed
  if (is.null(av)) return(check_todo(
    'Hugo not found - use blogdown::install_hugo() to install.'
  ))

  # if Hugo version is available (either set in .Rprofile or default)
  if (hugo_available())
    check_success('Found Hugo! You are using Hugo ', cv, '.')

  check_progress('Checking .Rprofile for Hugo version used by blogdown...')

  # .Rprofile exists + most recent Hugo
  if (hugo_available() && !(is.null(v_set <- getOption('blogdown.hugo.version'))))
    check_success('Blogdown is using Hugo ', format(v_set, decimal.mark = '.'), ' to build site locally.')

  # If no Hugo version set in .Rprofile
  if (hugo_available() && is.null(v_set)) {
    check_progress('Hugo version not set in .Rprofile.')
    check_todo('Use blogdown::config_Rprofile() to create project .Rprofile.')
    check_todo('Set options(blogdown.hugo.version = "', cv, '")', ' in .Rprofile to use current Hugo version.')
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
  v2 = hugo_version()
  if (is.null(v)) v = x$build$environment$HUGO_VERSION

  if (is.null(v)) {
    check_progress('HUGO_VERSION not found in ', f, '.')
    check_todo('Set HUGO_VERSION = ', v2, ' in [build] context of ', f, '.')
  }

  else if (!is.null(v)) {
    check_success('Found HUGO_VERSION = ', v, ' in [build] context of ', f, '.')
  }

  check_progress('Checking that Netlify & local Hugo versions match...')
  if (v2 == v) {
    check_success("It's a match! Blogdown and Netlify are using the same Hugo version (",
                  format(v2, decimal.mark('.')),").")
  }

  else if (v2 != v) {
    check_progress('Mismatch found:\n',
                   '  blogdown is using Hugo version (', format(v2, decimal.mark('.')),
                   ') to build site locally.\n',
                   '  Netlify is using Hugo version (', v, ') to build site.')
    check_todo('Option 1: Change HUGO_VERSION = ', format(v2, decimal.mark('.')), ' in ', f, ' to match local version.')
    check_todo('Option 2: Use blogdown::install_hugo("', v, '") to match Netlify version.')
    check_todo('If Option 2: Set options(blogdown.hugo.version = "', v, '")', ' in .Rprofile to pin this Hugo version.')
  }

  check_progress('Checking that Netlify & local Hugo publish directories match...')
  if (!is.null(p1 <- x$build$publish)) {
    p2 = publish_dir(tmp = FALSE, default = NULL)
    if (p3 <- is.null(p2)) p2 = 'public'
    if (!identical(p2, gsub('/$', '', p1))) {
      check_progress('Mismatch found:\n',
                     "  The Netlify 'publish' directory in '", f, "' is '", p1, "'.\n",
                     "  The local Hugo 'publishDir' directory is '", p2,
                     "' (", if (p3) "Hugo's default" else c("as set in ", cfg),
                     ')')
      check_todo('Open ', f, ' and under [build] set publish = "', p2, '".')
    }
    else check_success('Good to go - blogdown and Netlify are using the same publish directory: ', p2)
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
  check_init('Checking content files')
  meta = scan_yaml()
  detect = function(field, fun) names(unlist(lapply(
    meta, function(m) fun(m[[field]])
  )))
  check_progress('Checking for previewed content that will not be published...')
  files = detect('date', function(d) tryCatch(
    if (isTRUE(as.Date(d) > Sys.Date())) TRUE, error = function(e) NULL
  ))
  if (length(files)) {
    check_todo('Found ', n <- length(files), ' file', if (n > 1) 's',
               ' with a future publish date:\n\n',
               indent_list(files), '\n\n',
               "  To publish today, change a file's YAML key to 'date: ",
               format(Sys.Date(), "%Y-%m-%d"), "'")
  }
  else check_success('Found 0 files with future publish dates.')
  files = detect('draft', function(d) if (isTRUE(d)) TRUE)
  if (length(files)) {
    check_todo('Found ', n <- length(files), ' file', if (n > 1) 's',
               ' marked as drafts:\n\n',
               indent_list(files), '\n\n',
               "  To un-draft, change a file's YAML from 'draft: TRUE' to 'draft: FALSE'")
  }
  else check_success('Found 0 files marked as drafts.')
  check_progress('Checking your R Markdown content...')
  rmds = list_rmds()
  if (length(files <- filter_newfile(rmds))) {
    check_todo('Found ', n <- length(files), ' R Markdown file', if (n > 1) 's',
               ' to render:\n\n',
               indent_list(files), '\n\n',
               "  To render a file, knit or use blogdown::build_site(build_rmd = 'newfile')")
  }
  else check_success('All R Markdown files have been knitted.')
  files = setdiff(rmds, files)
  files = files[require_rebuild(output_file(files), files)]
  if (length(files)) {
    check_todo('Found ', n <- length(files), ' R Markdown file', if (n > 1) 's',
               ' to update by re-rendering:\n\n',
               indent_list(files), '\n\n',
               "  To update a file, re-knit or use blogdown::build_site(build_rmd = 'timestamp')")
  }
  else check_success('All R Markdown output files are up to date with their source files.')
  check_progress('Checking for .html files to clean up...')
  files = with_ext(list_rmds(pattern = '[.](md|markdown)$'), 'html')
  files = files[file_exists(files)]
  if (length(files)) {
    check_todo('Found ', n <- length(files), ' duplicated plain Markdown and .html output file', if (n > 1) 's', ':\n\n',
               indent_list(files), '\n\n',
               "  To fix, keep each Markdown file and delete the duplicate .html output file.")
  }
  else check_success('Found 0 duplicate .html output files.')
  check_garbage_html()
  check_done('Content')
}

check_garbage_html = function() {
  res = unlist(lapply(list_files(content_file(), '[.]html$'), function(f) {
    if (file.size(f) < 200000) return()
    x = readLines(f, n = 15)
    if (any(x == '<meta name="generator" content="pandoc" />')) return(f)
  }))
  if (length(res)) {
    check_todo('Found ', n <- length(res), ' incompatible .html file', if (n > 1) 's', ' introduced by previous blogdown versions:\n\n',
               remove_list(res), '\n\n',
               "  To fix, delete each .html file and re-render with blogdown::build_site(build_rmd = 'newfile').")
  }
  else check_success('Found 0 incompatible .html files to clean up.')
}
