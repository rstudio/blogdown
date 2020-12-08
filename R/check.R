check_site = function() in_root({
  check_config()
  check_gitignore()
  check_hugo()
  check_netlify()
  check_content()
})

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

check_gitignore = function() {
  f = '.gitignore'
  msg1('Checking ', f)
  if (!file_exists(f)) return(msg1(f, ' was not found'))
  x = read_utf8(f)
  y = c('*.html', '*.md', 'static', 'config.toml', 'config.yaml')
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

check_hugo = function() {
}

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

check_content = function() {
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
    'You may want to delete these files:\n\n', paste(' ', res, collapse = '\n')
  )
}
