quiet = "--quiet" %in% commandArgs(FALSE)
formats = commandArgs(TRUE)

src = (function() {
  attr(body(sys.function()), 'srcfile')
})()$filename
if (is.null(src) || src == '') src = '.'
owd = setwd(dirname(src))

# provide default formats if necessary
if (length(formats) == 0) formats = c(
  'bookdown::pdf_book', 'bookdown::epub_book', 'bookdown::gitbook'
)
# render the book to all formats unless they are specified via command-line args
for (fmt in formats) {
  cmd = sprintf("bookdown::render_book('index.Rmd', '%s', quiet = %s)", fmt, quiet)
  res = xfun::Rscript(c('-e', shQuote(cmd)))
  if (res != 0) stop('Failed to compile the book to ', fmt)
}

# When several format are rendered, usually when make all is called,
# then we publish everything to bookdown.org
if (length(formats) > 1) {
  if (!is.na(Sys.getenv("CI", NA))) {
    xfun::pkg_load2("rsconnect")
    # On CI connect to server, using API KEY and deploy using appId
    rsconnect::addConnectServer('https://bookdown.org', 'bookdown.org')
    rsconnect::connectApiUser(
      account = 'GHA', server = 'bookdown.org',
      apiKey = Sys.getenv('CONNECT_API_KEY')
    )
    rsconnect::deploySite(
      appId = Sys.getenv('CONTENT_ID'),
      server = 'bookdown.org',
      render = 'none', logLevel = 'verbose',
      forceUpdate = TRUE)
  } else if (Sys.getenv('USER') == 'yihui') {
    # for local deployment when rsconnect/ is available
    bookdown::publish_book('blogdown', server = 'bookdown.org', render = 'none')
  }
}

setwd(owd)
