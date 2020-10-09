#' The \pkg{blogdown} package
#'
#' The comprehensive documentation of this package is the book \bold{blogdown:
#' Creating Websites with R Markdown}
#' (\url{https://bookdown.org/yihui/blogdown/}). You are expected to read at
#' least the first chapter. If you are really busy or do not care about an
#' introduction to \pkg{blogdown} (e.g., you are very familiar with creating
#' websites), set your working directory to an empty directory, and run
#' \code{blogdown::\link{new_site}()} to get started right away.
#' @name blogdown
#' @aliases blogdown-package
#' @import utils
#' @import stats
#' @importFrom xfun attr in_dir read_utf8 write_utf8
#' @examples if (interactive()) blogdown::new_site()
NULL

with_ext = function(...) xfun::with_ext(...)
dir_exists = bookdown:::dir_exists
dir_create = bookdown:::dir_create
existing_files = bookdown:::existing_files
fetch_yaml = function(f) bookdown:::fetch_yaml(read_utf8(f))
Rscript = xfun::Rscript

`%n%` = knitr:::`%n%`

blogdown_skeleton = function(path, ...) {
  opts = options(blogdown.open_sample = FALSE); on.exit(options(opts), add = TRUE)
  new_site(dir = path, ..., serve = FALSE)
}

# stop all servers when the package is unloaded or R session is ended
.onLoad = function(libname, pkgname) {
  reg.finalizer(asNamespace(pkgname), function(e) {
    opts$set(quitting = TRUE); on.exit(opts$set(quitting = NULL), add = TRUE)
    stop_server()
  }, onexit = TRUE)
}
