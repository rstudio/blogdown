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
#' @importFrom xfun attr in_dir read_utf8 write_utf8 is_windows is_macos
#'   file_exists dir_exists file_ext msg_cat dir_create del_empty_dir proc_kill
#'   set_envvar exit_call existing_files
#' @examples if (interactive()) blogdown::new_site()
NULL

with_ext = function(...) xfun::with_ext(...)
fetch_yaml = function(f) bookdown:::fetch_yaml(read_utf8(f))

`%n%` = knitr:::`%n%`

blogdown_skeleton = function(path, ...) {
  opts = options(blogdown.open_sample = FALSE); on.exit(options(opts), add = TRUE)
  new_site(dir = path, ..., serve = FALSE)
}

.onLoad = function(libname, pkgname) {
  # stop all servers when the package is unloaded or R session is ended
  reg.finalizer(asNamespace(pkgname), function(e) {
    opts$set(quitting = TRUE); on.exit(opts$set(quitting = NULL), add = TRUE)
    stop_server()
  }, onexit = TRUE)

  # initialize some important global options, so RStudio could autocomplete
  # options(); I can't set them to NULL directly because options(foo = NULL)
  # would *remove* the option 'foo', so RStudio won't be able to recognize
  # option names (I have to set them to I(NA) instead); I hate this ugly hack
  if (interactive()) for (i in names(.options)) {
    if (is.null(getOption(i))) options(.options[i])
  }
}
