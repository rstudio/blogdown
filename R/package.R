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
NULL

with_ext = bookdown:::with_ext
readUTF8 = function(f) {
  s = file.info(f)$size
  if (is.na(s)) stop('File ', f, ' does not exist')
  if (s == 0) return(character(0))
  x = readChar(f, s, useBytes = TRUE)
  Encoding(x) = 'UTF-8'
  x = strsplit(x, '\n', fixed = TRUE)[[1]]
  gsub('\r$', '', x, perl = TRUE)
}

writeUTF8 = bookdown:::writeUTF8
dir_exists = bookdown:::dir_exists
dir_create = bookdown:::dir_create
existing_files = bookdown:::existing_files
fetch_yaml = function(f) bookdown:::fetch_yaml(readUTF8(f))
Rscript = bookdown:::Rscript

attr = knitr:::attr
in_dir = knitr:::in_dir
`%n%` = knitr:::`%n%`

blogdown_skeleton = function(path, ...) {
  opts = options(blogdown.open_sample = FALSE); on.exit(options(opts), add = TRUE)
  new_site(dir = path, ..., serve = FALSE)
}
