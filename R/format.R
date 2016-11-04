#' An R Markdown output format for \pkg{blogdown} web pages
#'
#' This function is a simple wrapper of \code{bookdown::\link{html_document2}()}
#' with different default arguments, and more importantly, a special HTML
#' template designed only for \pkg{blogdown} to render R Markdown to HTML pages
#' that can be processed by Hugo.
#'
#' The HTML output is not a complete HTML document, and only meaningful to
#' \pkg{blogdown} (it will be post-processed to render valid HTML pages). The
#' only purpose of this output format is for users to change options in YAML.
#'
#' The fact that it is based on \pkg{bookdown} means most \pkg{bookdown}
#' features are supported, such as numbering and cross-referencing
#' figures/tables.
#' @param ...,number_sections,self_contained,highlight Arguments passed to
#'   \code{bookdown::html_document2()}.
#' @references See Chapter 2 of the \pkg{bookdown} book for the Markdown syntax:
#'   \url{https://bookdown.org/yihui/bookdown}
#' @export
html_page = function(
  ..., number_sections = FALSE, self_contained = FALSE, highlight = NULL
) bookdown::html_document2(
  ..., number_sections = number_sections, theme = NULL,
  self_contained = self_contained, highlight = highlight,
  template = pkg_file('resources', 'template-minimal.html')
)
