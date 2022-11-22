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
#'
#' @param ...,number_sections,self_contained,highlight,template,pandoc_args
#'   Arguments passed to \code{bookdown::html_document2()} (note the option
#'   \code{theme} is not supported and set to \code{NULL} internally, and when
#'   \code{template = NULL}, a default template in \pkg{blogdown} will be used).
#' @param keep_md,pre_knit,post_processor Passed to
#'   \code{rmarkdown::\link{output_format}}.
#'
#' @note Do not use a custom template unless you understand how the default
#'   template actually works (see the \pkg{blogdown} book).
#'
#'   The argument \code{highlight} does not support the value \code{"textmate"},
#'   and the argument \code{template} does not support the value
#'   \code{"default"}.
#' @references See Chapter 2 of the \pkg{bookdown} book for the Markdown syntax:
#'   \url{https://bookdown.org/yihui/bookdown}. See the \pkg{blogdown} book for
#'   full details: \url{https://bookdown.org/yihui/blogdown}.
#' @export
html_page = function(
  ..., number_sections = FALSE, self_contained = FALSE, highlight = NULL,
  template = NULL, pandoc_args = c('-M', 'link-citations=true', '--preserve-tabs'),
  keep_md = FALSE, pre_knit = NULL, post_processor = NULL
) {
  if (identical(template, 'default')) stop(
    'blogdown::html_page() does not support template = "default"'
  )
  if (identical(highlight, 'textmate')) stop(
    'blogdown::html_page() does not support highlight = "textmate"'
  )
  if (is.character(pre_knit))
    pre_knit = eval(parse(text = pre_knit))
  if (is.character(post <- post_processor))
    post = eval(parse(text = post_processor))
  post_processor = function(metadata, input, output, ...) {
    if (is.function(post)) output = post(metadata, input, output, ...)
    # the output .html file contains no YAML metadata; need to prepend from .md
    if (grepl('[.]html~', output) && file_exists(f <- with_ext(output, '.knit.md~'))) {
      prepend_yaml(f, output, callback = function(s) {
        if (!getOption('blogdown.draft.output', FALSE)) return(s)
        if (length(s) < 2 || length(grep('^draft: ', s)) > 0) return(s)
        append(s, 'draft: true', 1)
      })
    }
    output
  }
  rmarkdown::output_format(
    knitr = NULL,
    pandoc = NULL,
    clean_supporting = self_contained,
    keep_md = keep_md,
    pre_knit = pre_knit,
    post_processor = post_processor,
    base_format = bookdown::html_document2(
      ..., number_sections = number_sections, theme = NULL,
      self_contained = self_contained, highlight = highlight,
      pandoc_args = pandoc_args,
      template = template %n% pkg_file('resources', 'template-minimal.html')
    )
  )
}
