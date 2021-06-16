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
  template = NULL, pandoc_args = NULL, keep_md = FALSE,
  pre_knit = NULL, post_processor = NULL
) {
  if (identical(template, 'default')) stop(
    'blogdown::html_page() does not support template = "default"'
  )
  if (identical(highlight, 'textmate')) stop(
    'blogdown::html_page() does not support highlight = "textmate"'
  )
  if (is.character(pre_knit))
    pre_knit <- eval(parse(text = pre_knit))
  if (is.character(post_processor))
    post_processor <- eval(parse(text = post_processor))
  
  file_with_meta_ext <- function(file, meta_ext, ext = tools::file_ext(file)) {
    paste(tools::file_path_sans_ext(file),
          ".", meta_ext, ".", ext,
          sep = ""
    )
  }
  
  get_parent_env_with <- function(var_names) {
    for (frame in rev(sys.frames())[-1]) {
      present <- all(vapply(
        var_names, exists, logical(1),
        envir = frame, inherits = FALSE
      ))
      if (present) return(frame)
    }
    stop(
      "No parent environment found with ",
      paste(var_names, collapse = ", ")
    )
  }
  
  pre_knit <- function(input, ...) {
    render_env <- get_parent_env_with("knit_input")
    pre_knit_input <- get("knit_input", envir = render_env)
    intermediates_loc <- get("intermediates_loc", envir = render_env)
    
    rmd_text <- readChar(input, file.info(input)$size)
    rmd_text <- gsub("\r\n", "\n", rmd_text)
    rmd_text <- sub("```\n", "source(sub('content.*', 'themes/teachR/R/pre_processing_code.R', here::here()))\n```\n", rmd_text, fixed = T)
    preprocessed_rmd_file <- intermediates_loc(
      file_with_meta_ext(pre_knit_input, "preprocessed")
    )
    cat(rmd_text, file = preprocessed_rmd_file)
    assign("knit_input", preprocessed_rmd_file, envir = render_env)
  }
  on.exit(file.remove(list.files(pattern = "preprocessed\\.[Rr]md")))
  
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
      pandoc_args = c('-M', 'link-citations=true', pandoc_args),
      template = template %n% pkg_file('resources', 'template-minimal.html')
    )
  )
}
