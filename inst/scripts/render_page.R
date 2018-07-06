local({
  options(htmltools.dir.version = FALSE)
  library(methods)
  args = commandArgs(TRUE)
  if (length(args) > 1) setwd(args[2])
  input = args[1]
  setwd(dirname(input))
  input = basename(input)
  to_md = blogdown:::is_rmarkdown(input)
  if (to_md) options(bookdown.output.markdown = TRUE, knitr.table.format = 'markdown')
  out = rmarkdown::render(
    input, 'blogdown::html_page', envir = globalenv(), quiet = TRUE,
    encoding = 'UTF-8', run_pandoc = !to_md, clean = !to_md
  )
  if (to_md) {
    file.rename(out, out2 <- blogdown:::output_file(input, to_md))
    unlink(xfun::attr(out, 'intermediates'))
    if (length(xfun::attr(out, 'knit_meta'))) warning(
      "Objects that have dependencies (e.g. HTML widgets) do not work when the ",
      "output format is Markdown instead of HTML."
    )
    # resolve bookdown references (figures, tables, sections, ...)
    bookdown:::process_markdown(out2, 'markdown', NULL, TRUE, to_md)
    # protect math expressions in backticks
    blogdown:::process_file(out2, xfun::protect_math)
  }
})
