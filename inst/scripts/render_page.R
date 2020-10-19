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
    run_pandoc = !to_md, clean = !to_md
  )
  if (to_md) {
    file.rename(out, out2 <- blogdown:::output_file(input, to_md))
    unlink(xfun::attr(out, 'intermediates'))
    # write HTML dependencies to the body of Markdown
    if (length(meta <- xfun::attr(out, 'knit_meta'))) {
      x = xfun::read_utf8(out2)
      m = rmarkdown:::html_dependencies_as_string(meta, attr(out, 'files_dir'), '.')
      if (length(i <- grep('^---\\s*$', x)) >= 2) {
        x = append(x, m, i[2])
        xfun::write_utf8(x, out2)
      }
    }
    # resolve bookdown references (figures, tables, sections, ...)
    bookdown:::process_markdown(out2, 'markdown', NULL, TRUE, to_md)
    # protect math expressions in backticks
    xfun::process_file(out2, xfun::protect_math)
  }
})
