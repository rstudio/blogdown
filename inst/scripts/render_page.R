local({
  options(htmltools.dir.version = FALSE)
  library(methods)
  args = commandArgs(TRUE)
  rmarkdown::render(
    args[1], 'blogdown::html_page', envir = globalenv(), quiet = TRUE, encoding = 'UTF-8'
  )
})
