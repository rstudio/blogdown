local({
  options(htmltools.dir.version = FALSE)
  library(methods)
  args = commandArgs(TRUE)
  if (length(args) > 1) setwd(args[2])
  rmarkdown::render(
    args[1], 'blogdown::html_page', envir = globalenv(), quiet = TRUE, encoding = 'UTF-8'
  )
})
