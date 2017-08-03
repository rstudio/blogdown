local({
  library(methods)
  args = commandArgs(TRUE)
  rmarkdown::render(args[1], envir = globalenv(), quiet = TRUE, encoding = 'UTF-8')
})
