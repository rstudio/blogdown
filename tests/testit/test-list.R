library(testit)

assert('list_rmds() ignores thing in renv / packrat folder', {

  dir = tempfile('testit-dir-')
  dir.create(dir, recursive = TRUE)

  dir.create(file.path(dir, 'renv'))
  file.create(file.path(dir, 'renv/ignore.Rmd'))

  rmd = list_rmds(dir)
  (rmd %==% character())

  unlink(dir, recursive = TRUE)

})
