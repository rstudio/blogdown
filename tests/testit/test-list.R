library(testit)

assert('list_rmds() ignores thing in renv / packrat folder', {

  dir = tempfile('testit-dir-')
  dir.create(dir, recursive = TRUE)

  dir.create(file.path(dir, 'renv'))
  file.create(rmd_renv <- file.path(dir, 'renv/ignore.Rmd'))

  rmd = list_rmds(dir)
  (rmd %==% character())
  rmd = list_rmds(files = rmd_renv)
  (rmd %==% character())

  unlink(dir, recursive = TRUE)

})
