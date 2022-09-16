library(testit)

test_site = function(theme) {
  dir.create(d1 <- tempfile())
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  d2 = new_site(d1, theme = theme)
  xfun::normalize_path(d1) %==% xfun::normalize_path(d2)
}

assert('new_site() works', {
  (test_site('yihui/hugo-xmin'))
})
