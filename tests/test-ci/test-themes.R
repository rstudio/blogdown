library(testit)

test_site = function(theme) {
  dir.create(d1 <- tempfile())
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  d2 = new_site(d1, theme = theme, serve = FALSE)
  xfun::in_dir(d2, blogdown::build_site(build_rmd = 'newfile'))
  (xfun::normalize_path(d1) %==% xfun::normalize_path(d2) &&
    xfun::in_dir(d2, blogdown::hugo_build()) == 0)
}

assert('new_site() and build_site() work with selected themes', {
  themes = c(
    'hugo-apero/hugo-apero',
    sprintf('wowchemy/starter-hugo-%s', c('academic', 'online-course', 'research-group')),
    sprintf('yihui/hugo-%s', c('lithium', 'prose', 'xmag', 'xmin'))
  )
  status = !sapply(themes, test_site)
  if (any(status)) stop('Theme(s) failed: ', paste(themes[status], collapse = ' '))
})
