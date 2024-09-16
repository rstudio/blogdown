library(testit)

test_site = function(theme) {
  o = options(blogdown.hugo.args = c('--panicOnWarning', '--quiet'))
  on.exit(options(o), add = TRUE)
  dir.create(d1 <- tempfile())
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  d2 = new_site(d1, theme = theme, serve = FALSE)
  xfun::in_dir(d2, blogdown::build_site(build_rmd = 'newfile'))
  (xfun::normalize_path(d1) %==% xfun::normalize_path(d2) &&
    xfun::in_dir(d2, blogdown::hugo_build(args = '--panicOnWarning')) == 0)
}

assert('new_site() and build_site() work with selected themes', {
  themes = c(
    'hugo-apero/hugo-apero',
    sprintf('HugoBlox/theme-%s', c('online-course', 'research-group')),
    sprintf('yihui/hugo-%s', c('lithium', 'prose', 'xmag', 'xmin', 'ivy', 'paged'))
  )
  status = !sapply(themes, test_site)
  if (any(status)) stop(
    'Theme(s) failed: ', paste(themes[status], collapse = ' '), call. = FALSE
  )
})
