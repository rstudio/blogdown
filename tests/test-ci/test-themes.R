library(testit)

test_site = function(theme) {
  dir.create(d1 <- tempfile())
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  d2 = new_site(d1, theme = theme)
  xfun::in_dir(d2, blogdown::build_site(build_rmd = 'newfile'))
  xfun::normalize_path(d1) %==% xfun::normalize_path(d2)
}

assert('new_site() and build_site() work with selected themes', {
  themes = c(
    'hugo-apero/hugo-apero',
    sprintf('wowchemy/starter-hugo-%s', c('academic', 'online-course', 'research-group')),
    sprintf('yihui/hugo-%s', c('lithium', 'prose', 'xmag', 'xmin'))
  )
  (sapply(themes, test_site))
})
