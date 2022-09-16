assert('new_site() works', {
  dir.create(d <- tempfile())
  (new_site(d, theme = 'yihui/hugo-xmin') %==% d)
})
