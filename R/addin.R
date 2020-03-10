new_post_addin = function() {
  sys.source(pkg_file('scripts', 'new_post.R'))
}

update_meta_addin = function() {
  sys.source(pkg_file('scripts', 'update_meta.R'))
}

insert_image_addin = function() {
  sys.source(pkg_file('scripts', 'insert_image.R'))
}

# use touch to update the timestamp of a file if available (not on Windows);
# otherwise modify a file, undo it, and save it
touch_file_rstudio = function() {
  ctx = rstudioapi::getSourceEditorContext()
  if (!file.exists(ctx$path)) stop('The current document has not been saved yet.')
  p = normalizePath(ctx$path); mtime = function() file.info(p)[, 'mtime']
  m = mtime()
  on.exit(if (!identical(m, m2 <- mtime())) message(
    'The modification time of "', p, '" has been updated from ', m, ' to ', m2
  ), add = TRUE)
  touch_file(p)
}

touch_file = function(path, time = Sys.time()) Sys.setFileTime(path, time)

# add > to the beginning of every paragraph, and two trailing spaces to every line
quote_poem = function(x) {
  x = paste(x, collapse = '\n')
  if (grepl('^\\s*$', x)) return(x)
  x = gsub(' *\n', '  \n', x)
  x = gsub('( *\n){2,}', '\n\n> ', x)
  paste('>', gsub(' *(\n*) *$', '\\1', x))
}

quote_poem_addin = function() {
  ctx = rstudioapi::getSourceEditorContext()
  sel = ctx$selection[[1]]
  if (sel$text == '') {
    message('Please select some text in the editor first.')
    return()
  }
  rstudioapi::modifyRange(sel$range, quote_poem(sel$text))
}
