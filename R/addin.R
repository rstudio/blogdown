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
touch_file = function() {
  ctx = rstudioapi::getSourceEditorContext()
  if (!file.exists(ctx$path)) stop('The current document has not been saved yet.')
  p = normalizePath(ctx$path); mtime = function() file.info(p)[, 'mtime']
  m = mtime(); check_mtime = function() {
    if (u <- !identical(m, m2 <- mtime())) message(
      'The modification time of "', p, '" has been updated from ', m, ' to ', m2
    )
    u
  }
  if (Sys.which('touch') != '') {
    if (system2('touch', shQuote(p)) == 0 && check_mtime()) return(TRUE)
  }
  id = ctx$id
  # add a space and delete it, then (re)save the document to update its mtime
  rstudioapi::insertText(c(1, 1), ' ', id = id)
  rstudioapi::modifyRange(list(c(1, 1, 1, 2)), '', id)
  rstudioapi::documentSave(id)
  check_mtime()
}
