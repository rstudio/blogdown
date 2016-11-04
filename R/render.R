render_pages = function() {
  config = load_config()
  files = list.files(
    'content', '[.]Rmd$', ignore.case = TRUE, recursive = TRUE, full.names = TRUE
  )
  # exclude Rmd that starts with _
  files = files[grep('^_', basename(files), invert = TRUE)]
  # do not allow special characters in filenames so dependency names are more
  # predictable, e.g. foo_files/
  bookdown:::check_special_chars(files)

  if (getOption('knitr.use.cwd', FALSE)) knitr::opts_knit$set(root.dir = getwd())

  for (f in files) in_dir(d <- dirname(f), {
    f = basename(f)
    html = with_ext(f, 'html')
    # do not recompile Rmd if html output is newer
    if (file_test('-nt', html, f)) next
    render_page(f)
    x = readUTF8(html)
    x = encode_paths(x, paste0(knitr:::sans_ext(f), '_files'), d)
    writeUTF8(c(bookdown:::fetch_yaml(readUTF8(f)), '', x), html)
  })
  hugo_build(config)
  in_dir(publish_dir(config), process_pages())
}

render_page = function(input) {
  bookdown:::Rscript(shQuote(c(pkg_file('scripts', 'render_page.R'), input)))
}

# given the content of a .html file, encode the figure paths (to be restored
# later) and move other dependencies to /public/rmarkdown-libs/
encode_paths = function(x, deps, parent) {
  if (!dir_exists(deps)) return(x)
  # find the dependencies referenced in HTML, add a marker ##### to their paths
  # (../ because future .html will be processed in_dir('public'))
  r = paste0('(<img src|<script src|<link href)(=")(', deps, '/)')
  x = gsub(r, paste0('\\1\\2#####../', parent, '/\\3'), x)
  x
}

decode_paths = function(x, dcur) {
  # dfig = file.path(deps, 'figure-html', fsep = '/')
  r = '(<img src|<script src|<link href)="#####([.][.]/[^"]+_files/[^"]+)"'
  m = gregexpr(r, x)
  regmatches(x, m) = lapply(regmatches(x, m), function(p) {
    if (length(p) == 0) return(p)
    path = gsub(r, '\\2', p)
    d0 = gsub('^(.+_files)/.+', '\\1', path)

    # process figure paths
    i = grepl('_files/figure-html/', path)
    d1 = gsub('^.+_files/figure-html/', 'figures/', path[i])
    d2 = dirname(file.path(dcur, d1))
    lapply(d2, dir_create)
    file.copy2(path[i], d2, overwrite = TRUE)
    unlink(path[i])
    path[i] = d1

    # process JS/CSS dependencies
    i = !i
    d3 = gsub('^(.+_files/[^/]+)/.+', '\\1', path[i])
    dir_create('rmarkdown-libs')
    file.copy2(d3, 'rmarkdown-libs/', recursive = TRUE, overwrite = TRUE)
    unlink(d3, recursive = TRUE)
    up = paste(rep('../', length(gregexpr('/', dcur)[[1]])), collapse = '')
    path[i] = gsub('^.+_files/', paste0(up, 'rmarkdown-libs/'), path[i])

    lapply(unique(d0), bookdown:::clean_empty_dir)

    paste0(gsub(r, '\\1="', p), path, '"')
  })
  x
}

process_pages = function() {
  files = list.files('.', '[.]html$', recursive = TRUE, full.names = TRUE)
  for (f in files) process_page(f)
}

process_page = function(f) {
  x = readUTF8(f)
  i1 = grep('<!-- BLOGDOWN-BODY-BEFORE', x)
  if (length(i1) == 0) return()
  i2 = grep('/BLOGDOWN-BODY-BEFORE -->', x)
  i3 = grep('<!-- BLOGDOWN-HEAD', x)
  i4 = grep('/BLOGDOWN-HEAD -->', x)
  i5 = (i3 + 1):(i4 - 1)
  h = paste(x[i5], collapse = '\n')
  x = x[-c(i1, i2, i3, i4, i5)]
  i6 = grep('<!-- RMARKDOWN-HEADER -->', x)
  if (length(i6) == 1) x[i6] = h else {
    i6 = grep('</head>', x)[1]
    x[i6] = paste0(h, '\n', x[i6])
  }
  x = decode_paths(x, dirname(f))
  writeUTF8(x, f)
}
