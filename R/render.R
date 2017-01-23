#' Build a website
#'
#' Compile all Rmd files, build the site through Hugo, and post-process HTML
#' files generated from Rmd (e.g. fix figure paths).
#'
#' You can use \code{\link{serve_site}()} to preview your website locally, and
#' \code{build_site()} to build the site for publishing.
#' @param local Whether to build the website locally to be served via
#'   \code{\link{serve_site}()}. If \code{TRUE}, the site configurations
#'   \code{baseurl} will be set to \code{/}, and \code{relativeurls} will be set
#'   to \code{true}. If \code{FALSE}, default configurations of the website will
#'   be used.
#' @note When \code{local = TRUE}, RSS feeds (typically the files named
#'   \code{index.xml} under the \file{public} directory) will not be
#'   post-processed to save time, which means if you have Rmd posts that contain
#'   R plots, these plots will not work. Since \code{local = TRUE} is only for
#'   previewing a website locally, you may not care about RSS feeds.
#' @export
build_site = function(local = FALSE) {
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

  # copy by-products from /blogdown/ to /content/
  lib1 = by_products(files)  # /content/.../foo_(files|cache) dirs and foo.html
  lib2 = gsub('^content', 'blogdown', lib1)  # /blogdown/.../foo_(files|cache)
  for (i in seq_along(lib2)) if (dir_exists(lib2[i])) {
    file.copy(lib2[i], dirname(lib1[i]), recursive = TRUE)
  } else if (file.exists(lib2[i])) {
    file.rename(lib2[i], lib1[i])
  }

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

  # copy (new) by-products from /content/ to /blogdown/ to make the source
  # directory clean (only .Rmd stays there)
  for (i in seq_along(lib1)) if (dir_exists(lib1[i])) {
    dir_create(dirname(lib2[i]))
    file.copy(lib1[i], dirname(lib2[i]), recursive = TRUE)
  }

  hugo_build(config, local)
  in_dir(publish_dir(config), process_pages(local))

  i = file_test('-f', lib1)
  lapply(unique(dirname(lib2[i])), dir_create)
  file.rename(lib1[i], lib2[i])  # use file.rename() to preserve mtime of .html
  unlink(lib1, recursive = TRUE)

  invisible()
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

decode_paths = function(x, dcur, env) {
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
    env$files = c(env$files, path[i])
    path[i] = d1

    # process JS/CSS dependencies
    i = !i
    d3 = gsub('^(.+_files/[^/]+)/.+', '\\1', path[i])
    dir_create('rmarkdown-libs')
    file.copy2(d3, 'rmarkdown-libs/', recursive = TRUE, overwrite = TRUE)
    env$files = c(env$files, d3)
    up = paste(rep('../', length(gregexpr('/', dcur)[[1]])), collapse = '')
    path[i] = gsub('^.+_files/', paste0(up, 'rmarkdown-libs/'), path[i])

    env$dirs = c(env$dirs, unique(d0))

    paste0(gsub(r, '\\1="', p), path, '"')
  })
  x
}

decode_paths_xml = function(x) {
  r1 = '(&lt;img src=&#34;)#####[.][.]/.+?_files/figure-html/'
  if (length(grep(r1, x)) == 0) return(x)
  m = gregexpr(r1, x)
  z = regmatches(x, m)
  r2 = '^\\s*<link>(.+)</link>\\s*$'
  l = NULL
  for (i in seq_along(x)) {
    if (grepl(r2, x[i])) l = gsub(r2, '\\1', x[i])
    if (is.null(l) || length(z[[i]]) == 0) next
    z[[i]] = paste0(gsub(r1, '\\1', z[[i]]), l, 'figures/')
  }
  regmatches(x, m) = z
  x
}

process_pages = function(local = FALSE) {
  files = list.files(
    '.', if (local) '[.]html$' else '[.](ht|x)ml$', recursive = TRUE,
    full.names = TRUE
  )
  # collect a list of dependencies to be cleaned up (e.g. figure/foo.png, libs/jquery.js)
  clean = new.env(parent = emptyenv())
  clean$files = clean$dirs = NULL
  for (f in files) process_page(f, clean, local)
  unlink(unique(clean$files), recursive = TRUE)
  lapply(unique(clean$dirs), bookdown:::clean_empty_dir)
}

process_page = function(f, env, local = FALSE) {
  x = readUTF8(f)
  if (!local && grepl('[.]xml$', f)) {
    x = decode_paths_xml(x)
    return(writeUTF8(x, f))
  }
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
  x = decode_paths(x, dirname(f), env)
  writeUTF8(x, f)
}
