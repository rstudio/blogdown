#' Live preview a site
#'
#' Calls \code{servr::\link{httw}()} to watch for changes in the site, rebuild
#' the site if necessary, and refresh the web page automatically.
#' @param ... Arguments passed to \code{servr::httw()} (arguments \code{dir},
#'   \code{site.dir}, \code{baseurl}, and \code{handler} have been provided,
#'   hence you cannot customize these arguments).
#' @export
serve_site = function(...) {
  dir = '.'
  if (!has_config(dir)) {
    warning('There is no config.toml or config.yaml under ', normalizePath(dir))
    dir2 = tryCatch(rprojroot::find_rstudio_root_file(), error = function(e) dir)
    if (dir2 != dir && has_config(dir2)) {
      message('Using the directory ', dir2, ' as the root directory of the website')
      dir = dir2
    }
  }
  owd = setwd(dir); on.exit(setwd(owd), add = TRUE)
  build_site(TRUE)
  pdir = publish_dir(); n = nchar(pdir)
  servr::httw(site.dir = pdir, baseurl = site_base_dir(), handler = function(...) {
    files = c(...)
    # exclude changes in the publish dir
    files = files[substr(files, 1, n) != pdir]
    # re-generate only if Rmd/md or config files or layouts were updated
    if (length(grep('^(themes|layouts|static)/|[.]([Rr]?md|toml|yaml)$', files)))
      build_site(TRUE)
  }, dir = '.', ...)
}

# figure out the base dir of the website, e.g. http://example.com/project/ ->
# project/, so that serve_site() works as a local server when the website is to
# be generated to a subdirectory of a domain (see the baseurl argument of
# servr::httw())
site_base_dir = function() {
  config = load_config()
  # baseurl is not meaningful when using relative URLs
  if (get_config('relativeurls', FALSE, config)) return('/')
  x = get_config('baseurl', '/', config)
  x = gsub('^(https?://[^/]+)?/', '', x)
  if (!grepl('^/', x)) x = paste0('/', x)
  x
}

has_config = function(dir) {
  length(grep('^config[.](toml|yaml)$', list.files(dir))) > 0
}

#' A helper function to return a dependency path name
#'
#' In most cases, \pkg{blogdown} can process images and HTML widgets
#' automatically generated from code chunks (they will be moved to the
#' \code{static/} folder by default), but it may fail to recognize dependency
#' files generated to other paths. This function returns a path that you can use
#' for your output files, so that \pkg{blogdown} knows that they should be be
#' processed, too. It is designed to be used in a \pkg{knitr} code chunk.
#' @param default Return this default value when this function is called outside
#'   of a \pkg{knitr} code chunk.
#' @return A character string of the \code{default} value (outside \pkg{knitr}),
#'   or a path consisting of the \pkg{knitr} figure path appended by the current
#'   chunk label.
#' @export
dep_path = function(default = knitr::opts_chunk$get('fig.path')) {
  opts = knitr::opts_current$get()
  if (length(opts) == 0) default else knitr::fig_path('', opts, NULL)
}

pkg_file = function(..., mustWork = TRUE) {
  system.file(..., package = 'blogdown', mustWork = mustWork)
}

# only copy files/dirs if they exist
file.copy2 = function(from, to, ...) {
  i = file.exists(from); from = from[i]
  if (length(from) == 0) return()
  if (length(to) > 1) {
    to = to[i]
    if (length(unique(to)) == 1) to = unique(to)
  }
  if (length(to) == 1) {
    file.copy(from, to, ...)
  } else mapply(file.copy, from, to, ...)
}

# make sure it is a file instead of an existing dir
file_exists = function(x) file_test('-f', x)

dir_copy = function(from, to) {
  if (dir_exists(from)) {
    dir_create(dirname(to))
    file.copy(from, dirname(to), recursive = TRUE)
  }
}

dirs_copy = function(from, to) {
  n = length(from); if (n == 0) return()
  if (length(to) != n) stop(
    'The number of source dirs must be equal to the number of target dirs'
  )
  for (i in seq_len(n)) dir_copy(from[i], to[i])
}

# when html output file does not exist, or html is older than Rmd, or the first
# line of the HTML is not --- (meaning it is not produced from build_rmds() but
# possibly from clicking the Knit button)
require_rebuild = function(html, rmd) {
  !file_exists(html) || file_test('-ot', html, rmd) || (readLines(html, n = 1) != '---')
}

is_windows = function() .Platform$OS.type == 'windows'
is_osx = function() Sys.info()[['sysname']] == 'Darwin'
is_linux = function() Sys.info()[['sysname']] == 'Linux'

# adapted from webshot:::download_no_libcurl due to the fact that
# download.file() cannot download Github release assets:
# https://stat.ethz.ch/pipermail/r-devel/2016-June/072852.html
download2 = function(url, ...) {
  download = function(method = 'auto', extra = getOption('download.file.extra')) {
    download.file(url, ..., method = method, extra = extra)
  }
  if (is_windows())
    return(tryCatch(download(method = 'wininet'), error = function(e) {
      download()  # try default method if wininet fails
    }))

  R340 = getRversion() >= '3.4.0'
  if (R340 && download() == 0) return(0L)
  # if non-Windows, check for libcurl/curl/wget/lynx, call download.file with
  # appropriate method
  res = NA
  if (Sys.which('curl') != '') {
    # curl needs to add a -L option to follow redirects
    if ((res <- download(method = 'curl', extra = '-L')) == 0) return(res)
  }
  if (Sys.which('wget') != '') {
    if ((res <- download.file(method = 'wget')) == 0) return(res)
  }
  if (Sys.which('lynx') != '') {
    if ((res <- download.file(method = 'lynx')) == 0) return(res)
  }
  if (is.na(res)) stop('no download method found (wget/curl/lynx)')

  res
}

opts = knitr:::new_defaults()

load_config = function() {
  config = opts$get('config')

  # read config only if it has been updated
  read_config = function(f, parser) {
    if (!is.null(time <- attr(config, 'config_time')) &&
        time == file.info(f)[, 'mtime']) return(config)
    config = parser(f)
    attr(config, 'config_time') = file.info(f)[, 'mtime']
    opts$set(config = config)
    config
  }

  find_config()

  if (file_exists('config.toml'))
    return(read_config('config.toml', parse_toml))

  if (file_exists('config.yaml'))
    return(read_config('config.yaml', yaml::yaml.load_file))
}

find_config = function(error = TRUE) {
  f = existing_files(c('config.toml', 'config.yaml'), first = TRUE)
  if (length(f) == 0 && error) stop(
    'Cannot find the configuration file config.toml or config.yaml of the website'
  )
  f
}

# figure out the possible root directory of the website
site_root = function() {
  while (length(find_config(FALSE)) == 0) {
    w1 = getwd(); w2 = dirname(w1)
    if (w1 == w2) stop(
      'Cannot find a website under the current working directory or upper-level directories'
    )
    setwd('..')
  }
  getwd()
}

# a simple parser that only reads top-level options unless RcppTOML is available
parse_toml = function(
  f, x = readUTF8(f), strict = requireNamespace('RcppTOML', quietly = TRUE)
) {
  if (strict) {
    if (no_file <- missing(f)) f = paste(x, collapse = '\n')
    return(RcppTOML::parseTOML(f, fromFile = !no_file))
  }
  z = list()
  # strings
  r = '^([[:alnum:]]+?)\\s*=\\s*"([^"]*?)"\\s*$'
  y = grep(r, x, value = TRUE)
  z[gsub(r, '\\1', y)] = as.list(gsub(r, '\\2', y))
  # boolean
  r = '^([[:alnum:]]+?)\\s*=\\s*(true|false)\\s*$'
  y = grep(r, x, value = TRUE)
  z[gsub(r, '\\1', y)] = as.list(as.logical(gsub(r, '\\2', y)))
  # numbers
  r = '^([[:alnum:]]+?)\\s*=\\s*([0-9.]+)\\s*$'
  y = grep(r, x, value = TRUE)
  z[gsub(r, '\\1', y)] = lapply(as.list(as.numeric(gsub(r, '\\2', y))), function(v) {
    v2 = as.integer(v)
    if (isTRUE(v2 == v)) v2 else v
  })
  z
}

# option names may be case insensitive
get_config = function(field, default, config = load_config()) {
  config[[field]] %n% config[[match(tolower(field), tolower(names(config)))]] %n% default
}

publish_dir = function(config = load_config()) {
  publish_dir_tmp() %n% get_config('publishDir', 'public', config)
}

# only a temporary workaround for the RStudio IDE issue: when a large number of
# files are changed, the IDE will not be responsive for quite a few seconds
publish_dir_tmp = function() {
  d = getOption('blogdown.publishDir')
  if (is.null(d)) return()
  if (is.function(d)) d = d(getwd())
  if (is.character(d)) d
}

# use RStudio to open the file if possible
open_file = function(x) {
  tryCatch(rstudioapi::navigateToFile(x), error = function(e) file.edit(x))
}

dash_filename = function(string, pattern = '[^[:alnum:]]+') {
  tolower(gsub('^-+|-+$', '', gsub(pattern, '-', string)))
}

post_filename = function(title, subdir, rmd, date) {
  file = paste0(dash_filename(title), ifelse(rmd, '.Rmd', '.md'))
  d = dirname(file); f = basename(file)
  if (is.null(subdir) || subdir == '') subdir = '.'
  d = if (d == '.') subdir else file.path(subdir, d)
  d = gsub('/+$', '', d)
  if (length(date) == 0 || is.na(date)) date = ''
  date = format(date)
  # FIXME: this \\d{4} will be problematic in about 8000 years
  if (date != '' && !grepl('^\\d{4}-\\d{2}-\\d{2}-', f)) f = paste(date, f, sep = '-')
  gsub('^([.]/)+', '', file.path(d, f))
}

# give a filename, return a slug by removing the date and extension
post_slug = function(x) {
  trim_ws(gsub('^\\d{4}-\\d{2}-\\d{2}-|[.][[:alnum:]]+$', '', basename(x)))
}

trim_ws = function(x) gsub('^\\s+|\\s+$', '', x)

run_script = function(script, ...) {
  if (file_exists(script) && Rscript(c(shQuote(script), ...)) != 0)
    stop('Failed to run ', script)
}

expand_grid = function(...) {
  expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

by_products = function(x, suffix = c('_files', '_cache', '.html')) {
  sx = knitr:::sans_ext(x)
  if (length(suffix) == 1) return(paste0(sx, suffix))
  ma = expand_grid(suffix, sx)
  if (nrow(ma) > 0) paste0(ma[, 2], ma[, 1])
}

new_post_addin = function() {
  sys.source(pkg_file('scripts', 'new_post.R'))
}

update_meta_addin = function() {
  sys.source(pkg_file('scripts', 'update_meta.R'))
}

scan_meta = function(fields = c('categories', 'tags'), dir = 'content') {
  res = list()
  files = list.files(dir, '[.][Rr]?md$', recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(res)
  meta = lapply(files, function(f) {
    yaml = fetch_yaml(f)
    if (length(yaml) == 0) return()
    yaml = yaml[-c(1, length(yaml))]
    if (length(yaml) == 0) return()
    tryCatch(yaml::yaml.load(paste(yaml, collapse = '\n')), error = function(e) {
      warning("Cannot parse the YAML metadata in '", f, "'")
      NULL
    })
  })
  for (i in fields) {
    res[[i]] = sort2(unique(unlist(lapply(meta, `[[`, i))))
  }
  res
}

# split Markdown to YAML and body (adapted from xaringan)
split_yaml_body = function(x) {
  i = grep('^---\\s*$', x)
  n = length(x)
  if (n < 2 || length(i) < 2 || (i[1] > 1 && !knitr:::is_blank(x[seq(i[1] - 1)]))) {
    list(yaml = character(), body = x)
  } else list(
    yaml = x[i[1]:i[2]], yaml_range = i[1:2],
    body = if (i[2] == n) character() else x[(i[2] + 1):n]
  )
}

fetch_yaml2 = function(f) {
  yaml = fetch_yaml(f)
  n = length(yaml)
  if (n < 2) return()
  if (n == 2 || length(grep(knitr::all_patterns$md$inline.code, yaml)) == 0)
    return(yaml)
  res = local({
    knitr::knit(text = yaml[-c(1, n)], quiet = TRUE)
  })
  c('---', res, '---')
}

as.yaml = function(..., .trim_ws = TRUE) {
  res = yaml::as.yaml(..., indent.mapping.sequence = TRUE)
  if (.trim_ws) sub('\\s+$', '', res) else res
}

append_yaml = function(x, value = list()) {
  if (length(value) == 0) return(x)
  value = as.yaml(value)
  res = split_yaml_body(x)
  if (length(res$yaml) == 0) return(x)
  append(x, value, res$yaml_range[2] - 1)
}

# filter out empty elements in a list
filter_list = function(x) {
  for (i in names(x)) {
    if (length(x[[i]]) == 0 || identical(x[[i]], '')) x[[i]] = NULL
  }
  x
}

# prevent sort(NULL), which will trigger a warning "is.na() applied to non-(list
# or vector) of type 'NULL'"
sort2 = function(x, ...) {
  if (length(x) == 0) x else sort(x, ...)
}

# on Windows, try system2(), system(), and shell() in turn, and see which
# succeeds, then remember it (https://github.com/rstudio/blogdown/issues/82)
if (is_windows()) system2 = function(command, args = character(), stdout = '', ...) {
  cmd = paste(c(shQuote(command), args), collapse = ' ')
  intern = isTRUE(stdout)
  shell2 = function() shell(cmd, mustWork = TRUE, intern = intern)

  i = getOption('blogdown.windows.shell', 'system2')
  if (i == 'shell') return(shell2())
  if (i == 'system') return(system(cmd, intern = intern))

  if (intern) return(
    tryCatch(base::system2(command, args, stdout = stdout, ...), error = function(e) {
      tryCatch({
        system(cmd, intern = intern)
        options(blogdown.windows.shell = 'system')
      }, error = function(e) {
        shell2()
        options(blogdown.windows.shell = 'shell')
      })
    })
  )

  if ((res <- base::system2(command, args, ...)) == 0) return(invisible(res))

  if ((res <- system(cmd)) == 0) {
    options(blogdown.windows.shell = 'system')
  } else if ((res <- shell2()) == 0) {
    options(blogdown.windows.shell = 'shell')
  }
  invisible(res)
}

# replace random HTML widgets IDs with incremental numbers
clean_widget_html = function(x) {
  r = '(?<=id="htmlwidget-)[a-z0-9]{10,}(?=")'
  m = gregexpr(r, x, perl = TRUE)
  id = unique(unlist(regmatches(x, m)))
  for (i in seq_along(id)) {
    r = sprintf(' (id|data-for)(="htmlwidget-)%s(")', id[i])
    x = gsub(r, sprintf(' \\1\\2%d\\3', i), x)
  }
  x
}

decode_uri = function(...) httpuv::decodeURIComponent(...)
encode_uri = function(...) httpuv::encodeURIComponent(...)
