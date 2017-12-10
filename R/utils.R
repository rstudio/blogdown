# figure out the base dir of the website, e.g. http://example.com/project/ ->
# project/, so that serve_site() works as a local server when the website is to
# be generated to a subdirectory of a domain (see the baseurl argument of
# servr::httw())
site_base_dir = function() {
  config = load_config()
  # baseurl is not meaningful when using relative URLs
  if (get_config('relativeurls', FALSE, config)) return('/')
  x = get_config('baseurl', '/', config)
  x = gsub('^https?://[^/]+', '', x)
  if (x == '') x = '/'
  if (!grepl('^/', x)) x = paste0('/', x)
  x
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

dir_rename = function(from, to, clean = FALSE) {
  if (!dir_exists(from)) return()
  if (clean) unlink(to, recursive = TRUE)
  dir_create(dirname(to))
  # I don't know why file.rename() might fail, but if it fails, fall back to
  # file.copy(): https://github.com/rstudio/blogdown/issues/232
  suppressWarnings(file.rename(from, to)) || {
    file.copy(from, dirname(to), recursive = TRUE) && unlink(from, recursive = TRUE)
  }
}

dirs_rename = function(from, to, ...) {
  n = length(from); if (n == 0) return()
  if (length(to) != n) stop(
    'The number of source dirs must be equal to the number of target dirs'
  )
  for (i in seq_len(n)) dir_rename(from[i], to[i], ...)
}

# when html output file does not exist, or html is older than Rmd, or the first
# line of the HTML is not --- (meaning it is not produced from build_rmds() but
# possibly from clicking the Knit button)
require_rebuild = function(html, rmd) {
  older_than(html, rmd) || length(x <- readLines(html, n = 1)) == 0 || x != '---'
}

#' Build all Rmd files under a directory
#'
#' List all Rmd files recursively under a directory, and compile them using
#' \code{rmarkdown::\link{render}()}.
#' @param dir A directory path.
#' @param force Whether to force building all Rmd files. By default, an Rmd file
#'   is built only if it is newer than its output file(s).
#' @export
build_dir = function(dir = '.', force = FALSE) {
  for (f in list_rmds(dir)) {
    render_it = function() render_page(f, 'render_rmarkdown.R')
    if (force) { render_it(); next }
    files = list.files(dirname(f), full.names = TRUE)
    i = files == f  # should be only one in files matching f
    bases = with_ext(files, '')
    files = files[!i & bases == bases[i]]  # files with same basename as f (Rmd)
    if (length(files) == 0 || any(older_than(files, f))) render_it()
  }
}

older_than = function(file1, file2) {
  !file_exists(file1) | file_test('-ot', file1, file2)
}

is_windows = function() .Platform$OS.type == 'windows'
is_osx = function() Sys.info()[['sysname']] == 'Darwin'
is_linux = function() Sys.info()[['sysname']] == 'Linux'

is_rmarkdown = function(x) grepl('[.][Rr]markdown$', x)

# build .Rmarkdown to .markdown, and .Rmd to .html
output_file = function(file, md = is_rmarkdown(file)) {
  with_ext(file, ifelse(md, 'markdown', 'html'))
}

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
    if ((res <- download(method = 'wget')) == 0) return(res)
  }
  if (Sys.which('lynx') != '') {
    if ((res <- download(method = 'lynx')) == 0) return(res)
  }
  if (is.na(res)) stop('no download method found (wget/curl/lynx)')

  res
}

opts = knitr:::new_defaults()

# read config file and cache the options (i.e. do not read again unless the config is newer)
load_config = function() {
  config = opts$get('config')

  # read config only if it has been updated
  read_config = function(f, parser) {
    if (!is.null(time <- attr(config, 'config_time')) &&
        time == file.info(f)[, 'mtime']) return(config)
    config = parser(f)
    attr(config, 'config_time') = file.info(f)[, 'mtime']
    opts$set(config = config)
    check_config(config, f)
  }

  find_config()

  if (file_exists('config.toml'))
    return(read_config('config.toml', parse_toml))

  if (file_exists('config.yaml'))
    return(read_config('config.yaml', yaml_load_file))
}

check_config = function(config, f) {
  base = config[['baseurl']]
  if (is_example_url(base)) {
    open_file(f)
    warning(
      'You should change the "baseurl" option in ', f, ' from ', base,
      ' to your actual domain; if you do not have a domain, set "baseurl" to "/"',
      immediate. = TRUE, call. = FALSE
    )
  }
  if (is.null(config[['ignoreFiles']])) warning(
    'You are recommended to ignore certain files in ', f, ': set the option ignoreFiles',
    if (grepl('[.]toml$', f)) ' = ' else ': ',
    '["\\\\.Rmd$", "\\\\.Rmarkdown$", "_files$", "_cache$"]',
    immediate. = TRUE, call. = FALSE
  )
  config
}

is_example_url = function(url) {
  is.character(url) && grepl(
    '^https?://(www[.])?(example.(org|com)|replace-this-with-your-hugo-site.com)/?', url
  )
}

# only support TOML and YAML (no JSON)
find_config = function(files = c('config.toml', 'config.yaml'), error = TRUE) {
  f = existing_files(files, first = TRUE)
  if (length(f) == 0 && error) stop(
    'Cannot find the configuration file ', paste(files, collapse = ' | '), ' of the website'
  )
  f
}

# figure out the possible root directory of the website
site_root = function(...) {
  owd = getwd(); on.exit(setwd(owd), add = TRUE)
  while (length(find_config(error = FALSE, ...)) == 0) {
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
  # remove comments
  x = gsub('\\s+#.+', '', x)
  z = list()
  # arbitrary values
  r = '^([[:alnum:]]+?)\\s*=\\s*(.+)\\s*$'
  y = grep(r, x, value = TRUE)
  z[gsub(r, '\\1', y)] = as.list(gsub(r, '\\2', y))
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

# read the publishDir option in config if the temporary publish dir is not set
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

# produce a dash-separated filename by replacing non-alnum chars with -
dash_filename = function(string, pattern = '[^[:alnum:]]+') {
  tolower(gsub('^-+|-+$', '', gsub(pattern, '-', string)))
}

# return a filename for a post based on title, date, etc
post_filename = function(title, subdir, ext, date) {
  file = paste0(dash_filename(title), ext)
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

rmd_pattern = '[.][Rr](md|markdown)$'
md_pattern  = '[.][Rr]?(md|markdown)$'

# scan YAML metadata of all Rmd/md files
scan_yaml = function(dir = 'content') {
  if (missing(dir)) dir = switch(generator(),
    hugo = 'content', jekyll = '.', hexo = 'source'
  )
  files = list.files(dir, md_pattern, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(list())
  res = lapply(files, function(f) {
    yaml = fetch_yaml(f)
    if (length(yaml) == 0) return()
    yaml = yaml[-c(1, length(yaml))]
    if (length(yaml) == 0) return()
    tryCatch(yaml::yaml.load(paste(yaml, collapse = '\n')), error = function(e) {
      warning("Cannot parse the YAML metadata in '", f, "'")
      NULL
    })
  })
  setNames(res, files)
}

# collect specific fields of all YAML metadata
collect_yaml = function(
  fields = c('categories', 'tags'), dir, uniq = TRUE, sort = TRUE
) {
  res = list()
  meta = scan_yaml(dir)
  for (i in fields) {
    res[[i]] = unlist(lapply(meta, `[[`, i))
    if (sort) res[[i]] = sort2(res[[i]])
    if (uniq) res[[i]] = unique(res[[i]])
  }
  res
}

#' Find posts containing the specified metadata
#'
#' Given a YAML field name, find the (R) Markdown files that contain this field
#' and its value contains any of the specified values. Functions
#' \code{find_tags()} and \code{find_categories()} are wrappers of
#' \code{find_yaml()} with \code{field = 'tags'} and \code{field =
#' 'categories'}, respectively; \code{count_fields()} returns the frequency
#' tables of the specified YAML fields, such as the counts of tags and
#' categories.
#' @param field,fields A character vector of YAML field names.
#' @param value A vector of the field values to be matched.
#' @param open Whether to open the matched files automatically.
#' @return \code{find_yaml()} returns a character vector of filenames;
#'   \code{count_yaml()} returns a list of frequency tables.
#' @export
#' @examples library(blogdown)
#' find_tags(c('time-series', 'support vector machine'))
#' find_categories('Statistics')
#'
#' count_yaml(sort_by_count = FALSE)
find_yaml = function(field = character(), value = character(), open = FALSE) {
  if (length(field) == 0) return()
  meta = scan_yaml()
  if (length(meta) == 0) return()
  files = names(which(unlist(lapply(meta, function(m) {
    identical(value, m[[field]]) || any(value %in% m[[field]])
  }))))
  n = length(files)
  if (n == 0) return(invisible(files))
  if (open) for (f in files) open_file(f)
  files
}

#' @export
#' @rdname find_yaml
find_tags = function(value = character(), open = FALSE) {
  find_yaml('tags', value, open)
}

#' @export
#' @rdname find_yaml
find_categories = function(value = character(), open = FALSE) {
  find_yaml('categories', value, open)
}

#' @param sort_by_count Whether to sort the frequency tables by counts.
#' @export
#' @rdname find_yaml
count_yaml = function(fields = c('categories', 'tags'), sort_by_count = TRUE) {
  res = collect_yaml(fields, uniq = FALSE)
  res = lapply(res, function(x) {
    z = table(x)
    if (sort_by_count) sort(z) else z
  })
  res
}

# split Markdown to YAML and body (adapted from xaringan)
split_yaml_body = function(x) {
  i = grep('^---\\s*$', x)
  n = length(x)
  res = if (n < 2 || length(i) < 2 || (i[1] > 1 && !knitr:::is_blank(x[seq(i[1] - 1)]))) {
    list(yaml = character(), body = x)
  } else list(
    yaml = x[i[1]:i[2]], yaml_range = i[1:2],
    body = if (i[2] == n) character() else x[(i[2] + 1):n]
  )
  res$yaml_list = if ((n <- length(res$yaml)) >= 3) {
    yaml_load(res$yaml[-c(1, n)])
  }
  res
}

# anotate seq type values because both single value and list values are
# converted to vector by default
yaml_load = function(x) yaml::yaml.load(
  paste(x, collapse = '\n'),
  handlers = list(
    seq = function(x) {
      # continue coerce into vector because many places of code already assume this
      if (length(x) > 0) {
        x = unlist(x, recursive = FALSE)
        attr(x, 'yml_type') = 'seq'
      }
      x
    }
  )
)

# remove the three dashes in the YAML file before parsing it (the yaml package
# cannot handle three dashes)
yaml_load_file = function(f) {
  x = paste(readUTF8(f), collapse = '\n')
  x = gsub('^\\s*---\\s*|\\s*---\\s*$', '', x)
  yaml::yaml.load(x)
}

# if YAML contains inline code, evaluate it and return the YAML
fetch_yaml2 = function(f) {
  yaml = fetch_yaml(f)
  n = length(yaml)
  if (n < 2) return()
  if (n == 2 || !any(stringr::str_detect(yaml, knitr::all_patterns$md$inline.code)))
    return(yaml)
  res = local({
    knitr::knit(text = yaml[-c(1, n)], quiet = TRUE)
  })
  c('---', res, '---')
}

# a wrapper of yaml::as.yaml() to indent sublists by default and trim white spaces
as.yaml = function(..., .trim_ws = TRUE) {
  res = yaml::as.yaml(..., indent.mapping.sequence = TRUE)
  Encoding(res) = 'UTF-8'
  if (.trim_ws) sub('\\s+$', '', res) else res
}

# append YAML to Markdown text
append_yaml = function(x, value = list()) {
  if (length(value) == 0) return(x)
  value = as.yaml(value)
  res = split_yaml_body(x)
  if (length(res$yaml) == 0) return(x)
  append(x, value, res$yaml_range[2] - 1)
}

# modify the YAML of a file using specified new YAML options, preserve a
# particular order, and optionally remove empty fields
modify_yaml = function(
  file, ..., .order = character(), .keep_fields = NULL,
  .keep_empty = getOption('blogdown.yaml.empty', TRUE)
) {
  x = readUTF8(file)
  res = split_yaml_body(x)
  if (length(yml <- res$yaml) > 2) {
    meta0 = meta1 = res$yaml_list
    meta2 = list(...)
    for (i in names(meta2)) {
      if (is.function(f <- meta2[[i]])) meta2[i] = list(f(meta1[[i]], meta1))
    }
    meta1 = c(meta2, meta1[setdiff(names(meta1), names(meta2))])
    if (length(.keep_fields)) meta1 = meta1[.keep_fields]
    if (length(.order)) {
      i1 = intersect(.order, names(meta1))
      i2 = setdiff(names(meta1), i1)
      meta1 = meta1[c(i1, i2)]
    }
    if (!.keep_empty) meta1 = filter_list(meta1)
    if (is.null(meta1[['draft']])) meta1$draft = NULL
    for (i in names(meta1)) {
      if (identical(attr(meta0[[i]], 'yml_type'), 'seq')) {
        meta1[[i]] = as.list(meta1[[i]])
      }
    }
    yml = as.yaml(meta1)
    writeUTF8(c('---', yml, '---', res$body), file)
  } else warning("Could not detect YAML metadata in the post '", file, "'")
}

# prepend YAML of one file to another file
prepend_yaml = function(from, to, body = readUTF8(to)) {
  x = c(fetch_yaml2(from), '', body)
  writeUTF8(x, to)
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
encode_uri = function(...) {
  # httpuv <= 1.3.5 is buggy: https://github.com/rstudio/httpuv/issues/86
  if (packageVersion('httpuv') > '1.3.5') {
    httpuv::encodeURIComponent(...)
  } else {
    URLencode(...)
  }
}

# convert arguments to a single string of the form "arg1=value1 arg2=value2 ..."
args_string = function(...) {
  v = list(...)
  if (length(v) == 0) return('')
  if (any(unlist(lapply(v, length)) != 1)) stop('All argument values must be of length 1')
  m = names(v)
  i = vapply(v, is.character, logical(1))
  v = as.character(v)
  i = i | grepl('\\s', v)  # quote values that contain spaces
  i = i & !grepl('^".+"$', v)  # not already quoted
  v[i] = sprintf('"%s"', v[i])
  if (is.null(m)) {
    paste(v, collapse = ' ')
  } else {
    if (any(m == '')) stop('All arguments must be either named or unnamed')
    paste(m, '=', v, sep = '', collapse = ' ')
  }
}
