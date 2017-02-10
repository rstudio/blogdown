#' Live preview a site
#'
#' Calls \code{servr::\link{httw}()} to watch for changes in the site, rebuild
#' the site if necessary, and refresh the page automatically.
#' @param ... Arguments passed to \code{servr::httw()}.
#' @export
serve_site = function(...) {
  build_site(TRUE)
  pdir = publish_dir(); n = nchar(pdir)
  servr::httw(site.dir = pdir, handler = function(...) {
    files = c(...)
    # exclude changes in the publish dir
    files = files[substr(files, 1, n) != pdir]
    # re-generate only if Rmd/md or config files or layouts were updated
    if (length(grep('^(themes|layouts|static)/|[.](R?md|toml|yaml)$', files)))
      build_site(TRUE)
  }, ...)
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

is_windows = function() .Platform$OS.type == 'windows'
is_osx = function() Sys.info()[['sysname']] == 'Darwin'
is_linux = function() Sys.info()[['sysname']] == 'Linux'

# adapted from webshot:::download_no_libcurl due to the fact that
# download.file() cannot download Github release assets:
# https://stat.ethz.ch/pipermail/r-devel/2016-June/072852.html
download2 = function(url, ...) {
  if (is_windows())
    return(tryCatch(download.file(url, method = 'wininet', ...), error = function(e) {
      download.file(url, ...)  # try default method if wininet fails
    }))

  # if non-Windows, check for libcurl/curl/wget/lynx, call download.file with
  # appropriate method
  if (Sys.which('curl') != '') {
    method = 'curl'
    # curl needs to add a -L option to follow redirects
    opts = options(download.file.extra = paste('-L', getOption('download.file.extra')))
    on.exit(options(opts), add = TRUE)
  } else if (Sys.which('wget') != '') {
    method = 'wget'
  } else if (Sys.which('lynx') != '') {
    method = 'lynx'
  } else {
    stop('no download method found (wget/curl/lynx)')
  }

  download.file(url, method = method, ...)
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

  if (file.exists('config.yaml'))
    return(read_config('config.yaml', yaml::yaml.load_file))

  if (file.exists('config.toml'))
    return(read_config('config.toml', parse_toml))
}

find_config = function() {
  f = existing_files(c('config.toml', 'config.yaml'), first = TRUE)
  if (length(f) == 0) stop(
    'Cannot find the configuration file config.yaml or config.toml of the website'
  )
  f
}

# not TOML parser in R yet, so a simple version that only reads top-level options
parse_toml = function(f, x = readUTF8(f)) {
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
  z[gsub(r, '\\1', y)] = as.list(as.numeric(gsub(r, '\\2', y)))
  z
}

# option names may be case insensitive
get_config = function(field, default, config = load_config()) {
  config[[field]] %n% config[[match(tolower(field), tolower(names(config)))]] %n% default
}

publish_dir = function(config = load_config()) {
  get_config('publishDir', 'public', config)
}

# use RStudio to open the file if possible
open_file = function(x) {
  tryCatch(rstudioapi::navigateToFile(x), error = function(e) file.edit(x))
}

dash_filename = function(string) {
  tolower(gsub('^-+|-+$', '', gsub('[^[:alnum:]]+', '-', string)))
}

post_filename = function(title, subdir, rmd, date) {
  file = paste0(dash_filename(title), ifelse(rmd, '.Rmd', '.md'))
  d = dirname(file); f = basename(file)
  if (!is.null(subdir) && subdir != '') d = if (d == '.') subdir else file.path(subdir, d)
  # FIXME: this \\d{4} will be problematic in about 8000 years
  if (!grepl('^\\d{4}-\\d{2}-\\d{2}-', f)) f = paste(format(date), f, sep = '-')
  file = if (d == '.') f else file.path(d, f)
  file.path('post', file)
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
    res[[i]] = sort(unique(unlist(lapply(meta, `[[`, i))))
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

as.yaml = function(...) {
  res = yaml::as.yaml(..., indent.mapping.sequence = TRUE)
  sub('\\s+$', '', res)
}

append_yaml = function(x, value = list()) {
  if (length(value) == 0) return(x)
  value = as.yaml(value)
  res = split_yaml_body(x)
  if (length(res$yaml) == 0) return(x)
  append(x, value, res$yaml_range[2] - 1)
}
