# figure out the base dir of the website, e.g. http://example.com/project/ ->
# project/, so that serve_site() works as a local server when the website is to
# be generated to a subdirectory of a domain (see the baseurl argument of
# servr::server_config())
site_base_dir = function() {
  config = load_config()
  # baseurl is not meaningful when using relative URLs
  if (get_config('relativeurls', FALSE, config)) return('/')
  x = get_config('baseurl', '/', config)
  x = gsub('^(https?:)?//[^/]+', '', x)
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

# tempfile under the current working directory
wd_tempfile = function(..., pattern = '') {
  basename(tempfile(pattern, '.', ...))
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

# relative path with '/' as the path separator
rel_path = function(x) {
  xfun::relative_path(xfun::normalize_path(x))
}

# change the default of full.names and recursive in list.files() because these
# values are used much more frequently than the original defaults
list_files = function(..., full.names = TRUE, recursive = TRUE) {
  list.files(..., full.names = full.names, recursive = recursive, no.. = TRUE)
}

# does html output file not exist, or is it older than Rmd for at least N seconds?
require_rebuild = function(html, rmd, N = get_option('blogdown.time_diff', 0)) {
  m1 = file.mtime(html); m2 = file.mtime(rmd)
  !file_exists(html) | difftime(m2, m1, units = 'secs') > N
}

#' Build all Rmd files under a directory
#'
#' List all Rmd files recursively under a directory, and compile them using
#' \code{rmarkdown::\link{render}()}.
#' @param dir A directory path.
#' @param force Whether to force building all Rmd files. By default, an Rmd file
#'   is built only if it is newer than its output file(s).
#' @param ignore A regular expression to match output filenames that should be
#'   ignored when testing if the modification time of the Rmd source file is
#'   newer than its output files.
#' @export
build_dir = function(dir = '.', force = FALSE, ignore = '[.]Rproj$') {
  for (f in list_rmds(dir)) {
    if (force) { render_new(f); next }
    files = list.files(dirname(f), full.names = TRUE)
    files = grep(ignore, files, value = TRUE, invert = TRUE)
    i = files == f  # should be only one in files matching f
    bases = with_ext(files, '')
    files = files[!i & bases == bases[i]]  # files with same basename as f (Rmd)
    if (length(files) == 0 || any(require_rebuild(files, f))) render_new(f)
  }
}

# render Rmd in a new R session
render_new = function(f, quiet = TRUE) xfun::Rscript_call(
  rmarkdown::render, list(f, envir = globalenv(), quiet = quiet),
  fail = c('Failed to render ', f)
)

#' Look for files that have been possibly modified or out-of-date
#'
#' Filter files by checking if their modification times or MD5 checksums have
#' changed.
#'
#' The function \code{filter_newfile()} returns paths of source files that do
#' not have corresponding output files, e.g., an \file{.Rmd} file that doesn't
#' have the \file{.html} output file.
#'
#' The function \code{filter_timestamp()} compares the modification time of an
#' Rmd file with that of its output file, and returns the path of a file if it
#' is newer than its output file by \code{N} seconds (or if the output file does
#' not exist), where \code{N} is obtained from the R global option
#' \code{blogdown.time_diff}. By default, \code{N = 0}. You may change it via
#' \code{options()}, e.g., \code{options(blogdown.time_diff = 5)} means an Rmd
#' file will be returned when its modification time at least 5 seconds newer
#' than its output file's modification time.
#'
#' The function \code{filter_md5sum()} reads the MD5 checksums of files from a
#' database (a tab-separated text file), and returns the files of which the
#' checksums have changed. If the database does not exist, write the checksums
#' of files to it, otherwise update the checksums after the changed files have
#' been identified. When a file is modified, its MD5 checksum is very likely to
#' change.
#'
#' These functions can be used to determine which Rmd files to be rebuilt in a
#' \pkg{blogdown} website. See \code{\link{build_site}()} for more information.
#' @param files A vector of file paths.
#' @return The filtered file paths.
#' @export
filter_newfile = function(files) {
  files[!file_exists(output_file(files))]
}

#' @rdname filter_newfile
#' @export
filter_timestamp = function(files) {
  files[require_rebuild(output_file(files), files)]
}

#' @param db Path to the database file.
#' @rdname filter_newfile
#' @export
filter_md5sum = function(files, db = 'blogdown/md5sum.txt') {
  opt = options(stringsAsFactors = FALSE); on.exit(options(opt), add = TRUE)
  md5 = data.frame(file = files, checksum = tools::md5sum(files))  # new checksums
  if (!file.exists(db)) {
    dir_create(dirname(db))
    write.table(md5, db, row.names = FALSE)
    return(files)
  }
  old = read.table(db, TRUE)  # old checksums (2 columns: file path and checksum)
  one = merge(md5, old, 'file', all = TRUE, suffixes = c('', '.old'))
  # exclude files if checksums are not changed
  files = setdiff(files, one[one[, 2] == one[, 3], 'file'])
  i = is.na(one[, 2])
  one[i, 2] = one[i, 3]  # update checksums
  write.table(one[, 1:2], db, row.names = FALSE)
  files
}

# detect architecture of the system
detect_arch = function() {
  info = Sys.info()
  m = info['machine']
  if (grepl('^(aarch|arm)', m)) {
    if (grepl('^(aarch|arm)64', m)) 'arm64' else 'arm'
  } else if (length(grep('64', unlist(info[c('machine', 'release')]))) > 0)
    '64bit' else '32bit'
}

# build .Rmarkdown to .markdown, and .Rmd to .html unless the global option
# blogdown.method = 'markdown'
output_file = function(file) {
  ext = if (build_method() == 'markdown') 'md' else 'html'
  ext = rep(ext, length(file))
  ext[grep('[.][Rr]markdown$', file)] = 'markdown'
  with_ext(file, ext)
}

opts = knitr:::new_defaults()

# execute code in the site root dir
in_root = function(expr) xfun::in_dir(site_root(), expr)

# read config file and cache the options (i.e. do not read again unless the config is newer)
load_config = function() in_root({
  config = opts$get('config')
  f = find_config(); m = file.info(f)[, 'mtime']
  # read config only if it has been updated
  if (identical(attr(config, 'config_time'), m)) return(config)
  parser = switch(
    basename(f), 'config.toml' = read_toml, 'config.yaml' = yaml_load_file
  )
  config = parser(f)
  attr(config, 'config_time') = m
  opts$set(config = config)
  config
})

# check if the user has configured Multilingual Mode for Hugo in config.toml
get_lang = function(config = load_config()) {
  if (generator() == 'hugo') get_config('defaultContentLanguage', NULL, config)
}

# a horizontal rule
hrule = function(char = '-', width = getOption('width')) {
  paste(rep('-', width), collapse = '')
}

warning2 = function(...) warning(..., call. = FALSE)

message2 = function(..., files = NULL) {
  message(hrule())
  message(...)
  message(hrule())
  for (f in files) open_file(f)
}

msg1 = function(...) msg_cat('* ', ..., '\n')
msg2 = function(...) msg_cat('\n==> ', ..., '\n\n')

msg_init = function(...) msg_cat('\u2015 ', ..., '\n')  # -
msg_next = function(...) msg_cat('\u007c ', ..., '\n')  # |
msg_todo = function(...) msg_cat('\u25cf ', "[TODO] ", ..., '\n')  # solid dot
msg_okay = function(...) msg_cat('\u25cb ', ..., '\n')  # o
msg_done = function(...) msg_init("Check complete: ", ..., '\n')

# c(ITEM, ITEM, ITEM) ->
#   before ITEM after sep
#   before ITEM after sep
#   before ITEM after
indent_list = function(x, before = '', after = '', sep = '\n') {
  paste0('  ', before, x, after, collapse = sep)
}

action_list = function(x, action = 'file.remove') {
  paste0('  ', action, '(c(\n', indent_list(x, '"', '"', ',\n'), '\n  ))')
}

# return a list of files to be opened initially in an RStudio project
initial_files = function(n = 10) {
  files = list.files(content_file(), md_pattern, full.names = TRUE, recursive = TRUE)
  # if .Rmd has .md output, exclude .md
  i = grep('^[Rr]', exts <- xfun::file_ext(files))
  files = setdiff(files, with_ext(files[i], sub('^[Rr]', '', exts[i])))
  files = head(files, n)
  c(files, existing_files(c('netlify.toml', '.Rprofile', config_files())))
}

generator = function() get_option('blogdown.generator', 'hugo')

# config files for different site generators
config_files = function(which = generator()) {
  all = list(
    hugo = c('config.toml', 'config.yaml'),  # only support TOML and YAML (no JSON)
    jekyll = '_config.yml',
    hexo = '_config.yml'
  )
  all$hugo = c(all$hugo, file.path('config', '_default', all$hugo))
  if (is.null(which)) all else all[[which]]
}

find_config = function(files = config_files(), error = TRUE) {
  f = head(existing_files(files), 1)
  if (length(f) == 0 && error) stop(
    'Cannot find the configuration file ', paste(files, collapse = ' | '), ' of the website'
  )
  f
}

# figure out the possible root directory of the website
site_root = function(config = config_files(), .site_dir = NULL) {
  if (!is.null(root <- opts$get('site_root'))) return(root)
  owd = getwd(); on.exit(setwd(owd), add = TRUE)
  # if starting point has been provided, change to this directory
  if (is.null(.site_dir)) .site_dir = get_option('blogdown.site_root')
  if (!is.null(.site_dir)) setwd(.site_dir)
  paths = NULL
  while (length(find_config(config, error = FALSE)) == 0) {
    w1 = getwd(); w2 = dirname(w1)
    paths = c(paths, w1)
    if (w1 == w2) stop(
      'Could not find ', paste(config, collapse = ' / '), ' under\n',
      paste('  ', paths, collapse = '\n')
    )
    setwd('..')
  }
  root = getwd(); opts$set(site_root = root)
  root
}

#' Read and write TOML data (Tom's Obvious Markup Language)
#'
#' The function \code{read_toml()} reads TOML data from a file or a character
#' vector, and the function \code{write_toml()} converts an R object to TOML.
#'
#' For \code{read_toml()}, it first tries to use the R package \pkg{RcppTOML} to
#' read the TOML data. If \pkg{RcppTOML} is not available, it uses Hugo to
#' convert the TOML data to YAML, and reads the YAML data via the R package
#' \pkg{yaml}. If Hugo is not available, it falls back to a naive parser, which
#' is only able to parse top-level fields in the TOML data, and it only supports
#' character, logical, and numeric (including integer) scalars.
#'
#' For \code{write_toml()}, it converts an R object to YAML via the R package
#' \pkg{yaml}, and uses Hugo to convert the YAML data to TOML.
#' @param file Path to an input (TOML or YAML) file.
#' @param x For \code{read_toml()}, the TOML data as a character vector (it is
#'   read from \code{file} by default; if provided, \code{file} will be
#'   ignored). For \code{write_toml()}, an R object to be converted to TOML.
#' @param strict Whether to try \pkg{RcppTOML} and Hugo only (i.e., not to use
#'   the naive parser). If \code{FALSE}, only the naive parser is used (this is
#'   not recommended, unless you are sure your TOML data is really simple).
#' @return For \code{read_toml()}, an R object. For \code{write_toml()},
#'   \code{toml2yaml()}, and \code{yaml2toml()}, a character vector (marked by
#'   \code{xfun::\link{raw_string}()}) of the TOML/YAML data if \code{output =
#'   NULL}, otherwise the TOML/YAML data is written to the output file.
#' @export
#' @examples
#' \dontrun{
#' v = blogdown::read_toml(x = c('a = 1', 'b = true', 'c = "Hello"', 'd = [1, 2]'))
#' v
#' blogdown::write_toml(v)
#' }
read_toml = function(file, x = read_utf8(file), strict = TRUE) {
  if (strict) {
    x2 = read_toml(x = x, strict = FALSE)  # obtain the names of top-level fields
    ok = FALSE
    if (hugo_available()) {
      f2 = tempfile('toml', fileext = '.md'); on.exit(unlink(f2), add = TRUE)
      write_utf8(c('+++', x, '+++'), f2)
      # Hugo may fail to convert TOML to YAML, e.g., https://community.rstudio.com/t/86903
      x = if (!is.null(hugo_convert_one(f2))) {
        ok = TRUE
        yaml_load_file(f2)
      }
    }
    if (!ok && xfun::loadable('RcppTOML')) {
      x = paste(x, collapse = '\n')
      parser = getFromNamespace('parseTOML', 'RcppTOML')
      x = parser(x, fromFile = FALSE, escape = FALSE)
      ok = TRUE
    }
    if (missing(strict)) {
      if (!ok) return(x2)
    } else if (!ok) stop(
      'Neither Hugo nor the R package RcppTOML is available or able to parse the TOML data.'
    )
    return(sort_by_names(x, names(x2)))
  }

  # extract the top-level key name, e.g., foo.bar.baz -> foo
  keys = function(x) {
    unlist(lapply(strsplit(x, '[.]'), `[[`, 1))
  }
  # generate list(name = x)
  named_list = function(x, name) setNames(list(x), name)

  # remove comments
  x = gsub('\\s+#.+', '', x)

  # arbitrary values of the form 'foo = bar' or '[foo]' or '[[foo.bar]]'
  r = '^(([[:alnum:]_]+?)\\s*=\\s*(.+)\\s*|\\[{1,2}([^]]+)\\]{1,2}(\\s*))$'
  m = regexec(r, x)
  z = lapply(regmatches(x, m), function(v) {
    if (length(v) < 6) return()
    # when data is '[foo]' instead of 'foo = bar', just return NULL
    if (v[3] == '') return(named_list(NULL, keys(v[5])))
    y = v[4]
    # strings
    if (grepl(r <- '^"([^"]*?)"$', y)) y = gsub(r, '\\1', y) else {
      # boolean
      if (y %in% c('true', 'false')) y = as.logical(y) else {
        # numbers
        if (grepl('^[0-9.]+$', y)) {
          y2 = as.numeric(y)
          if (!is.na(y2)) {
            y = y2
            y2 = as.integer(y)
            if (y2 == y) y = y2
          }
        }
      }
    }
    named_list(y, v[3])
  })
  do.call(c, z)
}

#' @param output Path to an output file. If \code{NULL}, the TOML data is
#'   returned, otherwise the data is written to the specified file.
#' @export
#' @rdname read_toml
write_toml = function(x, output = NULL) {
  if (!hugo_available('0.37')) stop(
    'Hugo >= 0.37 is required but not found. Run blogdown::install_hugo()?'
  )
  f = tempfile('yaml', fileext = '.md'); on.exit(unlink(f), add = TRUE)
  write_utf8(c('---', as.yaml(x), '---'), f)
  hugo_convert_one(f, 'TOML')
  x = trim_ws(read_utf8(f))
  i = which(x == '+++')
  if ((n <- length(i)) < 2)
    stop('Wrong TOML data generated by Hugo:\n', paste(x, collapse = '\n'))
  if (i[n] - i[1] <= 1) return('')
  x = x[(i[1] + 1):(i[n] - 1)]
  while((n <- length(x)) > 0 && x[n] == '') x = x[-n]  # remove empty lines at the end
  if (is.null(output)) xfun::raw_string(x) else write_utf8(x, output)
}

#' @export
#' @rdname read_toml
toml2yaml = function(file, output = NULL) {
  x = read_toml(file, strict = TRUE)
  x = as.yaml(x)
  if (is.null(output)) x else write_utf8(x, output)
}

#' @export
#' @rdname read_toml
yaml2toml = function(file, output = NULL) {
  x = yaml_load_file(file)
  write_toml(x, output)
}

# reorder x according to a given vector of names, e.g., c(b = 1, a = 2, c = 3)
# -> c(c = 3, a = 2, b = 1) when the given names are c('c', 'a')
sort_by_names = function(x, names) {
  m = names(x)
  x[c(intersect(names, m), setdiff(m, names))]
}

#' Create the configuration (file) for Netlify
#'
#' This function provides some default configurations for a Huge website to be
#' built via Hugo and deployed on Netlify. It sets the build command for the
#' production and preview contexts, respectively (for preview contexts such as
#' \samp{deploy-preview}, the command will build future posts). It also sets the
#' publish directory according to your setting in Hugo's config file (if it
#' exists, otherwise it will be the default \file{public} directory). The Hugo
#' version is set to the current version of Hugo found on your computer.
#' @param output Path to the output file, or \code{NULL}. If the file exists and
#'   the R session is interactive, you will be prompted to decide whether to
#'   overwrite the file.
#' @param new_config If any default configuration does not apply to your site,
#'   you may provide a list of configurations to override the default. For
#'   example, if you want to use Hugo v0.25.1, you may use \code{new_config =
#'   list(build = list(environment = list(HUGO_VERSION = '0.25.1')))}.
#' @return If \code{output = NULL}, a character vector of TOML data representing
#'   the configurations (which you can preview and decide whether to write it to
#'   a file), otherwise the TOML data is written to a file.
#' @references See Netlify's documentation on the configuration file
#'   \file{netlify.toml} for the possible settings:
#'   \url{https://docs.netlify.com/configure-builds/file-based-configuration/}
#' @export
#' @examples
#' blogdown::config_netlify(output = NULL)  # default data
#'
#' # change the publish dir to 'docs/'
#' blogdown::config_netlify(NULL, list(build = list(publish = 'docs')))
config_netlify = function(output = 'netlify.toml', new_config = list()) {
  if (xfun::is_R_CMD_check() && !hugo_available()) {
    warning('Hugo was not found. You may install it with blogdown::install_hugo().')
    return()
  }
  # default config
  d = list(
    build = list(
      command = 'hugo',
      publish = tryCatch(publish_dir(tmp = FALSE), error = function(e) 'public'),
      environment = list(HUGO_VERSION = as.character(hugo_version()))
    ),
    context = list(
      production = list(
        environment = list(HUGO_ENV = "production")
      ),
      `deploy-preview` = list(
        command = "hugo -F -b $DEPLOY_PRIME_URL"
      )
    )
  )
  d$context$`branch-deploy` = d$context$`deploy-preview`
  d = modifyList(d, new_config)
  f = tempfile('netlify'); on.exit(unlink(f), add = TRUE)
  write_toml(d, f)
  if (is.null(output)) xfun::file_string(f) else {
    if (file.exists(output)) {
      message("The current existing '", output, "' is:")
      message2(xfun::file_string(output), files = output)
      message("The new '", output, "' will be:")
      message2(xfun::file_string(f))
      if (!yes_no(sprintf("Overwrite the existing '%s'?", output)))
        return(warning(
          "Cannot write to the file '", output, "' because it exists. You have ",
          "to delete it (if you do not need it any more) before I can write to it."
        ))
    }
    file.copy(f, output, overwrite = TRUE)
    invisible(output)
  }
}

#' Create the configuration file for Vercel
#'
#' Create \file{vercel.json} that contains the Hugo version currently used.
#' @param output Path to the output file, or \code{NULL} to print the config.
#' @references Vercel: \url{https://vercel.com}
#' @export
config_vercel = function(output = 'vercel.json') {
  d = list(build = list(env = list(HUGO_VERSION = as.character(hugo_version()))))
  d = jsonlite::toJSON(d, pretty = TRUE, auto_unbox = TRUE)
  if (is.null(output)) return(d)
  if (file.exists(output)) {
    warning("The output file '", output, "' exists and will not be overwritten.")
    return(d)
  }
  write_utf8(d, output)
}

#' Create or modify the \file{.Rprofile} file for a website project
#'
#' If the file \file{.Rprofile} does not exist in the current directory, copy
#' the file from the \file{resources} directory of \pkg{blogdown}. If the option
#' \code{blogdown.hugo.version} is not found in this file, append
#' \code{options(blogdown.hugo.version = "VERSION")} to it, where \code{VERSION}
#' is obtained from \code{\link{hugo_version}()}.
#' @export
#' @return As a side-effect, the file \file{.Rprofile} is created or modified.
config_Rprofile = function() {
  f1 = '.Rprofile'; f2 = pkg_file('resources', 'Rprofile'); x2 = xfun::file_string(f2)
  if (file.exists(f1)) {
    message("The file '", f1, "' exists, so I will not overwrite it with:")
    message2(x2, files = f1)
  }
  file.copy(f2, f1, overwrite = FALSE)
  ver = sprintf('\n# fix Hugo version\noptions(blogdown.hugo.version = "%s")\n', hugo_version())
  if (!any(grepl('blogdown[.]hugo[.]version', x1 <- xfun::file_string(f1)))) {
    if (!identical(x1, x2)) message2(
      "I didn't find the option blogdown.hugo.version in '", f1,
      "', so I will append the option to it.", files = f1
    )
    cat(ver, file = f1, append = TRUE)
  }
}

# option names may be case insensitive
get_config = function(field, default, config = load_config()) {
  config[[field]] %n% index_ci(config, field) %n% default
}

# index an object with a case-insensitive name
index_ci = function(x, name) {
  x[[match(tolower(name), tolower(names(x)))]]
}

# read the publishDir option in config if the temporary publish dir is not set
publish_dir = function(config = load_config(), tmp = TRUE, default = 'public') {
  p = if (tmp) publish_dir_tmp()
  if (is.null(p)) get_config('publishDir', default, config) else p
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
open_file = function(x, open = interactive(), line = -1L) {
  if (open) tryCatch(rstudioapi::navigateToFile(x, line), error = function(e) file.edit(x))
}

# produce a dash-separated filename by replacing non-alnum chars with -
dash_filename = function(
  string, pattern = '[^[:alnum:]]+',
  pre = get_option('blogdown.filename.pre_processor', identity)
) {
  tolower(gsub('^-+|-+$', '', gsub(pattern, '-', pre(string), perl = TRUE)))
}

# return a filename for a post based on title, date, etc
post_filename = function(title, subdir, ext, date, lang = '', bundle = use_bundle()) {
  if (is.null(lang)) lang = ''
  file = dash_filename(title)
  d = dirname(file); f = basename(file)
  if (is.null(subdir) || subdir == '') subdir = '.'
  d = if (d == '.') subdir else file.path(subdir, d)
  d = gsub('/+$', '', d)
  f = date_filename(f, date)
  f = gsub('^([.]/)+', '', file.path(d, f))
  paste0(f, if (bundle) '/index', if (lang != '') '.', lang, ext)
}

date_filename = function(path, date, replace = FALSE) {
  if (length(date) == 0 || is.na(date)) date = ''
  date = format(date)
  if (date == '') return(path)
  # FIXME: this \\d{4} will be problematic in about 8000 years
  m = grepl(r <- '(^|[\\/])\\d{4}-\\d{2}-\\d{2}-', path)
  if ( replace &&  m) path = gsub(r, paste0('\\1', date, '-'), path)
  if (!replace && !m) path = paste(date, path, sep = '-')
  path
}

# give a filename, return a slug by removing the date and extension (and possible index.md)
post_slug = function(x) {
  x = gsub('([.][[:alnum:]]+){1,2}$', '', x)
  if (basename(x) == 'index') x = dirname(x)
  trim_ws(gsub('^\\d{4}-\\d{2}-\\d{2}-', '', basename(x)))
}

use_bundle = function() {
  get_option('blogdown.new_bundle', generator() == 'hugo' && hugo_available('0.32'))
}

# don't add slugs to posts when creating new posts as bundles and permalinks is
# not set in config: https://github.com/rstudio/blogdown/issues/370
auto_slug = function() {
  if (!use_bundle()) return(TRUE)
  cfg = load_config()
  !is.null(cfg[['permalinks']])
}

trim_ws = function(x) gsub('^\\s+|\\s+$', '', x)

run_script = function(script, ...) {
  if (!empty_script(script) && xfun::Rscript(c(shQuote(script), ...)) != 0)
    stop('Failed to run ', script)
}

empty_script = function(file) {
  !file_exists(file) || length(grep('^\\s*#', read_utf8(file), invert = TRUE)) == 0
}

expand_grid = function(...) {
  expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

by_products = function(x, suffix = c('_files', '_cache', '.html')) {
  sx = xfun::sans_ext(x)
  if (length(suffix) == 1) return(paste0(sx, suffix))
  ma = expand_grid(suffix, sx)
  if (nrow(ma) > 0) paste0(ma[, 2], ma[, 1])
}

rmd_pattern = '[.][Rr](md|markdown)$'
md_pattern  = '[.][Rr]?(md|markdown)$'

# scan YAML metadata of all Rmd/md files
scan_yaml = function(warn = TRUE) {
  # error if this function is not called inside a website directory
  files = tryCatch(list_rmds(pattern = md_pattern), error = function(e) NULL)
  if (length(files) == 0) return(list())
  res = lapply(files, function(f) {
    yaml = mtime_cache(f, fetch_yaml(f), 'scan_yaml')
    if (length(yaml) == 0) return()
    yaml = yaml[-c(1, length(yaml))]
    if (length(yaml) == 0) return()
    tryCatch(yaml::yaml.load(paste(yaml, collapse = '\n')), error = function(e) {
      if (warn) {
        warning("Cannot parse the YAML metadata in '", f, "': ", e$message)
        NULL
      } else {
        structure(list(), yaml_error = e)
      }
    })
  })
  setNames(res, files)
}

# cache a value computed from a file (if mtime has not changed, use the cache)
mtime_cache = local({
  global = list()
  cached = function(file, db) {
    (file %in% names(db)) && identical(db[[file]][['mtime']], file.mtime(file))
  }
  function(file, value, key) {
    db = global[[key]]
    if (cached(file, db)) return(db[[file]][['value']])
    global[[key]][[file]] <<- list(mtime = file.mtime(file), value = value)
    value
  }
})

# collect specific fields of all YAML metadata
collect_yaml = function(
  fields = c('categories', 'tags'), uniq = TRUE, sort = TRUE
) {
  res = list()
  meta = scan_yaml()
  for (i in fields) {
    res[[i]] = unlist(lapply(meta, function(m) if (is.list(m)) m[[i]]))
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
  if (open) for (f in files) open_file(f, TRUE)
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

# annotate seq type values because both single value and list values are
# converted to vector by default
yaml_load = function(x) yaml::yaml.load(
  x, handlers = list(
    seq = function(x) {
      # continue coerce into vector because many places of code already assume this
      if (length(x) > 0) {
        x = flatten_seq(x)
        if (!is.null(x)) attr(x, 'yml_type') = 'seq'
      }
      x
    }
  )
)

# flatten the list only if all elements are of length 1 and unnamed (e.g., post
# categories and tags); should not flatten in other cases, e.g.,
# https://github.com/rstudio/blogdown/issues/684
flatten_seq = function(x) {
  vec = is.list(x) && all(vapply(x, function(v) {
    length(v) == 1 && is.null(names(v))
  }, logical(1)))
  if (vec) unlist(x, recursive = FALSE) else x
}

yaml_load_file = function(...) yaml::yaml.load_file(...)

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
  .keep_empty = get_option('blogdown.yaml.empty', TRUE)
) {
  x = read_utf8(file)
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
    write_utf8(c('---', yml, '---', res$body), file)
  } else warning("Could not detect YAML metadata in the post '", file, "'")
}

# prepend YAML of one file to another file
prepend_yaml = function(from, to, body = read_utf8(to), callback = identity) {
  x = c(callback(fetch_yaml(from)), '', body)
  write_utf8(x, to)
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

system2_quiet = function(...) system2(..., stdout = FALSE, stderr = FALSE)

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

rstudio_mode = local({
  x = NULL
  function() {
    if (!is.null(x)) return(x)
    x <<- tryCatch(
      tolower(rstudioapi::versionInfo()$mode),
      error = function(e) ''
    )
  }
})

is_rstudio = function() rstudio_mode() != ''
is_rstudio_server = function() rstudio_mode() == 'server'

# tweak some env vars when building a site or running the hugo server
tweak_hugo_env = function(baseURL = NULL, relativeURLs = NULL, server = FALSE) {
  # set baseURL properly when it doesn't contain protocol or domain:
  # https://github.com/gohugoio/hugo/issues/7823 (add example.org/ to it); or
  # when relativeURLs = true, set baseURL to /
  config = load_config()
  b = if (is.null(baseURL)) get_config('baseurl', '/', config) else baseURL
  b = sub('^/([^/].*)', '\\1', b)
  c1 = b != '/' && !grepl('^https?://[^/]+', b)
  c2 = if (is.null(relativeURLs)) get_config('relativeurls', FALSE, config) else relativeURLs
  if (server && c1) b = paste0(if (grepl('^//', b)) 'http:' else 'http://example.org/', b)

  vars = c(HUGO_BASEURL = if (c2) '/' else b, HUGO_RELATIVEURLS = tolower(c2))
  if (server) {
    vars = c(vars, HUGO_BLOGDOWN_POST_RELREF = 'true')
    c3 = get_config('ignoreErrors', NA, config)
    # should also ignore error-missing-instagram-accesstoken, but I don't know
    # how to configure ignoreErrors to be an array through the env var
    if (all(is.na(c3))) vars = c(vars, HUGO_IGNOREERRORS = 'error-remote-getjson')
  }
  v = set_envvar(vars)
  exit_call(function() set_envvar(v))
}

get_author = function() {
  if (!is.null(a <- getOption('blogdown.author'))) return(a)
  if (xfun::loadable('whoami')) whoami::fullname('') else ''
}

get_subdirs = function() {
  owd = setwd(content_file()); on.exit(setwd(owd), add = TRUE)
  files = list_files(include.dirs = TRUE)
  files = sub('^[.]/', '', files)
  i = file_test('-d', files)
  dirs = files[i]
  dirs = dirs[!grepl('_(files|cache)/?$', dirs)]

  # exclude dirs that contain index.??? files
  files = files[!i]
  for (d in dirname(files[bundle_index(files)])) {
    dirs = dirs[substr(dirs, 1, nchar(d)) != d]
  }
  unique(dirs)
}

# is a file the index page of a leaf bundle? i.e., index.*; the filename may
# also contain language code, e.g., index.fr.Rmd
bundle_index = function(x, ext = TRUE) {
  x = basename(x)
  if (ext) x = xfun::sans_ext(x)
  grepl(bundle_regex(), x)
}

bundle_regex = function(x = '$') paste0('^index([.][a-z]{2})?', x)

xfun_session_info = function() {
  tryCatch(paste('Hugo version:', hugo_version()), error = function(e) NULL)
}

clean_hugo_cache = function() {
  if (!file.exists(tmp <- Sys.getenv('TMPDIR'))) return()
  # clean up the hugo cache dir during R CMD check
  if (xfun::is_R_CMD_check())
    unlink(file.path(tmp, 'hugo_cache'), recursive = TRUE)
}

# add the time of now to a date
format_datetime = function(date, time = TRUE) {
  if (inherits(date, c('Date', 'POSIXct', 'POSIXlt'))) date = format(date, '%Y-%m-%d')
  if (is.logical(time)) {
    time = if (isTRUE(time)) format(Sys.time(), 'T%T%z') else ''
  }
  paste0(date, time)
}

unicode_capable = local({
  ok = NULL; x = '\u25ba'  # a test Unicode character
  function() {
    if (is.null(ok)) ok <<- identical(capture.output(cat(x)), x)
    ok
  }
})

yes_no = function(question, prompt = if (unicode_capable()) '\u25ba ' else '> ') {
  interactive() && tolower(substr(readline(paste0(prompt, question, ' (y/n) ')), 1, 1)) == 'y'
}

source_file = function(...) sys.source(..., chdir = TRUE, keep.source = FALSE)

source_profile = function(dir, ...) {
  if (file_exists(f <- file.path(dir, '.Rprofile'))) source_file(f, ...)
}

# treat the special value I(NA) as NULL; see .onLoad()
get_option = function(x, default = NULL) na2null(getOption(x), default)

na_null = I(NA)
na2null = function(x, default = NULL) {
  if (is.null(x) || identical(x, I(NA))) default else x
}

# global options in blogdown that are likely to be useful to some users
.options = local({
  g = generator()
  i = c(
    'filename.pre_processor', 'files_filter', 'generator', 'initial_files',
    'knit.on_save', 'knit.serve_site', 'method', 'rename_file',
    'serve_site.startup', 'server.timeout', 'server.verbose', 'site_root',
    'subdir_fun', 'time_diff', 'warn.future', 'widgetsID', 'yaml.empty',
    paste0(g, '.server'),
    if (g == 'hugo') c(
      'hugo.args', 'hugo.dir', 'hugo.version', 'new_bundle', 'server.wait'
    )
  )
  # default them to I(NA) instead of NULL for reasons explained in .onLoad()
  x = setNames(rep(list(na_null), length(i)), i)
  x = c(x, list(
    author = get_author(), subdir = 'post', title_case = FALSE, ext = '.md', time = FALSE
  ))
  names(x) = paste0('blogdown.', names(x))
  x = x[sort(names(x))]
  x
})

# look up sys.calls() to see if current call is from a certain parent function
parent_call = function(name) {
  for (f in sys.calls()) if (f[[1]] == as.symbol(name)) return(TRUE)
  FALSE
}
