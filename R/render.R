#' Build a website
#'
#' Compile all Rmd files and build the site through Hugo.
#'
#' You can use \code{\link{serve_site}()} to preview your website locally, and
#' \code{build_site()} to build the site for publishing.
#'
#' For the \code{method} argument: \code{method = "html"} means to render all
#' Rmd files to HTML via \code{rmarkdown::\link[rmarkdown]{render}()} (which
#' means Markdown is processed through Pandoc), and process the paths of
#' external dependencies generated from R code chunks, including images and HTML
#' dependencies.
#'
#' For all rendering methods, a custom R script \file{R/build.R} will be
#' executed if you have provided it under the root directory of the website
#' (e.g. you can compile Rmd to Markdown through
#' \code{knitr::\link[knitr]{knit}()} and build the side via
#' \code{\link{hugo_cmd}()}). \code{method = "custom"} means it is entirely up
#' to this R script how a website is rendered. The script is executed via
#' command line \command{Rscript "R/build.R"}, which means it is executed in a
#' separate R session. The value of the argument \code{local} is passed to the
#' command line (you can retrieve the command-line arguments via
#' \code{\link{commandArgs}(TRUE)}).
#' @param local Whether to build the website locally to be served via
#'   \code{\link{serve_site}()}.
#' @param method Different methods to build a website (each with pros and cons).
#'   See Details. The value of this argument will be obtained from the global
#'   option \code{getOption('blogdown.method')} when it is set.
#' @param run_hugo Whether to run \code{hugo_build()} after R Markdown files are
#'   compiled.
#' @note This function recompiles all R Markdown files by default, even if the
#'   output files are newer than the source files. If you want to build the site
#'   without rebuilding all R Markdown files, you should use
#'   \code{\link{hugo_build}()} instead.
#' @export
build_site = function(
  local = FALSE, method = c('html', 'custom'), run_hugo = TRUE
) {
  if (missing(method)) method = getOption('blogdown.method', method)
  method = match.arg(method)
  on.exit(run_script('R/build.R', as.character(local)), add = TRUE)
  if (method == 'custom') return()
  files = list_rmds('content', TRUE)
  if (local && length(files)) {
    files = files[mapply(require_rebuild, output_file(files), files)]
  }
  build_rmds(files)
  if (run_hugo) on.exit(hugo_build(local), add = TRUE)
  invisible()
}

list_rmds = function(dir, check = FALSE) {
  files = list.files(dir, rmd_pattern, recursive = TRUE, full.names = TRUE)
  # exclude Rmd that starts with _ (preserve these names for, e.g., child docs)
  # but include _index.Rmd/.md
  files = files[!grepl('^_', basename(files)) | grepl('^_index[.]', basename(files))]
  # do not allow special characters in filenames so dependency names are more
  # predictable, e.g. foo_files/
  if (check) bookdown:::check_special_chars(files)
  files
}

# raw indicates paths of dependencies are not encoded in the HTML output
build_rmds = function(files) {
  if (length(files) == 0) return()

  # copy by-products {/content/.../foo_(files|cache) dirs and foo.html} from
  # /blogdown/ or /static/ to /content/
  lib1 = by_products(files, c('_files', '_cache'))
  lib2 = gsub('^content', 'blogdown', lib1)  # /blogdown/.../foo_(files|cache)
  i = grep('_files$', lib2)
  lib2[i] = gsub('^blogdown', 'static', lib2[i])  # _files are copied to /static
  # move by-products of a previous run to content/
  dirs_rename(lib2, lib1)
  # move (new) by-products from content/ to blogdown/ or static/ to make the
  # source directory clean
  on.exit(dirs_rename(lib1, lib2), add = TRUE)

  root = getwd()
  base = site_base_dir()
  shared_yml = file.path(root, '_output.yml')
  copied_yaml = character(); on.exit(unlink(copied_yaml), add = TRUE)

  copy_output_yml = function(to) {
    if (!file.exists(shared_yml)) return()
    if (file.exists(copy <- file.path(to, '_output.yml'))) return()
    if (file.copy(shared_yml, copy)) copied_yaml <<- c(copied_yaml, copy)
  }

  for (f in files) {
    d = dirname(f)
    out = output_file(f, to_md <- is_rmarkdown(f))
    copy_output_yml(d)
    message('Rendering ', f)
    render_page(f)
    x = read_utf8(out)
    x = encode_paths(x, by_products(f, '_files'), d, base, to_md)
    if (to_md) {
      write_utf8(x, out)
    } else {
      if (getOption('blogdown.widgetsID', TRUE)) x = clean_widget_html(x)
      prepend_yaml(f, out, x, callback = function(s) {
        if (getOption('blogdown.draft.output', FALSE)) return(s)
        if (length(s) < 2 || length(grep('^draft: ', s)) > 0) return(s)
        append(s, 'draft: yes', 1)
      })
    }
  }
}

render_page = function(input, script = 'render_page.R') {
  # needs --slave due to this bug in Rscript:
  # https://stat.ethz.ch/pipermail/r-devel/2018-April/075897.html
  args = c('--slave', pkg_file('scripts', script), input, getwd())
  if (Rscript(shQuote(args)) != 0) stop("Failed to render '", input, "'")
}

# given the content of a .html file: replace content/*_files/figure-html with
# /*_files/figure-html since this dir will be moved to /static/, and move the
# rest of dirs under content/*_files/ to /static/rmarkdown-libs/ (HTML
# dependencies), so all posts share the same libs (otherwise each post has its
# own dependencies, and there will be a lot of duplicated libs when HTML widgets
# are used extensively in a website)

# example values of arguments: x = <html> code; deps = '2017-02-14-foo_files';
# parent = 'content/post';
encode_paths = function(x, deps, parent, base = '/', to_md = FALSE) {
  if (!dir_exists(deps)) return(x)
  if (!grepl('/$', parent)) parent = paste0(parent, '/')
  deps = basename(deps)
  need_encode = !to_md
  if (need_encode) {
    deps2 = encode_uri(deps)  # encode the path and see if it can be found in x
    # on Unix, paths containing multibyte chars are always encoded by Pandoc
    if (need_encode <- !is_windows() || any(grepl(deps2, x, fixed = TRUE))) deps = deps2
  }
  # find the dependencies referenced in HTML
  r = paste0('(<img src|<script src|<link href)(=")(', deps, '/)')

  # move figures to /static/path/to/post/foo_files/figure-html
  if (FALSE) {
    # this is a little more rigorous: the approach below ("\'?)(%s/figure-html/)
    # means process any paths that "seems to have been generated from Rmd"; the
    # optional single quote after double quote is only for the sake of
    # trelliscopejs, where the string may be "'*_files/figure-html'"
    r1 = paste0(r, '(figure-html/)')
    x = gsub(r1, paste0('\\1\\2', gsub('^content/', base, parent), '/\\3\\4'), x)
  }
  r1 = sprintf('("\'?)(%s/figure-html/)', deps)
  x = gsub(r1, paste0('\\1', gsub('^content/', base, parent), '\\2'), x, perl = TRUE)
  # move other HTML dependencies to /static/rmarkdown-libs/
  r2 = paste0(r, '([^/]+)/')
  x2 = grep(r2, x, value = TRUE)
  if (length(x2) == 0) return(x)
  libs = unique(gsub(r2, '\\3\\4', unlist(regmatches(x2, gregexpr(r2, x2)))))
  libs = file.path(parent, if (need_encode) decode_uri(libs) else libs)
  x = gsub(r2, sprintf('\\1\\2%srmarkdown-libs/\\4/', base), x)
  to = file.path('static', 'rmarkdown-libs', basename(libs))
  dirs_rename(libs, to, clean = TRUE)
  x
}
