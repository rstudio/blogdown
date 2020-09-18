blogdown_site = function(input, ...) {

  output_dir = publish_dir()
  render = function(input_file, output_format, envir, quiet, encoding, ...) {
    # input_file is NULL when render the whole site, and is a file path when
    # rendering a single file (by clicking the Knit button)
    if (!is.null(input_file)) xfun::in_dir(site_root(), {
      input_file = rmarkdown::relative_to(getwd(), input_file)
      build_rmds(input_file)
    }) else {
      build_site()
      if (!quiet) message("\nOutput created: ", paste0(output_dir, '/index.html'))
    }
  }

  # return site generator
  list(
    name = basename(getwd()),
    output_dir = output_dir,
    render = render,
    subdirs = TRUE,
    clean = function() {
      c('blogdown', output_dir, clean_targets())
    }
  )
}

clean_targets = function() {
  rmds = list_rmds('content')
  files = by_products(rmds, c('.html', '.markdown'))
  files = files[file.exists(files)]
  c(files, 'static/rmarkdown-libs', list.files(
    'static', '.+_files$', recursive = TRUE, include.dirs = TRUE, full.names = TRUE
  ))
}
