blogdown_site = function(input, ...) {

  output_dir = publish_dir()
  render = function(input_file, output_format, envir, quiet, encoding, ...) {
    # we should disable knitting single posts in blogdown:
    # https://twitter.com/andrewheiss/status/1085316894180093952, but
    # unfortunately for blogdown sites, RStudio IDE doesn't run the site
    # generator, so this function won't be called when the Knit button is
    # clicked for a single post
    if (!is.null(input_file) && !getOption('blogdown.allow_knit', FALSE)) stop(
      'You probably should not knit the document in a blogdown project. Just call ',
      'blogdown::serve_site() once per R session. See https://bookdown.org/yihui/blogdown/workflow.html. ',
      'If you are sure you want to knit it and know what it means, set options(blogdown.allow_knit = TRUE).'
    )
    build_site()
    if (!quiet)
      message("\nOutput created: ", paste0(output_dir, '/index.html'))
  }

  # return site generator
  list(
    name = basename(getwd()),
    output_dir = output_dir,
    render = render,
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
