blogdown_site = function(input, ...) {
  # start serving the site when a blogdown project is opened in RStudio
  if (interactive() && getOption('blogdown.serve_site.startup', TRUE)) try({
    if (!isTRUE(opts$get('startup'))) {
      rstudioapi::sendToConsole('blogdown:::preview_site(startup = TRUE)')
      opts$set(startup = TRUE)  # don't send the above code again in this session
    }
  })

  output_dir = publish_dir()
  render = function(input_file, output_format, envir, quiet, encoding, ...) {
    # input_file is NULL when render the whole site, and is a file path when
    # rendering a single file (by clicking the Knit button)
    if (!is.null(input_file)) xfun::in_dir(site_root(), {
      # set a global option
      opts$set(render_one = TRUE); on.exit(opts$set(render_one = NULL), add = TRUE)
      input_file = rmarkdown::relative_to(getwd(), input_file)
      if (!grepl(rmd_pattern, input_file)) return(message(
        'The file "', input_file, '" does not need to be knitted.'
      ))
      build_site(TRUE, run_hugo = FALSE, build_rmd = input_file)
      # run serve_site() to preview the site if the server has not been started
      if (interactive()) preview_site() else tryCatch(
        rstudioapi::sendToConsole('blogdown:::preview_site()', echo = FALSE)
      )
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
  c(files, 'static/rmarkdown-libs', list_files(
    'static', '.+_files$', include.dirs = TRUE
  ))
}
