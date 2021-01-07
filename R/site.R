blogdown_site = function(input, ...) {
  # set the site root dir
  opts$set(site_root = input)

  # start serving the site when a blogdown project is opened in RStudio
  if (interactive() && get_option('blogdown.serve_site.startup', FALSE)) try({
    if (!isTRUE(opts$get('startup'))) {
      rstudioapi::sendToConsole('blogdown:::preview_site(startup = TRUE)')
      opts$set(startup = TRUE)  # don't send the above code again in this session
    }
  })

  output_dir = publish_dir()
  render = function(input_file, output_format, envir, quiet, ...) {
    # input_file is NULL when render the whole site, and is a file path when
    # rendering a single file (by clicking the Knit button)
    if (!is.null(input_file)) xfun::in_dir(input, {
      # set a global option
      opts$set(render_one = TRUE); on.exit(opts$set(render_one = NULL), add = TRUE)
      input_file = rel_path(input_file)
      # when knitting a file not in the project root, RStudio starts R from the
      # dir of the file instead of the root, hence .Rprofile is ignored (#562)
      if (dirname(input_file) != '.') source_profile(input, globalenv())
      # only build R Markdown files (no need to build plain .md files)
      if (grepl(rmd_pattern, input_file))
        build_site(TRUE, run_hugo = FALSE, build_rmd = input_file)
      # run serve_site() to preview the site if the server has not been started
      if (interactive()) preview_site() else tryCatch(
        rstudioapi::sendToConsole('blogdown:::preview_site()', echo = FALSE)
      )
    }) else {
      build_site()
      if (!quiet) message(
        "\n==> The site has been generated to the directory '", output_dir, "'.\n\n",
        "** Note that normally you cannot just open the .html files in this directory ",
        "to view them in a browser. This directory need to be served before you can ",
        "preview web pages correctly (e.g., you may deploy the folder to a web server). ",
        "Alternatively, blogdown::serve_site() gives you a local preview of the site.\n"
      )
    }
  }

  # return site generator
  list(
    name = basename(getwd()),
    output_dir = output_dir,
    render = render,
    subdirs = TRUE,
    clean = function() {
      x = c('blogdown', output_dir, clean_targets())
      x[file.exists(x)]
    }
  )
}

clean_targets = function() {
  rmds = list_rmds()
  files = by_products(rmds, c('.html', '.markdown'))
  c(files, 'static/rmarkdown-libs', list_files(
    'static', '.+_files$', include.dirs = TRUE
  ))
}
