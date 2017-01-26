blogdown_site = function(input, ...) {

  output_dir = publish_dir()
  render = function(input_file, output_format, envir, quiet, encoding, ...) {
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
      c('blogdown', output_dir)
    }
  )
}
