#' Live preview a site
#'
#' The function \code{serve_site()} calls \code{servr::\link{httw}()} to start a
#' web server, watch for changes in the site, rebuild the site if necessary, and
#' refresh the web page automatically by default; \code{stop_server()} stops the
#' web server.
#'
#' Alternatively, you can set the global option
#' \code{\link{options}(blogdown.generator.server = TRUE)}, and
#' \code{serve_site()} will use the web server provided by the static site
#' generator, such as \code{\link{hugo_server}()}. This requires additional
#' packages \pkg{processx} and \pkg{later}. You may use this option when you
#' primarily work on plain Markdown posts instead of R Markdown posts, because
#' it can be faster to preview Markdown posts using the web server of the static
#' site generator. The web server will always be stopped when the R session is
#' ended, so you may consider restarting your R session if \code{stop_server}
#' fails to stop the server for some reason.
#' @param ... Arguments passed to \code{servr::httw()} (arguments \code{dir},
#'   \code{site.dir}, \code{baseurl}, and \code{handler} have been provided,
#'   hence you cannot customize these arguments).
#' @export
serve_site = function(...) {
  op = options(servr.daemon = getOption('servr.daemon', interactive()))
  on.exit(options(op), add = TRUE)
  serve = switch(
    generator(), hugo = serve_it(),
    jekyll = serve_it(
      '_config.yml', baseurl = get_config2('baseurl', ''),
      pdir = get_config2('destination', '_site')
    ),
    hexo = serve_it(
      '_config.yml', baseurl = get_config2('root', ''),
      pdir = get_config2('public_dir', 'public')
    ),
    stop("Cannot recognize the site (only Hugo, Jekyll, and Hexo are supported)")
  )
  serve(...)
}

server_ready = function(url) {
  !inherits(
    xfun::try_silent(suppressWarnings(readLines(url))), 'try-error'
  )
}

generator = function() getOption('blogdown.generator', 'hugo')

serve_it = function(
  config = c('config.toml', 'config.yaml'), pdir = publish_dir(),
  baseurl = site_base_dir()
) {
  function(...) {
    owd = setwd(site_root(config)); on.exit(setwd(owd), add = TRUE)

    if (!getOption('blogdown.generator.server', FALSE)) {
      if (is_windows() && getOption('servr.daemon', FALSE)) {
        if (!xfun::loadable('later')) stop(
          "Please install the 'later' package: install.packages('later')", call. = FALSE
        )
      }
      build_site(TRUE)
      n = nchar(pdir)
      return(servr::httw(site.dir = pdir, baseurl = baseurl, handler = function(...) {
        files = c(...)
        # exclude changes in the publish dir
        files = files[substr(files, 1, n) != pdir]
        # re-generate only if Rmd/md or config files or layouts were updated
        if (length(grep('(_?layouts?|static)/|[.](toml|yaml)$', files)) ||
            length(grep(md_pattern, files)))
          build_site(TRUE)
      }, dir = '.', ...))
    }

    if (!xfun::loadable('processx') || !xfun::loadable('later')) stop(
      "Please install the packages 'processx' and 'later'", call. = FALSE
    )
    server = servr::server_config(...)

    # launch the hugo/jekyll/hexo server
    g = generator()
    cmd = if (g == 'hugo') find_hugo() else g
    host = server$host; port = server$port; intv = server$interval
    args_fun = match.fun(paste0(g, '_server_args'))
    cmd_args = args_fun(host, port)
    p1 = proc_new(cmd, cmd_args)
    pid = p1$get_pid()
    opts$set(pids = c(opts$get('pids'), pid))
    p1_print = function() {
      if (proc_print(p1, c(FALSE, TRUE))) later::later(p1_print, intv)
    }

    message(
      'Launching the server via the command:\n  ',
      paste(c(cmd, cmd_args), collapse = ' ')
    )
    i = 0
    repeat {
      Sys.sleep(1)
      if (server_ready(server$url)) break
      if (i >= getOption('blogdown.server.timeout', 30)) {
        proc_print(p1)
        cat('\n')
        p1$kill()
        stop(
          'It took more than ', i, ' seconds to launch the server. ',
          'There may be something wrong. The process has been killed.',
          'If the site needs more time to be built and launched, set ',
          'options(blogdown.server.timeout) to a larger value.',
          call. = FALSE
        )
      }
      i = i + 1
    }
    server$browse()
    message(
      'Launched the ', g, ' server in the background (process ID: ', pid, '). ',
      'To stop it, call blogdown::stop_server() or restart the R session.'
    )
    p1_print()

    watch = servr:::watch_dir('.', rmd_pattern)
    unix = .Platform$OS.type == 'unix'
    watch_build = function() {
      if (watch()) {
        if (unix) tools::pskill(pid, tools::SIGSTOP)
        try(build_site(TRUE, run_hugo = FALSE))
        if (unix) tools::pskill(pid, tools::SIGCONT)
      }
      later::later(watch_build, intv)
    }
    watch_build()

    return(invisible())
  }
}

jekyll_server_args = function(host, port) {
  c('serve', '--port', port, '--host', host)
}

hexo_server_args = function(host, port) {
  c('server', '-p', port, '-i', host)
}

proc_new = function(..., stderr = '|') {
  processx::process$new(..., stderr = stderr)
}

# control = c(show_out, show_error)
proc_print = function(p, control = c(TRUE, TRUE)) {
  if (!p$is_alive()) return(FALSE)
  if (control[1]) {
    out = p$read_output_lines()
    if (length(out)) cat(out, file = stdout(), sep = '\n')
  }
  if (control[2]) {
    err = p$read_error_lines()
    if (length(err)) cat(err, file = stderr(), sep = '\n')
  }
  TRUE
}

proc_kill = function(pid) {
  if (is_windows()) {
    system2('taskkill', c('/f', '/pid', pid))
  } else {
    system2('kill', pid)
  }
}

#' @export
#' @rdname serve_site
stop_server = function() {
  if (getOption('blogdown.generator.server', FALSE)) {
    for (i in opts$get('pids')) proc_kill(i)
  } else servr::daemon_stop()
}

get_config2 = function(key, default) {
  res = yaml_load_file('_config.yml')
  res[[key]] %n% default
}
