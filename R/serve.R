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
  serve = switch(
    generator(), hugo = serve_it(),
    jekyll = serve_it(
      baseurl = get_config2('baseurl', ''),
      pdir = get_config2('destination', '_site')
    ),
    hexo = serve_it(
      baseurl = get_config2('root', ''),
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

# this function is primarily for users who click the Knit button in RStudio (the
# main purpose is to suppres a message that is not useful to Knit button users);
# normally you wouldn't need to call it by yourself
preview_site = function(...) {
  opts$set(server_message = FALSE)
  on.exit(opts$set(server_message = NULL), add = TRUE)
  invisible(serve_site(...))
}

serve_it = function(pdir = publish_dir(), baseurl = site_base_dir()) {
  g = generator(); config = config_files(g)
  function(...) {
    okay = FALSE  # whether the server is successfully started
    root = site_root(config)
    if (root %in% opts$get('served_dirs')) {
      if (isFALSE(opts$get('server_message'))) return()
      servr::browse_last()
      return(message(
        'The site has been served under the directory "', root, '". I have tried ',
        'to reopen it for you with servr::browse_last(). If you do want to ',
        'start a new server, you may stop existing servers with ',
        'blogdown::stop_server(), or restart R. Normally you should not need to ',
        'serve the same site multiple times in the same R session',
        if (servr:::is_rstudio()) c(
          ', otherwise you may run into issues like ',
          'https://github.com/rstudio/blogdown/issues/404'
        ), '.'
      ))
    }

    on.exit(if (okay) opts$append(served_dirs = root), add = TRUE)
    owd = setwd(root); on.exit(setwd(owd), add = TRUE)

    if (!getOption('blogdown.generator.server', FALSE)) {
      build_site(TRUE)
      n = nchar(pdir)
      s = servr::httw(site.dir = pdir, baseurl = baseurl, handler = function(...) {
        files = c(...)
        # exclude changes in the publish dir
        files = files[substr(files, 1, n) != pdir]
        # re-generate only if Rmd/md or config files or layouts were updated
        if (length(grep('(_?layouts?|static|data)/|[.](toml|yaml)$', files)) ||
            length(grep(md_pattern, files)))
          build_site(TRUE, build_rmd = TRUE)
      }, dir = '.', ...)
      okay = TRUE
      return(invisible(s))
    }

    server = servr::server_config(...)

    # launch the hugo/jekyll/hexo server
    cmd = if (g == 'hugo') find_hugo() else g
    host = server$host; port = server$port; intv = server$interval
    if (!servr:::port_available(port, host)) stop(
      'The port ', port, ' at ', host, ' is unavailable', call. = FALSE
    )
    args_fun = match.fun(paste0(g, '_server_args'))
    cmd_args = args_fun(host, port)
    tweak_hugo_env()
    pid = bg_process(cmd, cmd_args)
    opts$set(pids = c(opts$get('pids'), pid))

    message(
      'Launching the server via the command:\n  ',
      paste(c(cmd, cmd_args), collapse = ' ')
    )
    i = 0
    repeat {
      Sys.sleep(1)
      if (server_ready(server$url)) break
      if (i >= getOption('blogdown.server.timeout', 30)) {
        proc_kill(pid)
        stop(
          'It took more than ', i, ' seconds to launch the server. ',
          'There may be something wrong. The process has been killed. ',
          'If the site needs more time to be built and launched, set ',
          'options(blogdown.server.timeout) to a larger value.',
          call. = FALSE
        )
      }
      i = i + 1
    }
    server$browse()
    okay = TRUE
    message(
      'Launched the ', g, ' server in the background (process ID: ', pid, '). ',
      'To stop it, call blogdown::stop_server() or restart the R session.'
    )

    watch = servr:::watch_dir('.', rmd_pattern)
    unix = .Platform$OS.type == 'unix'
    watch_build = function() {
      if (watch()) {
        if (unix) tools::pskill(pid, tools::SIGSTOP)
        try(build_site(TRUE, run_hugo = FALSE, build_rmd = TRUE))
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

proc_kill = function(...) tools::pskill(...)

#' @export
#' @rdname serve_site
stop_server = function() {
  if (getOption('blogdown.generator.server', FALSE)) {
    for (i in opts$get('pids')) proc_kill(i)
    opts$set(pids = NULL)
  } else servr::daemon_stop()
  opts$set(served_dirs = NULL)
}

get_config2 = function(key, default) {
  res = yaml_load_file('_config.yml')
  res[[key]] %n% default
}

# start a background process, and return its process ID
bg_process = function(command, args = character(), timeout = 30) {
  id = NULL

  if (is_windows()) {
    # format of task list: hugo.exe    4592 Console      1     35,188 K
    tasklist = function() system2('tasklist', stdout = TRUE)
    pid1 = tasklist()
    system2(command, args, wait = FALSE)

    get_pid = function() {
      pid2 = setdiff(tasklist(), pid1)
      cmd = basename(command)
      # the process's info should start with the command name
      pid2 = pid2[substr(pid2, 1, nchar(cmd)) == cmd]
      if (length(pid2) == 0) return()
      m = regexec('\\s+([0-9]+)\\s+', pid2)
      for (v in regmatches(pid2, m)) if (length(v) >= 2) return(v[2])
    }
  } else {
    pid = tempfile(); on.exit(unlink(pid), add = TRUE)
    code = paste(
      c(shQuote(c(command, args)), ' > /dev/null & echo $! >', shQuote(pid)), collapse = ' '
    )
    system2('sh', c('-c', shQuote(code)))
    get_pid = function() {
      if (file.exists(pid)) readLines(pid)
    }
  }

  t0 = Sys.time()
  while (difftime(Sys.time(), t0, units = 'secs') < timeout) {
    if (length(id <- get_pid()) == 1) break
  }

  if (length(id) == 1) return(id)

  system2(command, args, timeout = timeout)  # see what the error is
  stop(
    'Failed to run the command in ', timeout, ' seconds (timeout): ',
    paste(shQuote(c(command, args)), collapse = ' ')
  )
}
