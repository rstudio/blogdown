#' Live preview a site
#'
#' The function \code{serve_site()} executes the server command of a static site
#' generator (e.g., \command{hugo server} or \command{jekyll server}) to start a
#' local web server, which watches for changes in the site, rebuilds the site if
#' necessary, and refreshes the web page automatically; \code{stop_server()}
#' stops the web server.
#'
#' By default, the server also watches for changes in R Markdown files, and
#' recompile them automatically if they are modified. This means they will be
#' automatically recompiled once you save them. If you do not like this
#' behavior, you may set \code{options(blogdown.knit.on_save = FALSE)} (ideally
#' in your \file{.Rprofile}). When this feature is disabled, you will have to
#' manually compile Rmd documents, e.g., by clicking the Knit button in RStudio.
#'
#' The site generator is defined by the global R option
#' \code{blogdown.generator}, with the default being \code{'hugo'}. You may use
#' other site generators including \code{jekyll} and \code{hexo}, e.g.,
#' \code{options(blogdown.generator = 'jekyll')}. You can define command-line
#' arguments to be passed to the server of the site generator via the global R
#' option \code{blogdown.X.server}, where \code{X} is \code{hugo},
#' \code{jekyll}, or \code{hexo}. The default for Hugo is
#' \code{options(blogdown.hugo.server = c('-D', '-F', '--navigateToChanged'))}
#' (see the documentation of Hugo server at
#' \url{https://gohugo.io/commands/hugo_server/} for the meaning of these
#' arguments).
#' @param ... Arguments passed to \code{servr::\link{server_config}()} (only
#'   arguments \code{host}, \code{port}, \code{browser}, \code{daemon}, and
#'   \code{interval} are supported).
#' @note For the Hugo server, the argument \command{--navigateToChanged} is used
#'   by default, which means when you edit and save a source file, Hugo will
#'   automatically navigate the web browser to the page corresponding to this
#'   source file (if the page exists). However, due to a Hugo bug
#'   (\url{https://github.com/gohugoio/hugo/issues/3811}), this automatic
#'   navigation may not always work for R Markdown posts, and you may have to
#'   manually refresh your browser. It should work reliably for pure Markdown
#'   posts, though.
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
# main purposes are to suppress a message that is not useful to Knit button
# users, and avoid rebuilding Rmd files because Knit button has done the job);
# normally you wouldn't need to call it by yourself
preview_site = function(..., startup = FALSE) {
  # when startup = FALSE, set knitting = TRUE permanently for this R session, so
  # that build_site() in serve_site() no longer automatically rebuilds Rmds on
  # save by default, and an Rmd has to be manually knitted
  if (startup) {
    opts$set(preview = TRUE)
    on.exit(opts$set(preview = NULL), add = TRUE)
  } else {
    opts$set(knitting = TRUE)
  }
  invisible(serve_site(...))
}

preview_mode = function() {
  isTRUE(opts$get('preview')) || isTRUE(opts$get('knitting'))
}

serve_it = function(pdir = publish_dir(), baseurl = site_base_dir()) {
  g = generator(); config = config_files(g)
  function(...) {
    root = site_root(config)
    if (root %in% opts$get('served_dirs')) {
      if (preview_mode()) return()
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

    owd = setwd(root); on.exit(setwd(owd), add = TRUE)

    build_it = function(files) {
      if (is.null(b <- getOption('blogdown.knit.on_save'))) {
        b = !isTRUE(opts$get('knitting'))
        if (!b) {
          options(blogdown.knit.on_save = b)
          message(
            'It seems you have clicked the Knit button in RStudio. If you prefer ',
            'knitting a document manually over letting blogdown automatically ',
            'knit it on save, you may set options(blogdown.knit.on_save = FALSE) ',
            'in your .Rprofile so blogdown will not knit documents automatically ',
            'again (I have just set this option for you for this R session). If ',
            'you prefer knitting on save, set this option to TRUE instead.'
          )
          files = b  # just ignore changed Rmd files, i.e., don't build them
        }
      }
      build_site(TRUE, run_hugo = FALSE, build_rmd = files)
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
    if (g == 'hugo') tweak_hugo_env()
    # if requested not to demonize the server, run it in the foreground process,
    # which will block the R session
    if (!server$daemon) return(system2(cmd, cmd_args))

    pid = if (getOption('blogdown.use.processx', xfun::loadable('processx'))) {
      proc = processx::process$new(cmd, cmd_args, stderr = '|', cleanup_tree = TRUE)
      I(proc$get_pid())
    } else {
      bg_process(cmd, cmd_args)
    }
    opts$append(pids = list(pid))

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
    # server is correctly started so we record the directory served
    opts$append(served_dirs = root)
    message(
      'Launched the ', g, ' server in the background (process ID: ', pid, '). ',
      'To stop it, call blogdown::stop_server() or restart the R session.'
    )

    # whether to watch for changes in Rmd files?
    if (!getOption('blogdown.knit.on_save', TRUE)) return(invisible())

    rmd_files = NULL
    watch = servr:::watch_dir('.', rmd_pattern, handler = function(files) {
      rmd_files <<- files
    })
    watch_build = function() {
      # stop watching if stop_server() has cleared served_dirs
      if (is.null(opts$get('served_dirs'))) return(invisible())
      if (watch()) try(build_it(rmd_files))
      if (getOption('blogdown.knit.on_save', TRUE)) later::later(watch_build, intv)
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

# kill a process and all its child processes
proc_kill = function(pid, recursive = TRUE, ...) {
  run_cmd = base::system2
  if (is_windows()) {
    run_cmd('taskkill', c(if (recursive) '/t', '/f', '/pid', pid), ...)
  } else {
    run_cmd('kill', c(pid, if (recursive) child_pids(pid)), ...)
  }
}

# obtain pids of all child processes (recursively)
child_pids = function(id) {
  x = system2('sh', shQuote(c(pkg_file('scripts', 'child_pids.sh'), id)), stdout = TRUE)
  grep('^[0-9]+$', x, value = TRUE)
}

powershell = function(command) {
  if (Sys.which('powershell') == '') return()
  system2('powershell', c('-Command', shQuote(command)), stdout = TRUE)
}

#' @export
#' @rdname serve_site
stop_server = function() {
  ids = NULL  # collect pids that we failed to kill
  quitting = isTRUE(opts$get('quitting'))
  for (i in opts$get('pids')) {
    # no need to kill a process started by processx when R is quitting
    if (quitting && inherits(i, 'AsIs')) next
    if (proc_kill(i, stdout = FALSE, stderr = FALSE) != 0) ids = c(ids, i)
  }
  if (length(ids)) warning(
    'Failed to kill the process(es): ', paste(i, collapse = ' '),
    '. You may need to kill them manually.'
  ) else if (!quitting) message('The web server has been stopped.')
  opts$set(pids = NULL, served_dirs = NULL)
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

    get_pid = function(time) {
      # make sure the command points to an actual executable (e.g., resolve 'R'
      # to 'R.exe')
      if (!file.exists(command)) {
        if (Sys.which(command) != '') command = Sys.which(command)
      }
      cmd = basename(command)

      # use PowerShell to figure out the PID if possible:
      res = powershell(sprintf(
        'Get-CimInstance Win32_Process -Filter "name = \'%s\'" | select CommandLine, ProcessId | ConvertTo-Csv', cmd
      ))
      if (length(res) > 1) {
        res = read.csv(text = res, comment.char = '#', stringsAsFactors = FALSE)
        if (length(r1 <- res[, 'CommandLine']) && length(r2 <- res[, 'ProcessId'])) {
          cmd2 = paste(c(cmd, args), collapse = ' ')
          r2 = r2[grep(cmd2, r1, fixed = TRUE)]
          if (length(r2)) return(r2)
        }
      }

      # don't try this method until 1/5 of timeout has passed
      if (!is.null(res) && time < timeout/5) return()
      pid2 = setdiff(tasklist(), pid1)
      # the process's info should start with the command name
      pid2 = pid2[substr(pid2, 1, nchar(cmd)) == cmd]
      if (length(pid2) == 0) return()
      m = regexec('\\s+([0-9]+)\\s+', pid2)
      for (v in regmatches(pid2, m)) if (length(v) >= 2) return(v[2])
    }
  } else {
    pid = tempfile(); on.exit(unlink(pid), add = TRUE)
    code = paste(c(
      shQuote(c(command, args)),
      if (!getOption('xfun.bg_process.verbose', FALSE)) '> /dev/null',
      '& echo $! >', shQuote(pid)
    ), collapse = ' ')
    system2('sh', c('-c', shQuote(code)))
    get_pid = function(time) {
      if (file.exists(pid)) readLines(pid)
    }
  }

  t0 = Sys.time()
  while ((time <- difftime(Sys.time(), t0, units = 'secs')) < timeout) {
    if (length(id <- get_pid(time)) == 1) break
  }

  if (length(id) == 1) return(id)

  system2(command, args, timeout = timeout)  # see what the error is
  stop(
    'Failed to run the command in ', timeout, ' seconds (timeout): ',
    paste(shQuote(c(command, args)), collapse = ' ')
  )
}
