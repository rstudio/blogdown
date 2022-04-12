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
#' @param .site_dir Directory to search for site configuration file. It defaults
#'   to \code{getwd()}, and can also be specified via the global option
#'   \code{blogdown.site_root}.
#' @note For the Hugo server, the argument \command{--navigateToChanged} is used
#'   by default, which means when you edit and save a source file, Hugo will
#'   automatically navigate the web browser to the page corresponding to this
#'   source file (if the page exists). However, due to a Hugo bug
#'   (\url{https://github.com/gohugoio/hugo/issues/3811}), this automatic
#'   navigation may not always work for R Markdown posts, and you may have to
#'   manually refresh your browser. It should work reliably for pure Markdown
#'   posts, though.
#' @export
serve_site = function(..., .site_dir = NULL) {
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
  serve(..., .site_dir = .site_dir)
}

server_ready = function(url) {
  # for some reason, R cannot read localhost, but 127.0.0.1 works
  url = sub('^http://localhost:', 'http://127.0.0.1:', url)
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
    # open some files initially if specified
    init_files = get_option('blogdown.initial_files')
    if (is.function(init_files)) init_files = init_files()
    for (f in init_files) if (file_exists(f)) open_file(f)
  } else {
    opts$set(knitting = TRUE)
    on.exit(refresh_viewer(), add = TRUE)
  }
  invisible(serve_site(...))
}

preview_mode = function() {
  isTRUE(opts$get('preview')) || isTRUE(opts$get('knitting'))
}

serve_it = function(pdir = publish_dir(), baseurl = site_base_dir()) {
  g = generator(); config = config_files(g)
  function(..., .site_dir = NULL) {
    root = site_root(config, .site_dir)
    if (root %in% opts$get('served_dirs')) {
      if (preview_mode()) return()
      servr::browse_last()
      return(message(
        'The site has been served under the directory "', root, '". I have tried ',
        'to reopen it for you with servr::browse_last(). If you do want to ',
        'start a new server, you may stop existing servers with ',
        'blogdown::stop_server(), or restart R. Normally you should not need to ',
        'serve the same site multiple times in the same R session',
        if (is_rstudio()) c(
          ', otherwise you may run into issues like ',
          'https://github.com/rstudio/blogdown/issues/404'
        ), '.'
      ))
    }

    owd = setwd(root); on.exit(setwd(owd), add = TRUE)

    server = servr::server_config(..., baseurl = baseurl, hosturl = function(host) {
      if (g == 'hugo' && host == '127.0.0.1') 'localhost' else host
    })

    # launch the hugo/jekyll/hexo server
    cmd = if (g == 'hugo') find_hugo() else g
    host = server$host; port = server$port; intv = server$interval
    if (!servr:::port_available(port, host)) stop(
      'The port ', port, ' at ', host, ' is unavailable', call. = FALSE
    )
    args_fun = match.fun(paste0(g, '_server_args'))
    cmd_args = args_fun(host, port)
    if (g == 'hugo') {
      # RStudio Server uses a proxy like http://localhost:8787/p/56a946ed/ for
      # http://localhost:4321, so we must use relativeURLs = TRUE:
      # https://github.com/rstudio/blogdown/issues/124
      tweak_hugo_env(server = TRUE, relativeURLs = if (is_rstudio_server()) TRUE)
      if (length(list_rmds(pattern = bundle_regex('.R(md|markdown)$'))))
        create_shortcode('postref.html', 'blogdown/postref')
    }

    # run a function (if configured) before starting the server
    if (is.function(serve_first <- getOption('blogdown.server.first'))) serve_first()

    # if requested not to demonize the server, run it in the foreground process,
    # which will block the R session
    if (!server$daemon) return(system2(cmd, cmd_args))

    pid = if (server_processx()) {
      proc = processx::process$new(cmd, cmd_args, stderr = '|', cleanup_tree = TRUE)
      I(proc$get_pid())
    } else {
      xfun::bg_process(cmd, cmd_args)
    }
    opts$append(pids = list(pid))

    message(
      'Launching the server via the command:\n  ',
      paste(c(cmd, cmd_args), collapse = ' ')
    )
    i = 0
    repeat {
      Sys.sleep(1)
      # for a process started with processx, check if it has died with an error
      if (inherits(pid, 'AsIs') && !proc$is_alive()) {
        err = paste(gsub('^Error: ', '', proc$read_error()), collapse = '\n')
        stop(if (err == '') {
          'Failed to serve the site; see if blogdown::build_site() gives more info.'
        } else err, call. = FALSE)
      }
      if (server_ready(server$url)) break
      if (i >= get_option('blogdown.server.timeout', 30)) {
        s = proc_kill(pid)  # if s == 0, the server must have been started successfully
        stop(if (s == 0) c(
          'Failed to launch the site preview in ', i, ' seconds. Try to give ',
          'it more time via the global option "blogdown.server.timeout", e.g., ',
          'options(blogdown.server.timeout = 600).'
        ) else c(
          'It took more than ', i, ' seconds to launch the server. An error might ',
          'have occurred with ', g, '. You may run blogdown::build_site() and see ',
          'if it gives more info.'
        ), call. = FALSE)
      }
      i = i + 1
    }
    server$browse()
    # server is correctly started so we record the directory served
    opts$append(served_dirs = root)
    Sys.setenv(BLOGDOWN_SERVING_DIR = root)
    message(
      'Launched the ', g, ' server in the background (process ID: ', pid, '). ',
      'To stop it, call blogdown::stop_server() or restart the R session.'
    )

    # delete the resources/ dir if it is empty
    if (g == 'hugo') del_empty_dir('resources')

    # whether to watch for changes in Rmd files?
    if (!get_option('blogdown.knit.on_save', TRUE)) return(invisible())

    # rebuild specific or changed Rmd files
    rebuild = function(files) {
      if (is.null(b <- get_option('blogdown.knit.on_save'))) {
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
      xfun::in_dir(root, build_site(TRUE, run_hugo = FALSE, build_rmd = files))
    }

    # build Rmd files that are new and don't have corresponding output files
    rebuild(rmd_files <- filter_newfile(list_rmds()))

    watch = servr:::watch_dir('.', rmd_pattern, handler = function(files) {
      files = list_rmds(files = files)
      # ignore Rmd files in the public/ directory, in case users forgot to set
      # ignoreFiles in config.yaml and Rmd files would be copied to public/
      # (they should not be): https://github.com/rstudio/blogdown/issues/610
      i = if (g == 'hugo') !xfun::is_sub_path(files, rel_path(publish_dir())) else TRUE
      rmd_files <<- files[i]
    })
    watch_build = function() {
      # stop watching if stop_server() has cleared served_dirs
      if (is.null(opts$get('served_dirs'))) return(invisible())
      if (watch()) try({rebuild(rmd_files); refresh_viewer()})
      if (get_option('blogdown.knit.on_save', TRUE)) later::later(watch_build, intv)
    }
    watch_build()

    return(invisible())
  }
}

server_processx = function() {
  v = get_option('blogdown.server.verbose', FALSE)
  # to see verbose output, don't use processx but xfun::bg_process() instead;
  # TODO: we may use the polling method in #555 to have processx output, too
  if (v) {
    options(xfun.bg_process.verbose = TRUE)
    return(FALSE)
  }
  getOption('blogdown.use.processx', xfun::loadable('processx'))
}

jekyll_server_args = function(host, port) {
  c('serve', '--port', port, '--host', host, get_option(
    'blogdown.jekyll.server', c('--watch', '--incremental', '--livereload')
  ))
}

hexo_server_args = function(host, port) {
  c('server', '-p', port, '-i', host, get_option('blogdown.hexo.server'))
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
  set_envvar(c('BLOGDOWN_SERVING_DIR' = NA))
  opts$set(pids = NULL, served_dirs = NULL)
}

get_config2 = function(key, default) {
  res = yaml_load_file('_config.yml')
  res[[key]] %n% default
}

# refresh the viewer because hugo's livereload doesn't work on RStudio
# Server: https://github.com/rstudio/rstudio/issues/8096 (TODO: check if
# it's fixed in the future: https://github.com/gohugoio/hugo/pull/6698)
refresh_viewer = function() {
  if (!is_rstudio_server()) return()
  server_wait()
  rstudioapi::executeCommand('viewerRefresh')
}

server_wait = function() {
  Sys.sleep(get_option('blogdown.server.wait', 2))
}
