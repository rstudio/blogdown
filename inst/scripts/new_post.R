xfun::in_dir(blogdown:::site_root(), local({
  tags = htmltools::tags
  txt_input = function(..., width = '100%') shiny::textInput(..., width = width)
  sel_input = function(...) shiny::selectizeInput(
    ..., width = '98%', multiple = TRUE, options = list(create = TRUE)
  )
  meta = blogdown:::collect_yaml()
  lang = blogdown:::check_lang()
  adir = blogdown:::theme_flag()
  adir = if (length(adir) == 4) file.path(adir[2], adir[4], 'archetypes')
  adir = c('archetypes', adir)
  suff = ifelse(utils::file_test('-d', dir(adir, full.names = TRUE)), '/', '')
  shiny::runGadget(
    miniUI::miniPage(miniUI::miniContentPanel(
      txt_input('title', 'Title', placeholder = 'Post Title'),
      shiny::fillRow(
        txt_input('author', 'Author', blogdown:::get_author(), width = '98%'),
        shiny::dateInput('date', 'Date', Sys.Date(), width = '98%'),
        shiny::selectizeInput(
          'subdir', 'Subdirectory', blogdown:::get_subdirs(),
          selected = getOption('blogdown.subdir', 'post'),
          width = '98%', multiple = FALSE,
          options = list(create = TRUE, placeholder = '(optional)')
        ),
        height = '70px'
      ),
      shiny::fillRow(
        sel_input('cat', 'Categories', meta$categories),
        sel_input('tag', 'Tags', meta$tags),
        shiny::selectInput(
          'kind', 'Archetype', width = '98%',
          choices = unique(c('', xfun::sans_ext(paste0(dir(adir), suff))))
        ),
        height = '70px'
      ),
      shiny::fillRow(
        txt_input('file', 'Filename', '', 'automatically generated (edit if you want)'),
        height = '70px'
      ),
      if (is.null(lang)) {
        shiny::fillRow(txt_input('slug', 'Slug', '', '(optional)'), height = '70px')
      } else {
        shiny::fillRow(
          txt_input('slug', 'Slug', '', '(optional)', width = '98%'),
          txt_input('lang', 'Language', lang, width = '98%'),
          height = '70px'
        )
      },
      shiny::fillRow(
        shiny::radioButtons(
          'format', 'Format', inline = TRUE,
          c('Markdown' = '.md', 'R Markdown (.Rmd)' = '.Rmd', 'R Markdown (.Rmarkdown)' = '.Rmarkdown'),
          selected = getOption('blogdown.ext', '.md')
        ),
        height = '70px'
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output, session) {
      empty_title = shiny::reactive(grepl('^\\s*$', input$title))
      shiny::observe({
        shiny::updateTextInput(
          session, 'slug',
          placeholder = if (empty_title()) '(optional)' else blogdown:::dash_filename(input$title)
        )
      })
      shiny::observe({
        # update subdir in according to the title
        if (is.function(subdir_fun <- getOption('blogdown.subdir_fun'))) shiny::updateSelectizeInput(
          session, 'subdir', selected = subdir_fun(input$title)
        )
        # calculate file path
        if (grepl('^\\s*$', slug <- input$slug)) slug = blogdown:::dash_filename(input$title)
        shiny::updateTextInput(
          session, 'file', value = blogdown:::post_filename(
            slug, input$subdir, shiny::isolate(input$format), input$date, input$lang
          )
        )
      })
      shiny::observeEvent(input$format, {
        f = input$file
        if (f != '') shiny::updateTextInput(
          session, 'file', value = xfun::with_ext(f, input$format)
        )
      }, ignoreInit = TRUE)
      shiny::observeEvent(input$done, {
        if (grepl('^\\s*$', input$file)) return(
          warning('The filename is empty!', call. = FALSE)
        )
        if (is.null(getOption('blogdown.author'))) options(blogdown.author = input$author)
        blogdown::new_post(
          input$title, author = input$author, ext = input$format,
          categories = input$cat, tags = input$tag,
          file = gsub('[-[:space:]]+', '-', input$file),
          slug = if (input$slug != '') input$slug, subdir = input$subdir,
          date = input$date, kind = input$kind
        )
        shiny::stopApp()
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('New Post', height = 500)
  )
}))
