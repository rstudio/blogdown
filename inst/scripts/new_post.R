tags = htmltools::tags
txt_input = function(..., width = '100%') shiny::textInput(..., width = width)
sel_input = function(...) shiny::selectizeInput(
  ..., width = '98%', multiple = TRUE, options = list(create = TRUE)
)
meta = blogdown:::collect_yaml()
lang = blogdown:::get_lang()
adir = blogdown:::theme_dir()
adir = if (length(adir)) file.path(adir, 'archetypes')
adir = c('archetypes', adir)
adir = dir(adir, full.names = TRUE)
adir = paste0(basename(adir), ifelse(utils::file_test('-d', adir), '/', ''))

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
        choices = unique(c('', adir))
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
    # update subdir in according to the title
    if (is.function(subdir_fun <- getOption('blogdown.subdir_fun'))) shiny::observe({
      sub2 = subdir_fun(input$title)
      shiny::updateSelectizeInput(session, 'subdir', selected = sub2, choices = unique(c(
        sub2, blogdown:::get_subdirs()
      )))
    })
    shiny::observe({
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
      options(blogdown.author = input$author)  # remember the author name
      blogdown::new_post(
        input$title, author = input$author, ext = input$format,
        categories = input$cat, tags = input$tag,
        file = gsub('[-[:space:]]+', '-', input$file),
        slug = if (input$slug != '') input$slug, subdir = input$subdir,
        date = input$date, kind = xfun::sans_ext(input$kind)
      )
      shiny::stopApp()
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  },
  stopOnCancel = FALSE, viewer = shiny::dialogViewer('New Post', height = 500)
)
