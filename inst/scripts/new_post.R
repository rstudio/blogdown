local({
  tags = htmltools::tags
  txt_input = function(..., width = '100%') shiny::textInput(..., width = width)
  sel_input = function(...) shiny::selectizeInput(
    ..., width = '95%', multiple = TRUE, options = list(create = TRUE)
  )
  meta = blogdown:::scan_meta()
  shiny::runGadget(
    miniUI::miniPage(miniUI::miniContentPanel(
      txt_input('title', 'Title', placeholder = 'Post Title'),
      shiny::fillRow(
        txt_input('author', 'Author', getOption('blogdown.author', ''), width = '95%'),
        shiny::dateInput('date', 'Date', Sys.Date(), width = '95%'),
        txt_input(
          'subdir', 'Subdirectory', getOption('blogdown.subdir', ''),
          '(optional)', width = '95%'
        ),
        height = '70px'
      ),
      shiny::fillRow(
        sel_input('cat', 'Categories', meta$categories),
        sel_input('tag', 'Tags', meta$tags),
        height = '70px'
      ),
      shiny::fillRow(
        txt_input('file', 'Filename', '', 'automatically generated (edit if you want)'),
        height = '70px'
      ),
      shiny::fillRow(
        shiny::radioButtons(
          'format', 'Format', c('Markdown', 'R Markdown'), inline = TRUE,
          selected = ifelse(getOption('blogdown.use.rmd', FALSE), 'R Markdown', 'Markdown')
        ),
        height = '70px'
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output, session) {
      empty_title = shiny::reactive(grepl('^\\s*$', input$title))
      shiny::observe({
        if (!empty_title()) shiny::updateTextInput(
          session, 'file', value = blogdown:::post_filename(
            input$title, NULL, input$subdir, input$format == 'R Markdown', input$date
          )
        )
      })
      shiny::observeEvent(input$done, {
        if (empty_title()) return(
          warning('The post title should not be empty!', call. = FALSE)
        )
        blogdown::new_post(
          input$title, author = input$author, rmd = input$format == 'R Markdown',
          categories = input$cat, tags = input$tag, file = input$file,
          subdir = input$subdir, date = input$date
        )
        shiny::stopApp()
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('New Post', height = 430)
  )
})
