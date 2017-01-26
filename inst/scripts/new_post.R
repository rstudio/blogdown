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
        sel_input('cat', 'Categories', meta$categories),
        sel_input('tag', 'Tags', meta$tags),
        height = '60px'
      ),
      shiny::radioButtons(
        'format', 'Format', c('Markdown', 'R Markdown'), inline = TRUE,
        selected = ifelse(getOption('blogdown.use.rmd', FALSE), 'R Markdown', 'Markdown')
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output) {
      shiny::observeEvent(input$done, {
        if (grepl('^\\s*$', input$title)) return(
          warning('The post title should not be empty!', call. = FALSE)
        )
        blogdown::new_post(
          input$title, author = input$author, rmd = input$format == 'R Markdown',
          categories = input$cat, tags = input$tag
        )
        shiny::stopApp()
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('New Post', height = 200)
  )
})
