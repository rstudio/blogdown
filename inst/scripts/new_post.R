local({
  tags = htmltools::tags
  shiny::runGadget(
    miniUI::miniPage(
      miniUI::gadgetTitleBar('Create New Post'),
      shiny::textInput('title', 'Title'),
      shiny::textInput('author', 'Author'),
      shiny::checkboxInput('rmd', 'Use R Markdown')
    ),
    server = function(input, output) {
      shiny::observeEvent(input$done, {
        blogdown::new_post(input$title, author = input$author, rmd = input$rmd)
        shiny::stopApp()
      })
    },
    viewer = shiny::dialogViewer('New Post')
  )
})
