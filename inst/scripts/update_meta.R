local({
  tags = htmltools::tags
  sel_input = function(...) shiny::selectizeInput(
    ..., width = '95%', multiple = TRUE, options = list(create = TRUE)
  )
  meta = blogdown:::scan_meta()
  slct = rstudioapi::getSourceEditorContext()$selection[[1]]
  x0 = if (slct$text != '') yaml::yaml.load(slct$text)
  x1 = x0[['categories']]; x2 = x0[['tags']]
  n1 = length(x1); n2 = length(x2)

  shiny::runGadget(
    miniUI::miniPage(miniUI::miniContentPanel(
      if (n1 > 0 || n2 == 0) sel_input(
        'cat', 'Categories', sort(unique(c(x1, meta$categories))), selected = x1
      ),
      if (n2 > 0 || n1 == 0) sel_input(
        'tag', 'Tags', sort(unique(c(x2, meta$tags))), selected = x2
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output) {
      shiny::observeEvent(input$done, {
        res = list(categories = input$cat, tags = input$tag)
        for (i in names(res)) res[[i]] = if (length(res[[i]]) > 0) as.list(res[[i]])
        if (length(res)) rstudioapi::modifyRange(
          slct$range, yaml::as.yaml(res, indent.mapping.sequence = TRUE)
        )
        shiny::stopApp()
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('Update Categories/Tags', height = 100)
  )

})
