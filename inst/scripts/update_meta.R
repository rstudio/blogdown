local({
  tags = htmltools::tags
  txt_input = function(..., width = '100%') shiny::textInput(..., width = width)
  sel_input = function(...) shiny::selectizeInput(
    ..., width = '100%', multiple = TRUE, options = list(create = TRUE)
  )
  meta = blogdown:::collect_yaml()

  ctxt = rstudioapi::getSourceEditorContext(); txt = ctxt$contents
  res = blogdown:::split_yaml_body(txt); yml = res$yaml_list; rng = res$yaml_range
  if (length(yml) == 0) return(
    warning("The current document does not seem to contain YAML metadata", call. = FALSE)
  )
  rstudioapi::setSelectionRanges(list(c(rng[1] + 1, 1, rng[2], 1)))
  slct = rstudioapi::getSourceEditorContext()$selection[[1]]

  if (length(yml) == 0) yml = list()
  yml = blogdown:::filter_list(yml)
  if (is.null(yml[['title']])) yml$title = ''
  if (is.null(yml[['author']])) yml$author = getOption('blogdown.author', '')
  if (is.null(yml[['date']])) yml$date = Sys.Date()

  shiny::runGadget(
    miniUI::miniPage(miniUI::miniContentPanel(
      txt_input('title', 'Title', yml[['title']], placeholder = 'Post Title'),
      shiny::fillRow(
        txt_input('author', 'Author', yml[['author']], width = '99%'),
        shiny::dateInput('date', 'Date', yml[['date']], width = '99%'),
        height = '80px'
      ),
      shiny::checkboxInput(
        'rename', 'Rename file if the date is changed', getOption('blogdown.rename_file', FALSE)
      ),
      sel_input(
        'cat', 'Categories', blogdown:::sort2(unique(c(yml[['categories']], meta$categories))),
        selected = yml[['categories']]
      ),
      sel_input(
        'tag', 'Tags', blogdown:::sort2(unique(c(yml[['tags']], meta$tags))),
        selected = yml[['tags']]
      ),
      shiny::fillRow(tags$div(), height = '20px'),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output) {
      shiny::observeEvent(input$done, {
        seq_keys = Filter(function(key) {
          identical(attr(yml[[key]], 'yml_type'), 'seq')
        }, names(yml))
        seq_keys = unique(c(seq_keys, 'categories', 'tags'))

        res = list(
          title = input$title, author = input$author, date = format(input$date),
          categories = input$cat, tags = input$tag
        )
        yml = c(res, yml[setdiff(names(yml), names(res))])
        for (i in seq_keys) yml[[i]] = if (length(yml[[i]]) > 0) as.list(yml[[i]])
        if (!getOption('blogdown.yaml.empty', TRUE)) yml = blogdown:::filter_list(yml)
        rstudioapi::modifyRange(
          slct$range, blogdown:::as.yaml(yml, .trim_ws = FALSE)
        )
        if (input$rename) {
          rstudioapi::documentSave()
          p = ctxt$path; p2 = blogdown:::date_filename(p, res$date, replace = TRUE)
          b = if (basename(p) == basename(p2)) {
            file.rename(dirname(p), dirname(p2))
          } else file.rename(p, p2)
          if (b) rstudioapi::navigateToFile(p2)
        }
        shiny::stopApp()
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE,
    viewer = shiny::dialogViewer('Update YAML metadata', 500, 450)
  )

})
