local({
  txt_input = function(..., width = '100%') shiny::textInput(..., width = width)

  blogfile = rstudioapi::getActiveDocumentContext()
  if(blogfile$path == '') stop('Please select the blog post source file before using this addin', call. = FALSE)

  imgdir = file.path(
    gsub('/content/.*', '', normalizePath(blogfile$path)),
    'static',
    dirname(gsub('.*content/', '', normalizePath(blogfile$path))),
    paste0(xfun::sans_ext(basename(blogfile$path)), '_files')
  )

  shiny::runGadget(
    miniUI::miniPage(miniUI::miniContentPanel(
      shiny::fillRow(
        shiny::fileInput('newimg', 'Image', placeholder = 'Select external image'),
        height = '90px'
      ),
      shiny::fillRow(
        txt_input('imgwidth', 'Image width', '', '(optional) Example: 400 for 400px'),
        txt_input('imgheight', 'Image height', '', '(optional) Example: 200 for 200px'),
        height = '70px'),
      shiny::fillRow(
        txt_input('imgalt', 'Image alternative text', '', '(optional) Example: awesome screenshot'),
        height = '70px'
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output, session) {
      shiny::observeEvent(input$done, {
        if (is.null(input$newimg)) return(
          warning('You have to choose an image!', call. = FALSE)
        )
        dir.create(imgdir, showWarnings = FALSE)
        file.copy(input$newimg$datapath, file.path(imgdir, input$newimg$name))
        if(file.exists(file.path(imgdir, input$newimg$name))) message(paste('successfully copied the image to', file.path(imgdir, input$newimg$name)))
        
        imgsrc = paste0(
          "/",
          basename(dirname(imgdir)),
          "/",
          basename(imgdir),
          "/",
          input$newimg$name
        )
                
        image_text = if(input$imgwidth == '' & input$imgheight == '') shiny::img(src = imgsrc) else if (input$imgwidth == '') shiny::img(src = imgsrc, height = input$imgheight) else if(input$imgheight == '') shiny::img(src = imgsrc, width = input$imgwidth) else shiny::img(src = imgsrc, width = input$imgwidth, height = input$imgheight)
        
        rstudioapi::insertText(as.character(image_text))
        shiny::stopApp()
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('Add external image to a blogdown post', height = 260)
  )
})
