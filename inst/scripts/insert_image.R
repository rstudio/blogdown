local({
  txt_input = function(..., width = '100%') shiny::textInput(..., width = width)

  blogfile = rstudioapi::getSourceEditorContext()
  if (blogfile$path == '') stop('Please select the blog post source file before using this addin', call. = FALSE)

  blogpath = normalizePath(blogfile$path)
  imgdir = file.path(
    gsub('/content/.*', '', blogpath),
    'static',
    dirname(gsub('.*content/', '', blogpath)),
    paste0(xfun::sans_ext(basename(blogpath)), '_files')
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
      shiny::fillRow(
        txt_input('imgdir', 'Target image location (customize only if necessary)', imgdir),
        height = '70px'
      ),
      shiny::fillRow(
        shiny::radioButtons(
          'imgoverwrite', 'Overwrite image?', inline = TRUE,
          c('Yes' = TRUE, 'No' = FALSE), selected = FALSE),
          height = '70px'
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output, session) {
      shiny::observeEvent(input$done, {
        if (is.null(input$newimg)) return(
          warning('You have to choose an image!', call. = FALSE)
        )
        imgfinaldir <- ifelse(input$imgdir == '', imgdir, input$imgdir)
        if(!as.logical(input$imgoverwrite)) {
            if(file.exists(file.path(imgfinaldir, input$newimg$name))) {
                warning('The image already exists and you chose not to overwrite it!', call. = FALSE)
            }
        }
        dir.create(imgfinaldir, showWarnings = FALSE, recursive = TRUE)
        copy_check = file.copy(input$newimg$datapath,
            file.path(imgfinaldir, input$newimg$name),
            overwrite = as.logical(input$imgoverwrite))
        if(copy_check) message(paste('successfully copied the image to', file.path(imgfinaldir, input$newimg$name)))
        
        imgsrc = paste0("/", basename(dirname(imgfinaldir)), "/",
            basename(imgfinaldir), "/", input$newimg$name)
                
        image_text = if(input$imgwidth == '' & input$imgheight == '') {
            paste0('![', input$imgalt, '](', imgsrc, ')')
        } else if (input$imgwidth == '') {
            shiny::img(src = imgsrc, alt = input$imgalt, 
                height = input$imgheight)
        } else if (input$imgheight == '') {
            shiny::img(src = imgsrc, alt = input$imgalt, 
                width = input$imgwidth) 
        } else {
            shiny::img(src = imgsrc, alt = input$imgalt,
                width = input$imgwidth, height = input$imgheight)
        }
        
        rstudioapi::insertText(as.character(image_text))
        shiny::stopApp()
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('Add external image to a blogdown post', height = 450)
  )
})
