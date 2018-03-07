local({
  txt_input = function(..., width = '100%') shiny::textInput(..., width = width)

  blogfile = rstudioapi::getSourceEditorContext()
  if (blogfile$path == '') stop('Please select the blog post source file before using this addin', call. = FALSE)

  blogpath = normalizePath(blogfile$path)
  imgdir = file.path(
    'static',
    dirname(gsub('.*content/', '', blogpath)),
    paste0(xfun::sans_ext(basename(blogpath)), '_files')
  )
  shiny::runGadget(
    miniUI::miniPage(miniUI::miniContentPanel(
      shiny::fillRow(
        shiny::fileInput('newimg', 'Image', placeholder = 'Select external image'),
        shiny::column(width = 6, offset = 2, shiny::uiOutput('overbutton')),
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
        txt_input('imgfinal', 'Target image file', '', '(optional) customize only if necessary. Autotomatically fills after choosing an image.'),
        height = '70px'
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output, session) {
      shiny::observeEvent(input$newimg, {
        shiny::updateTextInput(session, 'imgfinal', value = file.path(imgdir,
          input$newimg$name))
      })
      shiny::observeEvent(input$imgfinal, {
        if(file.exists(input$imgfinal)) {
          output$overbutton = shiny::renderUI({
            shiny::radioButtons(
              'imgoverwrite', 'Target image exists. Overwrite it?',
              inline = TRUE, c('Yes' = TRUE, 'No' = FALSE), selected = FALSE)
          })
        } else {
          ## Reset in case the user changes the target image name and
          ## the new target image doesn't exist
          output$overbutton = shiny::renderUI({})
        }
      })
      shiny::observeEvent(input$done, {
        if (is.null(input$newimg)) return(
          warning('You have to choose an image!', call. = FALSE)
        )
        if(file.exists(input$imgfinal)) {
          if(!as.logical(input$imgoverwrite)) {
            warning('The image already exists and you chose not to overwrite it! Linking to the previous version of the image.', call. = FALSE)
          }
        }
        imgfinaldir = dirname(input$imgfinal)
        dir.create(imgfinaldir, showWarnings = FALSE, recursive = TRUE)
        copy_check = file.copy(input$newimg$datapath, input$imgfinal,
          overwrite = ifelse(is.null(input$imgoverwrite), FALSE,
          as.logical(input$imgoverwrite)))
        if(copy_check) message(paste('successfully copied the image to', input$imgfinal))
        
        imgsrc = paste0("/", basename(dirname(imgfinaldir)), "/",
          basename(imgfinaldir), "/", basename(input$imgfinal))
                
        image_text = if(input$imgwidth == '' && input$imgheight == '') {
          paste0('![', input$imgalt, '](', imgsrc, ')')
        } else {
          shiny::img(src = imgsrc, alt = input$imgalt,
            width = ifelse(input$imgwidth == '', NULL, input$imgwidth),
            height = ifelse(input$imgheight == '', NULL, input$imgheight))
        }
        
        rstudioapi::insertText(as.character(image_text))
        shiny::stopApp()
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('Add external image to a blogdown post', height = 380)
  )
})
