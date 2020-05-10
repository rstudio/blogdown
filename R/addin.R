new_post_addin = function() {
  sys.source(pkg_file('scripts', 'new_post.R'))
}

update_meta_addin = function() {
  sys.source(pkg_file('scripts', 'update_meta.R'))
}

insert_image_addin = function() {
  sys.source(pkg_file('scripts', 'insert_image.R'))
}

# use touch to update the timestamp of a file if available (not on Windows);
# otherwise modify a file, undo it, and save it
touch_file_rstudio = function() {
  ctx = rstudioapi::getSourceEditorContext()
  if (!file.exists(ctx$path)) stop('The current document has not been saved yet.')
  p = normalizePath(ctx$path); mtime = function() file.info(p)[, 'mtime']
  m = mtime()
  on.exit(if (!identical(m, m2 <- mtime())) message(
    'The modification time of "', p, '" has been updated from ', m, ' to ', m2
  ), add = TRUE)
  touch_file(p)
}

touch_file = function(path, time = Sys.time()) Sys.setFileTime(path, time)

# add > to the beginning of every paragraph, and two trailing spaces to every line
quote_poem = function(x) {
  x = paste(x, collapse = '\n')
  if (grepl('^\\s*$', x)) return(x)
  x = gsub(' *\n', '  \n', x)
  x = gsub('( *\n){2,}', '\n\n> ', x)
  paste('>', gsub(' *(\n*) *$', '\\1', x))
}

quote_poem_addin = function() {
  ctx = rstudioapi::getSourceEditorContext()
  sel = ctx$selection[[1]]
  if (sel$text == '') {
    message('Please select some text in the editor first.')
    return()
  }
  rstudioapi::modifyRange(sel$range, quote_poem(sel$text))
}

grab_clipboard = function(filepath) {
  # platform = Sys.info()[1]
  if (is_osx()) {
    script = paste0(
      "osascript -e \'
        set theFile to (open for access POSIX file \"",
      filepath, "\" with write permission)
        try
          write (the clipboard as \u00abclass PNGf\u00bb) to theFile
        end try
        close access theFile'"
    )
    system(script)
  } else if (is_windows()) {
    script = paste0(
      "powershell -sta \"\n",
      "Add-Type -AssemblyName System.Windows.Forms;\n",
      "if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {\n",
      "  [System.Drawing.Bitmap][System.Windows.Forms.Clipboard]::GetDataObject(
           ).getimage().Save('",
      paste0(filepath, "', [System.Drawing.Imaging.ImageFormat]::Png) \n"),
      "  }\""
    )
    system(script)
  } else if (is_linux()) {
    # Executing on Linux! -> use xclip
    tryCatch(
      targets = tolower(
        system("xclip -selection clipboard -t TARGETS -o", intern = T)
      ),
      error = function(e) {
        stop("Please install the required system dependency xclip")
      }
    ) # Validate xclip is installed and get targets from clipboard
    if (any(grepl(".*png$", targets))) {
      system(paste0("xclip -selection clipboard -t image/png -o > ", filepath))
    }
  }

  # in mac os, if no image in clipboard, exec script will create a empty image
  # in window, no image be create
  if (!file.exists(filepath) || file.size(filepath) == 0) {
    stop("Clipboard data is not an image.")
  }
}

is_blogdown_post = function() {
  # current rmd is a blogdown post?
  # Criteria:
  # - is a project and .Rproj have attr like BuildType: Website
  # - filepath like **/post/***

  proj_root = rstudioapi::getActiveProject()
  if (is.null(proj_root)) {
    return(FALSE)
  }

  proj_settings = list.files(proj_root, pattern = ".Rproj", full.names = TRUE)
  currpath = rstudioapi::getSourceEditorContext()$path
  if (any(grep("BuildType: Website", readLines(proj_settings)) > 0) &&
    basename(dirname(currpath)) == "post") {
    return(TRUE)
  }

  FALSE
}

generate_filepath = function() {
  #' @return
  # list of filepath and filepath_insert
  #   filepath: absolute path, to save image in clipboard
  #   filepath_insert: path in rmd code, ![](filepath_insert)
  # 
  # for a blogdown post, filepath_insert is different from filepath
  # lcolladotor.github.io/2018/03/07/blogdown-insert-image-addin/#.XrZ9dxMzbjA
  # 
  # for a generic rmd, filepath_insert is same with filepath, while filepath_insert is relative path
  

  filename = format(Sys.time(), "rmd-img-paste-%Y%m%d%H%M%s.png")
  currpath = rstudioapi::getSourceEditorContext()$path
  if(!nchar(currpath)) stop("Please save the file before pasting an image.")
  currpath = xfun::normalize_path(currpath)

  if (is_blogdown_post()) {
    proj_root = rstudioapi::getActiveProject()
    
    post_files = file.path(
      dirname(gsub(".*content/", "", currpath)),
      paste0(tools::file_path_sans_ext(basename(currpath)), "_files")
    )
    dir = file.path(proj_root, "static", post_files)
    
    # path like /post/..., insert to md
    baseurl = ifelse(
      getOption("blogdown.insertimage.usebaseurl", FALSE),
      blogdown:::load_config()$baseurl, ""
    )
    dir_insert = file.path(baseurl, post_files)
    
  } else {
    dir = file.path(dirname(currpath), ".asserts")
    dir_insert = ".asserts"
  }
  if (!file.exists(dir)) dir.create(dir, recursive = TRUE)

  list(
    filepath = file.path(dir, filename),
    filepath_insert = file.path(dir_insert, filename)
  )
}

insert_image_from_clipboard_addin = function() {
  doc_id = rstudioapi::getSourceEditorContext()$id
  if (doc_id %in% c("#console", "#terminal")) {
    stop("You can`t insert an image in the console nor in the terminal.
         Please select a line in the source editor.")
  }
  res = generate_filepath()
  grab_clipboard(res$filepath)
  position = rstudioapi::getSourceEditorContext()$selection[[1]]$range$start
  func = function(filepath) paste0("![](", filepath, ")")
  rstudioapi::insertText(position, func(res$filepath_insert), id = doc_id)
}
