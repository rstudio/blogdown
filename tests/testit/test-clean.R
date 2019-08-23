remove_extra_empty_lines = function(x) {
  x = paste(gsub('\\s+$', '', x), collapse = '\n')
  trim_ws(gsub('\n{3,}', '\n\n', x))
}

assert("# replace three or more \n with two, i.e. two or more empty lines with one", {

  x1 = "a line with some new lines.\n\n\n"
  x2 = "a line with some new lines.\n\n"
  x3 = "a line with some new lines.\n"

  #these seem to remove all new lines?
  (remove_extra_empty_lines(x1) %==%  "a line with some new lines.")
  (remove_extra_empty_lines(x2) %==%  "a line with some new lines.")
  (remove_extra_empty_lines(x3) %==%  "a line with some new lines.")

})


process_bare_urls =  function(x) {
  gsub('\\[([^]]+)]\\(\\1/?\\)', '<\\1>', x)
}

assert("# replace [url](url) with <url>", {

  x4 = "[url](url)"
  x5 = "some text before [url](url) and after"
  process_bare_urls(x4) %==% "<url>"
  process_bare_urls(x5) %==% "some text before <url> and after"
})


normalize_chars = function(x) {
  # curly single and double quotes to straight quotes
  x = gsub(paste0('[', intToUtf8(8216:8217), ']'), "'", x)
  x = gsub(paste0('[', intToUtf8(8220:8221), ']'), '"', x)
  x = gsub(intToUtf8(8230), '...', x)  # ellipses
  x = gsub(intToUtf8(160), ' ', x)  # zero-width space
  x
}

assert("# curly single and double quotes to straight quotes", {

  x6 = paste0('[', intToUtf8(8216:8217), ']')
  x7 = paste0('[', intToUtf8(8220:8221), ']')
  x8 = intToUtf8(8230)
  x9 = intToUtf8(160)
  normalize_chars(x6) %==% "'"
  normalize_chars(x7) %==% '"'
  normalize_chars(x8) %==% '...'
  normalize_chars(x9) %==% ' '
})


clean = function(x) {
  # remove the <code></code> tags
  x = gsub('^(\\s+)<code( class="[^"]*")?>(.*)', '\\1\\3', x)
  x = gsub('</code>\\s*$', '', x)
  # remove <span></span>
  x = gsub('</?span([^>])*>', '', x)
  x
}

assert('clean up code blocks that have been syntax highlighted by Pandoc', {

  #not sure if empty space is needed because of
  #only process lines that are indented by at least 4 spaces in clean.R
  x10 = "    <code>some code</code>"
  x11 = "<span>some span</span>"

  (clean(x10) %==% "    some code")
  (clean(x11) %==% "some span")
})


fix_img_tags =  function(x) {
  gsub('></img>', ' />', x)
}

assert('<img></img> to <img/>', {

  x12 = "<img></img>"
  x13 = "text before <img></img> and after"
  (fix_img_tags(x12) %==% "<img />") #is there a typo in the function comment <img/>
  (fix_img_tags(x13) %==% "text before <img /> and after")

})
