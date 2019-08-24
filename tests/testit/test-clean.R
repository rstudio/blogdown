assert("# replace three or more \n with two, i.e. two or more empty lines with one", {

  x1 = "a line with some new lines.\n\n\n"
  x2 = "a line with some new lines.\n\n"
  x3 = "a line with some new lines.\n"

  (remove_inline_n(x1) %==%  "a line with some new lines.\n\n\n")
  (remove_inline_n(x2) %==%  "a line with some new lines.\n\n")
  (remove_inline_n(x3) %==%  "a line with some new lines.\n")

})

assert("# replace [url](url) with <url>", {

  x4 = "[url](url)"
  x5 = "some text before [url](url) and after"

  (replace_inline_url(x4) %==% "<url>")
  (replace_inline_url(x5) %==% "some text before <url> and after")

})


assert("# curly single and double quotes to straight quotes", {

  x6 = paste0('[', intToUtf8(8216:8217), ']')
  x7 = paste0('[', intToUtf8(8220:8221), ']')
  x8 = intToUtf8(8230)
  x9 = intToUtf8(160)

  (replace_chars_inline(x6) %==% "'")
  (replace_chars_inline(x7) %==% '"')
  (replace_chars_inline(x8) %==% '...')
  (replace_chars_inline(x9) %==% ' ')

})


assert('clean up code blocks that have been syntax highlighted by Pandoc', {

  #not sure if empty space is needed because of
  #only process lines that are indented by at least 4 spaces in clean.R
  x10 = "    <code>some code</code>"
  x11 = "<span>some span</span>"

  (remove_tags_inline(x10) %==% "    some code")
  (remove_tags_inline(x11) %==% "some span")

})



assert('<img></img> to <img/>', {

  x12 = "<img></img>"
  x13 = "text before <img></img> and after"

  (replace_img_inline(x12) %==% "<img />") #is there a typo in the function comment <img/>
  (replace_img_inline(x13) %==% "text before <img /> and after")

})
