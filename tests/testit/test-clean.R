assert("# replace three or more \n with two, i.e. two or more empty lines with one", {

  x1 = "a line with some new lines.\n\n\n  and some text."
  x2 = "a line with some new lines.\n\n and some text."
  x3 = "a line with some new lines.\n and some text."

  (remove_inline_n(x1) %==%  "a line with some new lines.\n\n  and some text.")
  (remove_inline_n(x2) %==%  "a line with some new lines.\n\n and some text.")
  (remove_inline_n(x3) %==%  "a line with some new lines.\n and some text.")

})

assert("# replace [url](url) with <url>", {

  x4 = "[url](url)"
  x5 = "some text before [url](url) and after"

  (replace_inline_url(x4) %==% "<url>")
  (replace_inline_url(x5) %==% "some text before <url> and after")

})


assert("# curly single and double quotes to straight quotes", {

  x6 = "some text with a strange ‘ symbol"
  x7 = "some text with another strange ” symbool"
  x8 = "…"
  x9 = " "

  (replace_chars_inline(x6) %==% "some text with a strange ' symbol")
  (replace_chars_inline(x7) %==% 'some text with another strange " symbool')
  (replace_chars_inline(x8) %==% '...')
  (replace_chars_inline(x9) %==% ' ')

})


assert('clean up code blocks that have been syntax highlighted by Pandoc', {

  #not sure if empty space is needed because of
  #only process lines that are indented by at least 4 spaces in clean.R
  x10 = "    <code>some code</code>"
  x11 = "    <span>some span</span>"

  (remove_tags_inline(x10) %==% "    some code")
  (remove_tags_inline(x11) %==% "    some span")

})



assert('<img></img> to <img/>', {

  x12 = "<img></img>"
  x13 = "text before <img></img> and after"

  (replace_img_inline(x12) %==% "<img />") #is there a typo in the function comment <img/>
  (replace_img_inline(x13) %==% "text before <img /> and after")

})
