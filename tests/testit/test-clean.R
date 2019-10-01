library(testit)

assert('remove_extra_empty_lines() replaces two or more empty lines with one', {

  x0 = 'a line with some new lines.\n\n  and some text.'
  x1 = c('a line with some new lines.', '', '', '', '  and some text.')
  x2 = x1[-2]
  x3 = x1[-(2:3)]

  (remove_extra_empty_lines(x = x1) %==%  x0)
  (remove_extra_empty_lines(x = x2) %==%  x0)
  (remove_extra_empty_lines(x = x3) %==%  x0)

})


assert('process_bare_urls() replaces [url](url) with <url>', {

  x4 = '[url](url)'
  x5 = 'some text before [url](url) and after'

  (process_bare_urls(x = x4) %==% '<url>')
  (process_bare_urls(x = x5) %==% 'some text before <url> and after')

})


assert('normalize_chars() converts curly quotes to straight quotes', {

  x6 = intToUtf8(8216:8217)
  x7 = intToUtf8(8220:8221)
  x8 = intToUtf8(8230)
  x9 = intToUtf8(160)

  (normalize_chars(x = x6) %==% "''")
  (normalize_chars(x = x7) %==% '""')
  (normalize_chars(x = x8) %==% '...')
  (normalize_chars(x = x9) %==% ' ')

})


assert('remove_highlight_tags() cleans up code blocks syntax highlighted by Pandoc', {

  x10 = '    <code>some code</code>'
  x11 = '    <span>some span</span>'

  (remove_highlight_tags(x = x10) %==% '    some code')
  (remove_highlight_tags(x = x11) %==% '    some span')

})


assert('fix_img_tags() converts <img></img> to <img />', {

  x12 = '<img></img>'
  x13 = 'text before <img></img> and after'

  (fix_img_tags(x = x12) %==% '<img />')
  (fix_img_tags(x = x13) %==% 'text before <img /> and after')

})
