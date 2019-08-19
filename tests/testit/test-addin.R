assert('quote_poem() add > to the beginning of every paragraph, and two trailing spaces to every line', {


  (quote_poem("               ")  %==% "               ")

  (quote_poem("some text.") %==% '> some text.')

  (quote_poem("some text.\nsome more text.\neven more text.")) %==%
    "> some text.  \nsome more text.  \neven more text."

  (quote_poem("some text.    \nsome more text.     \neven more text.")) %==%
    "> some text.  \nsome more text.  \neven more text."

  (quote_poem("some text.    \nsome more text.     \n\neven more text.")) %==%
    "> some text.  \nsome more text.\n\n> even more text."

  # Note: quote_poem() doesn't add two spaces to the last line
  # of each paragraph. I am not sure if this is by design.
  # The following will not pass the test.
  (quote_poem("some text.    \nsome more text.     \neven more text.")) %==%
    "> some text.  \nsome more text.  \neven more text.  "

})
