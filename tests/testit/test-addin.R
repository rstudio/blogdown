assert('quote_poem() adds > to the beginning of every paragraph, and two trailing spaces to every line', {

  (quote_poem("               ")  %==% "               ")

  (quote_poem("some text.") %==% '> some text.')

  (quote_poem("some text.\nsome more text.\neven more text.")) %==%
    "> some text.  \nsome more text.  \neven more text."

  (quote_poem("some text.    \nsome more text.     \neven more text.")) %==%
    "> some text.  \nsome more text.  \neven more text."

  (quote_poem("some text.    \nsome more text.     \n\neven more text.")) %==%
    "> some text.  \nsome more text.\n\n> even more text."

})
