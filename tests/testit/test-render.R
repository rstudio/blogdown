library(testit)

assert('run_pandoc() find when Pandoc needs to convert', {
  (run_pandoc(c("nothing special", "requiring render")) %==% FALSE)
  (run_pandoc(c("```{=html}", "using raw block content", "```")) %==% TRUE)
  (run_pandoc(c("With inline raw `this works too`{=html}")) %==% TRUE)
  (run_pandoc(c("Using bib", "references:")) %==% TRUE)
  (run_pandoc(c("Using bib", "bibliography: test.bib")) %==% TRUE)
  opts = options(blogdown.markdown.format = "gfm")
  (run_pandoc(c("nothing special", "but option is set")) %==% TRUE)
  options(opts)
})

