# blogdown

[![Build Status](https://travis-ci.org/rstudio/blogdown.svg)](https://travis-ci.org/rstudio/blogdown)

A open-source (GPL-3) R package to generate static websites based on [R Markdown](http://rmarkdown.rstudio.com) and [Hugo](https://gohugo.io). You can install the package via:

```r
devtools::install_github('rstudio/blogdown')
```

If you are not familiar with Hugo and do not want to read the Hugo [installation instructions](https://gohugo.io/overview/installing/), you may use the helper function `blogdown::install_hugo()` to install Hugo.

Once you have installed Hugo, you may create a new site via `blogdown::new_site()` under an _empty_ directory. It will create a skeleton site, download a Hugo theme from Github,  add some sample content, launch a web browser and you will see the new site. The sample blog post `hello-world.Rmd` should be opened automatically, and you can edit it. The website will be automatically rebuilt and the page will be refreshed after you save the file.

If you use RStudio, you can create a new RStudio project for your website, and generate the new site in the project (the new project will contain a `*.Rproj` file but that is fine).

You may not be satisfied with the default site created from `new_site()`. There are two things you may want to do after your first successful experiment with **blogdown**:

1. Pick a Hugo theme that you like from http://themes.gohugo.io. All you need is its Github user and repository name. For example, if you like the [Ghostwriter theme](https://github.com/jbub/ghostwriter), you can install it via `blogdown::install_theme('jbub/ghostwriter')`.
2. Add more content (pages or posts), or migrate your existing website.

The full documentation of this package is still under development. If you do not want to wait, you can read the Hugo documentation, which can be a little overwhelming if you have no experience with static site generators. Note although Hugo supports TOML, YAML, and JSON in the front matter of Markdown files, R Markdown only supports YAML, so you have to use YAML in `*.Rmd` files. Feel free to use other formats in plain Markdown files (`*.md`).

You are welcome to send us feedback using [Github issues](https://github.com/rstudio/blogdown/issues) or ask questions on [StackOverflow](http://stackoverflow.com/questions/tagged/blogdown) with the `blogdown` tag. Please note this package is still in a very early beta stage.
