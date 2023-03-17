# blogdown <a href="https://pkgs.rstudio.com/blogdown/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/rstudio/blogdown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/blogdown/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/blogdown)](https://CRAN.R-project.org/package=blogdown)
[![Codecov test coverage](https://codecov.io/gh/rstudio/blogdown/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rstudio/blogdown?branch=main)
<!-- badges: end -->

The goal of the blogdown package is to provide a powerful and customizable website output format for [R Markdown](https://rmarkdown.rstudio.com/). Use dynamic R Markdown documents to build webpages featuring:

+ R code (or other programming languages that [knitr](https://yihui.org/knitr/) supports),

+ automatically rendered output such as graphics, tables, analysis results, and HTML widgets, and

+ technical writing elements such as citations, footnotes, and LaTeX math, enabled by the [bookdown package](https://pkgs.rstudio.com/bookdown/).

By default, blogdown uses [Hugo](https://gohugo.io), a popular open-source static website generator, which provides a fast and flexible way to build your site content to be shared online. Other website generators like Jekyll and Hexo are also supported.

A useful feature of blogdown sites, compared to other R Markdown-based [websites](https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html), is that you may organize your website content (including R Markdown files) within subdirectories. This makes blogdown a good solution not just for blogging or sites about R &mdash; it can also be used to create general-purpose websites to communicate about data science, statistics, data visualization, programming, or education.

## Book

<a href="https://bookdown.org/yihui/blogdown/"><img class="book" src="https://bookdown.org/yihui/blogdown/images/cover.png" alt = "blogdown: Creating Websites with R Markdown" height="400"></a>

## Installation

You can install the package via CRAN as follows:

```r
install.packages('blogdown')
```

If you want to use the development version of the **blogdown** package, you can install the package from GitHub via the [**remotes** package](https://remotes.r-lib.org):

```r
remotes::install_github('rstudio/blogdown')
```
## Usage

You may create a new site via the function `blogdown::new_site()` under an _empty_ directory. It will create a skeleton site, download a Hugo theme from Github,  add some sample content, launch a web browser and you will see the new site. The sample blog post `hello-world.Rmd` should be opened automatically, and you can edit it. The website will be automatically rebuilt and the page will be refreshed after you save the file.

If you use RStudio, you can create a new RStudio project for your website from the menu `File -> New Project -> New Directory -> Website using blogdown`.

The function `blogdown::serve_site()` may be the most frequently used function in this package. It builds the website, loads it into your web browser, and automatically refreshes the browser when you update the Markdown or R Markdown files. Do not use the command line `hugo server` to build or serve the site. It only understands plain Markdown files, and cannot build R Markdown.

You may not be satisfied with the default site created from `new_site()`. There are two things you may want to do after your first successful experiment with **blogdown**:

1. Pick a Hugo theme that you like from https://themes.gohugo.io. All you need is its Github user and repository name, to be passed to the `theme` argument of `new_site()`.
2. Add more content (pages or posts), or migrate your existing website.

## Getting help

There are two main places to get help:

1. The [RStudio community](https://community.rstudio.com/tags/c/R-Markdown/10/blogdown) is a friendly place to ask any questions about **blogdown**. Be sure to use the `blogdown` tag.

1. [Stack Overflow](https://stackoverflow.com/questions/tagged/blogdown) is a great source of answers to common **blogdown** questions. Use the tags [`[r][blogdown]`](https://stackoverflow.com/questions/tagged/blogdown+r) if you ask a question.

## Code of Conduct

Please note that the blogdown project is released with a [Contributor Code of Conduct](https://pkgs.rstudio.com/blogdown/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
