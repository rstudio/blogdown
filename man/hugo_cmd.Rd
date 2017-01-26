% Please edit documentation in R/hugo.R
\name{hugo_cmd}
\alias{hugo_cmd}
\alias{new_content}
\alias{new_post}
\alias{new_site}
\title{Run Hugo commands}
\usage{
hugo_cmd(...)

new_site(dir = ".", install_hugo = TRUE, format = "toml", 
    sample = TRUE, theme = "yihui/hugo-lithium-theme", theme_example = TRUE, 
    serve = TRUE)

new_content(path, kind = "default", open = interactive())

new_post(title, kind = "default", open = interactive(), 
    author = getOption("blogdown.author"), categories = NULL, 
    tags = NULL, date = Sys.Date(), file = NULL, subdir = getOption("blogdown.subdir"), 
    rmd = getOption("blogdown.use.rmd", FALSE))
}
\arguments{
\item{...}{Arguments to be passed to \code{system2('hugo', ...)}, e.g.
\code{new_content(path)} is basically \code{hugo_cmd(c('new', path))} (i.e.
run the command \command{hugo new path}).}

\item{dir}{The directory of the new site. It should be empty or only contain
hidden files and RStudio project (\file{*.Rproj}) files.}

\item{install_hugo}{Whether to install Hugo automatically if it is not found.}

\item{format}{The format of the configuration file. Note that the frontmatter
of the new (R) Markdown file created by \code{new_content()} always uses
YAML instead of TOML.}

\item{sample}{Whether to add sample content. Hugo creates an empty site by
default, but this function adds sample content by default).}

\item{theme}{A Hugo theme on Github (a chararacter string of the form
\code{user/repo}).}

\item{theme_example}{Whether to copy the example in the \file{exampleSite}
directory if it exists in the theme. Not all themes provide example sites.}

\item{serve}{Whether to start a local server to serve the site.}

\item{path}{The path to the new file under the \file{content} directory.}

\item{kind}{The content type to create.}

\item{open}{Whether to open the new file after creating it. By default, it is
opened in an interactive R session.}

\item{title}{The title of the post.}

\item{author}{The author of the post.}

\item{categories}{A character vector of category names.}

\item{tags}{A character vector of tag names.}

\item{date}{The date of the post.}

\item{file}{The filename of the post. By default, the filename will be
automatically generated from the title by replacing non-alphanumeric
characters with dashes, e.g. \code{title = 'Hello World'} may create a file
\file{content/post/2016-12-28-hello-world.md}. The date of the form
\code{YYYY-mm-dd} will be prepended if the filename does not start with a
date.}

\item{subdir}{If specified (not \code{NULL}), the post will be generated
under a subdirectory under \file{content/post/}.}

\item{rmd}{Whether to create an R Markdown (.Rmd) or plain Markdown (.md)
file. Ignored if \code{file} has been specified.}
}
\description{
Wrapper functions to run Hugo commands via \code{\link{system2}('hugo',
...)}.
}
\section{Functions}{
\itemize{
\item \code{hugo_cmd}: Run an arbitrary Hugo command.

\item \code{new_site}: Create a new site (skeleton) via \command{hugo new
site}. The directory of the new site should be empty,

\item \code{new_content}: Create a new (R) Markdown file via \command{hugo new}
(e.g. a post or a page).

\item \code{new_post}: A wrapper function to create a new (R) Markdown post
under the \file{content/post/} directory via \code{new_content()}. If your
post will use R code chunks, you can set \code{rmd = TRUE} or the global
option \code{options(blogdown.use.rmd = TRUE)} in your \file{~/.Rprofile}.
Similarly, you can set \code{options(blogdown.author = 'Your Name')} so
that the author field is automatically filled out when creating a new post.
}}
\references{
The full list of Hugo commands: \url{https://gohugo.io/commands},
  and themes: \url{http://themes.gohugo.io}.
}
