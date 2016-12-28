% Please edit documentation in R/hugo.R
\name{hugo_cmd}
\alias{hugo_cmd}
\alias{new_content}
\alias{new_post}
\alias{new_site}
\title{Run Hugo commands}
\usage{
hugo_cmd(...)

new_site(dir = ".", force, format = "toml", sample = TRUE, 
    theme = "yihui/hugo-future-imperfect", theme_example = TRUE, 
    serve = TRUE)

new_content(path, kind = NA, open = interactive())

new_post(title, kind = NA, open = interactive(), author = getOption("blogdown.author"), 
    rmd = getOption("blogdown.use.rmd", TRUE))
}
\arguments{
\item{...}{Arguments to be passed to \code{system2('hugo', ...)}, e.g.
\code{new_content(path)} is basically \code{hugo_cmd(c('new', path))} (i.e.
run the command \command{hugo new path}).}

\item{dir}{The directory of the new site.}

\item{force}{Whether to create a new site in an existing directory. The
default value is \code{TRUE} if the \code{dir} directory is empty or only
contain hidden files and RStudio project (\file{*.Rproj}) files, otherwise
\code{FALSE}, to make sure your existing files are not overwritten.}

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

\item{title}{The title or filename of the post. If it contains an extension
\file{.Rmd} or \file{.md}, it will be treated as the filename of the post,
otherwise it is supposed to be the title of the post, and the actual
filename will be automatically generated from the title by replacing
non-alphanumeric characters with dashes, e.g. \code{title = 'Hello World'}
may create a file \file{content/post/2016-12-28-hello-world.Rmd}. The
current date of the form \code{YYYY-mm-dd} will be prepended if the
filename does not start with a date.}

\item{author}{The author of the post.}

\item{rmd}{Whether to create an R Markdown (.Rmd) or plain Markdown (.md)
file.}
}
\description{
Wrapper functions to run Hugo commands via \code{\link{system2}('hugo',
...)}.
}
\section{Functions}{
\itemize{
\item \code{hugo_cmd}: Run an arbitrary Hugo command.

\item \code{new_site}: Create a new site (skeleton) via \command{hugo new
site}.

\item \code{new_content}: Create a new (R) Markdown file via \command{hugo new}
(e.g. a post or a page).

\item \code{new_post}: A wrapper function to create a new (R) Markdown post
under the \file{content/post/} directory via \code{new_content()}. If your
post will not contain any R code chunks, you can set \code{rmd = FALSE} or
the global option \code{options(blogdown.use.rmd = FALSE)} in your
\file{~/.Rprofile}. Similarly, you can set \code{options(blogdown.author =
'Your Name')} so that the author field is automatically filled out when
creating a new post.
}}
\references{
The full list of Hugo commands: \url{https://gohugo.io/commands},
  and themes: \url{http://themes.gohugo.io}.
}
