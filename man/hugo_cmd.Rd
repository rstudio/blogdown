% Please edit documentation in R/hugo.R
\name{new_site}
\alias{new_content}
\alias{new_site}
\title{Run Hugo commands}
\usage{
new_site(dir = ".", force, format = "yaml", sample = TRUE, 
    theme = "dim0627/hugo_theme_robust", theme_example = TRUE, 
    serve = TRUE)

new_content(path, format = "yaml", kind = NA, editor = interactive())
}
\arguments{
\item{dir}{The directory of the new site.}

\item{force}{Whether to create a new site in an existing directory. The
default value is \code{TRUE} if none of the files/directories to be
generated exist in this directory, otherwise \code{FALSE}, to make sure
your existing files are not overwritten.}

\item{format}{The format of the configuration file or the frontmatter of the
new (R) Markdown file.}

\item{sample}{Whether to add sample content. Hugo creates an empty site by
default, but this function adds sample content by default).}

\item{theme}{A Hugo theme on Github (a chararacter string of the form
\code{user/repo}).}

\item{theme_example}{Whether to copy the example in the \file{exampleSite}
directory if it exists in the theme. Not all themes provide example sites.}

\item{serve}{Whether to start a local server to serve the site.}

\item{path}{The path to the new file.}

\item{kind}{The content type to create.}

\item{editor}{Whether to open the new file after creating it. By default, it
is opened in an interactive R session.}
}
\description{
Wrapper functions to run Hugo commands via \code{\link{system2}('hugo',
...)}.
}
\section{Functions}{
\itemize{
\item \code{new_site}: Create a new site (skeleton) via \command{hugo new
site}.

\item \code{new_content}: Create a new (R) Markdown file via \command{hugo new}
(e.g. a post or a page).
}}
\references{
The full list of Hugo commands: \url{https://gohugo.io/commands},
  and themes: \url{http://themes.gohugo.io}.
}
