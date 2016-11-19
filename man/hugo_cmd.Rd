% Please edit documentation in R/hugo.R
\name{hugo_cmd}
\alias{hugo_cmd}
\alias{new_content}
\alias{new_site}
\title{Run Hugo commands}
\usage{
hugo_cmd(...)

new_site(dir = ".", force, format = "yaml", sample = TRUE, 
    theme = "yihui/hugo-future-imperfect", theme_example = TRUE, 
    serve = TRUE)

new_content(path, format = "yaml", kind = NA, open = interactive())
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

\item{open}{Whether to open the new file after creating it. By default, it is
opened in an interactive R session.}
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
}}
\references{
The full list of Hugo commands: \url{https://gohugo.io/commands},
  and themes: \url{http://themes.gohugo.io}.
}
