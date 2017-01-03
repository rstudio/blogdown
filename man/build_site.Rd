% Please edit documentation in R/render.R
\name{build_site}
\alias{build_site}
\title{Build a website}
\usage{
build_site(local = FALSE)
}
\arguments{
\item{local}{Whether to build the website locally to be served via
\code{\link{serve_site}()}. If \code{TRUE}, the site configurations
\code{baseurl} will be set to \code{/}, and \code{relativeurls} will be set
to \code{true}. If \code{FALSE}, default configurations of the website will
be used.}
}
\description{
Compile all Rmd files, build the site through Hugo, and post-process HTML
files generated from Rmd (e.g. fix figure paths).
}
\details{
You can use \code{\link{serve_site}()} to preview your website locally, and
\code{build_site()} to build the site for publishing.
}
