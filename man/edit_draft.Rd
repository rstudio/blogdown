% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{edit_draft}
\alias{edit_draft}
\title{Open a list of draft posts}
\usage{
edit_draft(files)
}
\arguments{
\item{files}{A vector of file paths.}
}
\description{
If a file is opened in RStudio, this function will try to locate the
\code{draft} field in YAML automatically, so you can edit this field
immediately.
}
\keyword{internal}
