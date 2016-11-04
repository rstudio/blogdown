% Please edit documentation in R/utils.R
\name{serve_site}
\alias{serve_site}
\title{Live preview a site}
\usage{
serve_site(...)
}
\arguments{
\item{...}{Arguments passed to \code{servr::httw()}.}
}
\description{
Calls \code{servr::\link{httw}()} to watch for changes in the site, rebuild
the site if necessary, and refresh the page automatically.
}
