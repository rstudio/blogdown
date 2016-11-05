% Please edit documentation in R/hugo.R
\name{install_theme}
\alias{install_theme}
\title{Install a Hugo theme from Github}
\usage{
install_theme(theme, theme_example = FALSE)
}
\arguments{
\item{theme}{A Hugo theme on Github (a chararacter string of the form
\code{user/repo}).}

\item{theme_example}{Whether to copy the example in the \file{exampleSite}
directory if it exists in the theme. Not all themes provide example sites.}
}
\description{
Download the specified theme from Github and install to the \file{themes}
directory. Available themes are listed at \url{http://themes.gohugo.io}.
}
