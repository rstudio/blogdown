#' Build a site, using only modified .Rmd files
#'
#' The function \code{build_site_modified()} builds the site through Hugo,
#' using only modified .Rmd files. This avoids the case where blogdown
#' compiles all .Rmd files in the website directory, which can often cause
#' problems with code from an earlier date that does not run anymore for whatever
#' reason.
#' @export
build_site_modified = function() {
  blogdown::build_site(local = TRUE, run_hugo = FALSE)
  blogdown::hugo_build()
}