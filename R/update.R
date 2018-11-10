#' Get the "content" directory of the site
#'
#' `find_blog_content` gets the "content" directory of the site.
#'
#' @return A string containing the absolute path of the root directory for the
#' site.
#' @keywords internal
find_blog_content = function() {
  file.path(site_root(), "content")
}

#' Get the digest algorithm to use
#'
#' `get_digest_algorithm` gets the digest algorithm that will be used.
#'
#' Set the algorithm with `options(blogdown.algorithm = <algorithm>)`.
#' If the option is not set, then use crc32.
#'
#' @return A string containing the name of the algorithm.
#' @keywords internal
get_digest_algorithm = function() {
  getOption("blogdown.algorithm", default = "crc32")
}

#' Check which files need to be rebuilt
#'
#' `needs_rebuild` returns a vector of logicals indicating which files
#' need to be rebuilt, based on whether the file has changed.
#'
#' This function compares digests of current files to stored digests in order
#' to tell whether the source file needs to be rebuilt.
#' If the digests are not equal, then the file has changed. If a digest is
#' missing, then the source file is new or the output file has been deleted
#' and in either case, the source file needs to be rebuilt.
#'
#' @param current_digest A character vector containing digests of the
#' current source files (`.Rmd` or `.rmarkdown``).
#' @param current_dest_digest A character vector containing digests of the
#' current destination (output) files (`.html``).
#' `NA` for destination files that do not exist.
#' @param old_digest The stored digest for the source file from the last time
#' the site was built. `NA` if the source file did not exist at the time of the
#' last build.
#' @param old_dest_digest A character vector containing stored digests for the
#' destination files from the last time the site was built.
#' `NA` for destination files that did not exist after the last build.
#' @return A vector of logicals indicating whether the destination (output)
#' files are out of date relative to the source files.
#'
#' If a destination file is missing or if any of the digests don't match,
#' then the file needs to be rebuilt.
#' @keywords internal
needs_rebuild = function(current_digest, current_dest_digest,
                          old_digest, old_dest_digest) {
  out_of_date = current_digest != old_digest |
    current_dest_digest !=  old_dest_digest
  out_of_date = ifelse(is.na(out_of_date), TRUE, out_of_date)
  out_of_date
}

#' Calculate the file digest if the file exists
#'
#' `digest_if_file_exists` returns a character vector with the hashed digest
#' of the file, or `NA` if the file does not exist.
#'
#' @param file A string containing the path to a file.
#' @param alg A string containing the name of the digest algorithm to use.
#' Set the algorithm with `options(blogdown.digest = <algorithm>)`.
#' If no option was set, use crc32.
#' @return A string: if the file exists return the digest. Otherwise, return
#' `NA`.
#' @seealso \link[digest:digest]{digest()} for details about available algorithms.
#' @keywords internal
digest_if_exists = function(file) {
  if (file.exists(file)) {
    alg = get_digest_algorithm()
    digest::digest(file, file = TRUE, algo = alg)
  } else {
    as.character(NA)
  }
}

#' Figure out which files need to be rebuilt
#'
#' `files_to_rebuild` returns a vector of files that need to be rebuilt.
#'
#' This function accepts a vector of source files and
#' returns a vector of files that need to be rebuilt because the source file is
#' new or has changed since the last time the site was built.
#'
#' @param files A character vector of paths to source files (e.g., `.Rmd`).
#' @return A character vector of files that need to be rebuilt.
#' @keywords internal
files_to_rebuild = function(files) {
  base = site_root()
  files = normalizePath(files)
  files = unique(files)
  files = files[file.exists(files)]
  df = data.frame(file = files,
                   dest = output_file(files),
                   stringsAsFactors = FALSE)
  df = within(df, {
    cur_digest = sapply(file, digest_if_exists)
    cur_dest_digest = sapply(dest, digest_if_exists)
  })

  digest_file = file.path(base, "digests.Rds")

  if (file.exists(digest_file)) {
    digest = readRDS(digest_file)
    digest = within(digest, {
      file = sub("^~", base, file)
    })
    # Don't store the name of the output file because we're going to
    # merge digest with df by source file path, and df already has a dest
    # column.
    digest = digest[,colnames(digest) != "dest"]
    df = merge(df, digest, by = "file")
  } else {
    # If there isn't a digest file, then the site has not been updated
    # previously, so we store NA's and build the whole site.
    df = within(df, {
      digest = NA
      dest_digest = NA
    })
  }

  df = within(df, {
    rebuild = needs_rebuild(cur_digest, cur_dest_digest, digest, dest_digest)
  })

  df[df$rebuild,"file"]
}

#' Generates and stores digests for all source and output files.
#'
#' `update_rmd_digests` calculates hashed digests for a list of source files
#' and their corresponding output files and stores them in a file.
#'
#' Generates new hashed digests for both source and destination (output) files
#' and save the digests to a file "`digests.Rds`" in the root directory of the
#' site.
#'
#' @param files A character vector of paths to the source files.
#' @param partial Logical. If `TRUE`, keep rows from digest file for source
#' files that aren't in `files`. Otherwise, get rid of the old file and only
#' keep digests for source files in `files`.
#' @return The path to the digest file.
#' @keywords internal
#'
update_rmd_digests = function(files, partial = FALSE) {
  base = site_root()
  files = normalizePath(files)
  files = unique(files)
  files = files[file.exists(files)]

  digest_file = file.path(base, "digests.Rds")

  digests = data.frame(file = files,
                        dest = output_file(files),
                        stringsAsFactors = FALSE)
  digests = within(digests, {
    digest = sapply(file, digest_if_exists)
    dest_digest = sapply(dest, digest_if_exists)
    file = sub(base, "~", file, fixed = TRUE)
    dest = sub(base, "~", dest, fixed = TRUE)
  })

  if (partial && file.exists(digest_file)) {
    old_digests = readRDS(digest_file)
    to_keep = setdiff(old_digests$file, digests$file)
    old_digests = old_digests[old_digests$file %in% to_keep,]
    digests = cbind(digests, old_digests)
  }

  saveRDS(digests, file = digest_file)
  invisible(digest_file)
}

#' Delete stored digests for specified source files
#'
#' `prune_rmd_digests` removes the lines from the digest file corresponding to
#' a vector of source files.
#'
#' Modifies the stored digest file to remove lines corresponding to selected
#' source files.
#'
#' @param files A character vector of paths to the source files to be removed.
#' @return The path to the digest file.
#' @keywords internal
#'
prune_rmd_digests = function(files) {
  base = site_root()
  files = normalizePath(files)
  files = unique(files)
  files = sub(base, "~", files, fixed = TRUE)

  digest_file = file.path(base, "digests.Rds")

  if (length(files) && file.exists(digest_file)) {
    digests = readRDS(digest_file)
    digests = digests[! digests$file %in% files,]
    saveRDS(digests, file = digest_file)
  }

  invisible(digest_file)
}

#' Update all files that are out of date
#'
#' `update_site` rebuilds all source files that are new or have changed since
#' the last time the site was built.
#'
#' Given a source directory (by default the "content" directory in the
#' root directory of the project), find all source files (`.Rmd` and
#' `.rmarkdown`) in the directory tree under the source directory,
#' calculate hashed digests of the files, and compare them to a
#' stored list of digests from the last time the site was built.
#'
#' If the digests of either the source or output files don't match,
#' if a source file is new since the last time the site was built,
#' or if the output file does not exist,
#' then render the source file.
#'
#' After rendering any out-of-date files, regenerate the digest list
#' and save it to a file.
#'
#' @param dir A string containing the root directory for checking.
#' By default, the "content" directory of the project.
#' @param quiet Suppress output. By default this is `FALSE` and the
#' function emits an informational message about how many files will
#' be rebuit.
#' @return This function does not return anything
#' @seealso [build_site()], [build_dir()].
#' @export
update_site = function(dir = NULL, quiet = FALSE) {
  if (is.null(dir)) {
    dir = find_blog_content()
  }
  files = list_rmds(dir)
  to_build = files_to_rebuild(files)
  if (! quiet) {
    message("Building ", length(to_build), " out of date ",
            ifelse(length(to_build) == 1, "file", "files"),
            "; site has ", length(files), " ",
            ifelse(length(files) == 1, "file", "files"),
            " in total.")
  }
  build_rmds(to_build)
  update_rmd_digests(files)
}
