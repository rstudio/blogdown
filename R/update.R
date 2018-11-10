#' Get the "content" directory of the site
#'
#' \code{find_blog_content} gets the "content" directory of the site.
#'
#' @return A string containing the absolute path of the root directory for the
#' site.
#' @keywords internal
find_blog_content = function() {
  file.path(site_root(), "content")
}

#' Get the digest algorithm to use
#'
#' \code{get_digest_algorithm} gets the digest algorithm that will be used.
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
#' \code{needs_rebuild} returns a vector of logicals indicating which files
#' need to be rebuilt, based on whether the file has changed.
#'
#' This function compares digests of current files to stored digests in order
#' to tell whether the source file needs to be rebuilt.
#' If the digests are not equal, then the file has changed. If a digest is
#' missing, then the source file is new or the output file has been deleted
#' and in either case, the source file needs to be rebuilt.
#'
#' @param current_digest A character vector containing digests of the
#' current source files (\code{.Rmd} or \code{.rmarkdown}`).
#' @param current_dest_digest A character vector containing digests of the
#' current destination (output) files (\code{.html}`).
#' \code{NA} for destination files that do not exist.
#' @param old_digest The stored digest for the source file from the last time
#' the site was built. \code{NA} if the source file did not exist at the time of the
#' last build.
#' @param old_dest_digest A character vector containing stored digests for the
#' destination files from the last time the site was built.
#' \code{NA} for destination files that did not exist after the last build.
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
#' \code{digest_if_file_exists} returns a character vector with the hashed digest
#' of the file, or \code{NA} if the file does not exist.
#'
#' @param file A string containing the path to a file.
#' @param alg A string containing the name of the digest algorithm to use.
#' Set the algorithm with `options(blogdown.digest = <algorithm>)`.
#' If no option was set, use crc32.
#' @return A string: if the file exists return the digest. Otherwise, return
#' \code{NA}.
#' @seealso \code{\link[digest:digest]{digest()}} for details about available
#' algorithms.
#' @keywords internal
digest_if_exists = function(file, alg = NA) {
  if (file.exists(file)) {
    if (is.na(alg)) {
      alg = get_digest_algorithm()
    }
    dgst = digest::digest(file, file = TRUE, algo = alg)
  } else {
    dgst = as.character(NA)
    alg = as.character(NA)
  }
  c(digest = dgst, alg = alg)
}

#' Create a data frame with stored digests and digests of current files
#'
#' \code{get_current_digests} returns a data frame with a row for every file
#' and columns for stored and current digests of source and output files.
#'
#' This function accepts a vector of source files and
#' returns a data frame with a row for each file and columns for the
#' stored digests and the digests of current source and output files.
#'
#' @param files A character vector of paths to source files (e.g., \code{.Rmd}).
#' @return A a data frame with a row for every file and columns:
#' \describe{
#' \item{\code{file}}{The source file name.}
#' \item{\code{dest}}{The output file name.}
#' \item{\code{alg}}{The digest algorithm.}
#' \item{\code{digest}}{The stored digest for the source file.}
#' \item{\code{dest_digest}}{The stored digest for the output file.}
#' \item{\code{cur_digest}}{The digest for the current source file.}
#' \item{\code{cur_dest_digest}}{The digest for the current output file.}
#' }
#'
#' Digests for missing files are set to \code{NA}.
#' @seealso \code{\link{files_to_rebuild}()},
#' \code{\link{digest_if_exists}()}.
#' @keywords internal
get_current_digests = function(files) {
  base = site_root()
  files = normalizePath(files)
  files = unique(files)
  files = files[file.exists(files)]
  df = data.frame(file = files,
                  dest = output_file(files),
                  stringsAsFactors = FALSE)

  digest_file = file.path(base, "digests.Rds")

  if (file.exists(digest_file)) {
    digests = readRDS(digest_file)
    digests$file = sub("^~", base, digests$file)
    # Don't store the name of the output file because we're going to
    # merge digest with df by source file path, and df already has a dest
    # column.
    digests = digests[,colnames(digests) != "dest"]

    # left join: we only want to check digests for the specified files.
    df = merge(df, digests, by = "file", all.x = TRUE)
  } else {
    # If there isn't a digest file, then the site has not been updated
    # previously, so we store NA's and build the whole site.
    df$digest = as.character(NA)
    df$dest_digest = as.character(NA)
    df$alg = as.character(NA)
  }

  #
  # All of the stuff below would be much cleaner and easier to maintain
  # and debug if I used tidyverse (dplyr/tidyr/purrr), but that would
  # require making blogdown dependent on all of these packages, which
  # would lead to greater installation bloat. --- Jonathan Gilligan
  #

  files_to_digest = data.frame(file = files, stringsAsFactors = FALSE)
  # left join because we only want to check the specified files
  files_to_digest = merge(files_to_digest, df[,c("file", "dest", "alg")],
                          by = "file", all.x = TRUE)

  cur_digests = mapply(digest_if_exists, file = files_to_digest$file,
                       alg = files_to_digest$alg)
  # Convert from a matrix with columns for each file and two rows to a data
  # frame with three columns: filename, digest, and algorithm and a row for
  # each file
  cur_digests = as.data.frame(t(cur_digests), stringsAsFactors = FALSE)
  cur_digests$file = rownames(cur_digests)
  colnames(cur_digests)[colnames(cur_digests) == "digest"] = "cur_digest"

  cur_dest_digests = mapply(digest_if_exists, file = files_to_digest$dest,
                            alg = files_to_digest$alg)
  cur_dest_digests = as.data.frame(t(cur_dest_digests),
                                   stringsAsFactors = FALSE)
  cur_dest_digests$file = rownames(cur_dest_digests)
  colnames(cur_dest_digests)[colnames(cur_dest_digests) == "digest"] =
    "cur_dest_digest"
  colnames(cur_dest_digests)[colnames(cur_dest_digests) == "file"] = "dest"

  #
  # Check for mismatches in the algorithms used.
  # This is for debugging/consistentcy testing and
  # can be removed when this moves to production
  #
  da = digests$alg
  cda = cur_digests$alg
  cdda = cur_dest_digests$alg
  cdd_na_idx <- which(is.na(cdda))
  if (length(cdd_na_idx) > 0) {
    cdda = cdda[-cdd_na_idx]
    cda = cda[-cdd_na_idx]
  }
  if (! isTRUE(all.equal(cda, cdda))) {
    warning("Unequal algorithms at ", length(cda), " places.")
  }

  da = digests$alg
  cda = cur_digests$alg
  na_idx = which(is.na(da))
  da = da[-na_idx]
  cda = cda[-na_idx]

  diff_idx = which(cda != da)
  if (length(diff_idx) > 0) {
    warning("Unequal algorithms digest vs. current at ", length(diff_idx), " places.")
  }

  df = merge(df, cur_digests[,c("file", "cur_digest")], by = "file",
             all.x = TRUE, all.y = TRUE)
  df = merge(df, cur_dest_digests[,c("dest", "cur_dest_digest")], by = "dest",
             all.x = TRUE, all.y = TRUE)
  rownames(df) = NULL
  # Organize columns in an aesthetically pleasing order.
  df = df[,c("file", "dest", "alg", "digest", "dest_digest",
             "cur_digest", "cur_dest_digest")]
  invisible(df)
}

#' Figure out which files need to be rebuilt
#'
#' \code{files_to_rebuild} returns a vector of files that need to be rebuilt.
#'
#' This function accepts a vector of source files and
#' returns a vector of files that need to be rebuilt because the source file is
#' new or has changed since the last time the site was built.
#'
#' @param files A character vector of paths to source files (e.g., \code{.Rmd}).
#' @return A character vector of files that need to be rebuilt.
#' @seealso \code{\link{get_current_digests}()}.
#' @keywords internal
files_to_rebuild = function(files) {
  base = site_root()
  files = normalizePath(files)
  files = unique(files)
  files = files[file.exists(files)]

  df = get_current_digests(files)

  df$rebuild = needs_rebuild(df$cur_digest, df$cur_dest_digest,
                             df$digest, df$dest_digest)
  df[df$rebuild,"file"]
}

#' Generates and stores digests for all source and output files.
#'
#' \code{update_rmd_digests} calculates hashed digests for a list of source files
#' and their corresponding output files and stores them in a file.
#'
#' Generates new hashed digests for both source and destination (output) files
#' and save the digests to a file "\code{digests.Rds}" in the root directory of the
#' site.
#'
#' @param files A character vector of paths to the source files.
#' @param partial Logical. If \code{TRUE}, keep rows from digest file for source
#' files that aren't in \code{files}. Otherwise, get rid of the old file and only
#' keep digests for source files in \code{files}.
#' @return The path to the digest file.
#' @seealso \code{\link{update_site_digests}()}.
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

  file_digests = sapply(digests$file, digest_if_exists)
  file_digests = as.data.frame(t(file_digests), stringsAsFactors = FALSE)
  file_digests$file = rownames(file_digests)

  dest_digests = sapply(digests$dest, digest_if_exists)
  dest_digests = as.data.frame(t(dest_digests), stringsAsFactors = FALSE)
  dest_digests$dest = rownames(dest_digests)
  dest_digests = dest_digests[,names(dest_digests) != "alg"]
  colnames(dest_digests)[colnames(dest_digests) == "digest"] = "dest_digest"

  digests = merge(digests, file_digests, by = "file", all.x = TRUE)
  digests = merge(digests, dest_digests, by = "dest", all.x = TRUE)
  digests$file = sub(base, "~", digests$file, fixed = TRUE)
  digests$dest = sub(base, "~", digests$dest, fixed = TRUE)

  if (partial && file.exists(digest_file)) {
    old_digests = readRDS(digest_file)
    to_keep = setdiff(old_digests$file, digests$file)
    old_digests = old_digests[old_digests$file %in% to_keep,]
    digests = rbind(digests, old_digests)
  }

  saveRDS(digests, file = digest_file)
  invisible(digest_file)
}

#' Generates and stores digests for all source and output files.
#'
#' \code{update_site_digests} calculates hashed digests for a site.
#'
#' Generates new hashed digests for both source and destination (output) files
#' and save the digests to a file "\code{digests.Rds}" in the root directory of the
#' site.
#'
#' @param dir A string with the name of the directory to search
#' (by default the "content" directory at the top-level directory of the site)
#' @param partial Logical. If \code{TRUE}, keep digests for source
#' files that aren't in the specified directory and its children and
#' descendants.
#' Otherwise, get rid of the old digest file and only keep digests for
#' source files in the source directory and its descendants.
#' @return The path to the digest file.
#' @seealso \code{\link{prune_site_digests}()}, \code{\link{update_site}()}.
#' @export
#'
update_site_digests = function(dir = NA, partial = FALSE) {
  if (is.na(dir)) {
    dir = find_blog_content()
  }
  files = list_rmds(dir)
  invisible(update_rmd_digests(files, partial))
}


#' Delete stored digests for specified source files
#'
#' \code{prune_rmd_digests} removes the lines from the digest file corresponding to
#' a vector of source files.
#'
#' Modifies the stored digest file to remove lines corresponding to selected
#' source files.
#'
#' @param files A character vector of paths to the source files to be removed.
#' @return The path to the digest file.
#' @seealso \code{\link{prune_site_digests}()},
#' \code{\link{update_site_digests}()}, \code{\link{update_rmd_digests}()}.
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

#' Delete stored digests for specified source files
#'
#' \code{prune_site_digests} removes the lines from the digest file corresponding to
#' a vector of source files.
#'
#' Modifies the stored digest file to remove lines corresponding to selected
#' source files.
#'
#' @param files A character vector of paths to the source files to be removed.
#' @return The path to the digest file.
#' @seealso \code{\link{update_site_digests}()}.
#' @export
#'
prune_site_digests = function(files) {
  files = normalizePath(files)
  files = unique(files)

  digest_file = prune_rmd_digests(files)

  invisible(digest_file)
}

#' Update all files that are out of date
#'
#' \code{update_site} rebuilds all source files that are new or have changed since
#' the last time the site was built.
#'
#' Given a source directory (by default the "content" directory in the
#' root directory of the project), find all source files (\code{.Rmd} and
#' \code{.rmarkdown}) in the directory tree under the source directory,
#' calculate hashed digests of the files, and compare them to a
#' stored list of digests from the last time the site was built.
#'
#' If the digests of either the source or output files don't match,
#' if a source file is new since the last time the site was built,
#' or if the output file does not exist,
#' then render the source file.
#'
#' After rendering any out-of-date files, regenerate the digest list
#' and saves it to a file.
#'
#' @param dir A string containing the root directory for checking.
#' By default, the "content" directory of the project.
#' @param quiet Suppress output. By default this is \code{FALSE} and the
#' function emits an informational message about how many files will
#' be rebuit.
#' @return This function does not return anything
#' @seealso \code{\link{build_site}()}, \code{\link{build_dir}()}.
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
