##' Utility function for checking an R version
##'
##' @title Check R version
##'
##' @param version Something to coerce into an R version.  Valid
##'   values are \code{NULL} (the current session's R version), a
##'   character string of the form \code{x.y.z}, a character string
##'   \code{oldrel} or \code{release} or a \code{numeric_version}
##'   object.
##'
##' @export
check_r_version <- function(version = NULL) {
  if (is.null(version)) {
    version <- getRversion()
  } else {
    assert_scalar(version)
    if (is.character(version)) {
      version <- switch(version,
                        oldrel = r_oldrel_version(),
                        release = r_release_version(),
                        numeric_version(version))
    } else if (!inherits(version, "numeric_version")) {
      stop("Invalid type for version")
    }
    version_str <- as.character(version)
    valid <- r_versions()$version
    if (!(version_str %in% valid)) {
      stop(sprintf("%s is not a known R version", version_str))
    }
  }
  version
}

## Until the PR that I made on rversions makes it to CRAN
cache <- new.env(parent = emptyenv())
r_versions <- function() {
  if (is.null(cache$r_versions)) {
    cache$r_versions <- rversions::r_versions()
  }
  cache$r_versions
}

r_release_version <- function() {
  ## This one is easy to re-implement and increases the chance of a
  ## cache hit:
  numeric_version(tail(r_versions()$version, 1))
}

r_oldrel_version <- function() {
  ## This one is harder to get right so I will take a second download
  ## of the version list
  if (is.null(cache$r_oldrel)) {
    cache$r_oldrel <- numeric_version(rversions::r_oldrel()[["version"]])
  }
  cache$r_oldrel
}
