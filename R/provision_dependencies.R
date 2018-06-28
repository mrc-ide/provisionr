##' Provision dependencies for a package.  The
##' \code{provision_dependencies_bootstrap} function writes out a
##' bootstrap script.
##'
##' @title Provision dependencies for a package
##'
##' @param lib Library to provision
##'
##' @param path_description Path to the \code{DESCRIPTION} file
##'
##' @param ... Additional arguments to \code{\link{provision_library}}
##'
##' @param src An optional description of additional packages, using
##'   \code{\link{package_sources}}.
##'
##' @param read_travis Logical, indicating if the \code{.travis.yml}
##'   should be read (if present).  If \code{TRUE} then packages
##'   listed within \code{r_github_packages} will be used in
##'   provisioning.
##'
##' @export
provision_dependencies <- function(lib, path_description = ".", ...,
                                   src = NULL, read_travis = FALSE) {
  dat <- provision_dependencies_read(path_description, src)
  provision_library(packages = dat$packages, lib = lib, src = dat$src, ...)
}


## TODO: this does not handle Remotes: yet (but see
## https://github.com/mrc-ide/provisionr/issues/13), as that should
## probably happen there.
provision_dependencies_read <- function(path, src = NULL, read_travis = FALSE) {
  path_description <- file.path(path, "DESCRIPTION")
  path_travis <- file.path(path, ".travis.yml")

  if (!file.exists(path_description)) {
    stop(sprintf("Did not find a DESCRIPTION file at '%s'", path))
  }
  cols <- c("Depends", "Imports", "LinkingTo")
  packages <- parse_deps(na.omit(c(read.dcf(path_description, cols))))

  if (read_travis && file.exists(path_travis)) {
    yml <- yaml::yaml.load_file(path_travis)
    pkgs <- yml$r_github_packages
    if (!is.null(pkgs)) {
      github <- package_sources(github = pkgs)
      if (is.null(src)) {
        src <- github
      } else {
        src$spec <- union(src$spec, github$spec)
      }
    }
  }
  list(packages = packages, src = src)
}


##' @export
##' @rdname provision_dependencies
##'
##' @param strict_lib Should the bootstrap script install provisionr
##'   within the local library too?
provision_dependencies_bootstrap <- function(lib = ".packages", src = NULL,
                                             read_travis = FALSE,
                                             strict_lib = TRUE) {
  if (!is.null(src)) {
    assert_is(src, "package_sources")
    src_list <- src$as_list()[c("repos", "spec")]
    src_list <- src_list[!vlapply(src_list, is.null)]
    if (length(src_list) == 0L) {
      src <- NULL
    } else {
      src <- src_list
    }
  }
  if (!file.exists("DESCRIPTION")) {
    stop("Expected a DESCRIPTION file in this directory")
  }
  code <- substitute({
    args <- commandArgs(trailingOnly = FALSE)
    file <- grep("^--file=", args, value = TRUE)
    if (length(file) != 1L) {
      stop("Could not determine path to running script")
    }
    here <- dirname(normalizePath(sub("^--file=", "", file)))
    message(sprintf("Working in '%s'", here))
    setwd(here)
    dir.create(lib, FALSE)

    if (strict_lib) {
      .libPaths(lib)
    } else {
      .libPaths(c(lib, .libPaths()))
    }
    repos <- c("https://cran.rstudio.com", "https://mrc-ide.github.io/drat/")
    has_provisionr <- function() {
      requireNamespace("provisionr", quietly = TRUE,
                       lib.loc = if (strict_lib) lib else NULL)
    }
    if (!has_provisionr()) {
      message("Installing provisionr")
      install.packages("provisionr", repos = repos, lib = lib)
    }
    if (!has_provisionr()) {
      stop("Failed to bootstrap provisionr")
    }
    src <- provisionr::package_sources(data = src_list)
    provisionr::provision_dependencies(lib, src = src,
                                       read_travis = read_travis)
    if (!file.exists(".Renviron")) {
      writeLines(sprintf('R_LIBS_USER="%s"', lib), ".Renviron")
    }
  }, list(lib = lib, strict_lib = strict_lib,
          src_list = src, read_travis = read_travis))

  dest <- "bootstrap.R"
  writeLines(c("#!/usr/bin/env Rscript", deparse(code)), dest)
  Sys.chmod(dest, "777")
}
