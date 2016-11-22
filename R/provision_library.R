##' Create or update a library of packages.
##'
##' Cross installation of binary files is difficult and I need to come
##' up with a way of making that work nicely.
##'
##' @title Create or update a library
##' @param packages A character vector of packages to include
##'
##' @param lib A path to the library; if it does not exist, it will be
##'   created.
##'
##' @param platform The platform to create the library for.  If
##'   \code{NULL} then we build for the current platform (using
##'   \code{\link{install_packages}} if \code{version} is \code{NULL}
##'   or compatible with our current version).  Otherwise this can be
##'   one of \code{"windows"}, \code{"macosx"},
##'   \code{"macosx/mavericks"} or \code{"linux"}, correspinding to
##'   the different directories that binaries live in (in the case of
##'   \code{"linux"} there are no binaries and things are a little
##'   more complicated).
##'
##' @param version The version of R to install packages for.  By
##'   default, we use the same version (major.minor) as the current
##'   running R version.  Otherwise provide the desired version number
##'   as a string or \code{numeric_version} object.
##'
##' @param src An optional description of additional packages, using
##'   \code{\link{package_sources}} (can be either a
##'   \code{package_sources} object or a \code{local_drat} object).
##'
##' @param path_drat When using a \code{package_sources} object for
##'   \code{src}, the path to store the drat repository.  If not
##'   given, then a temporary directory will be used.
##'
##' @param check_dependencies Logical, indicating if dependencies of
##'   \code{packges} should be checked.  If \code{TRUE}, then any
##'   missing dependencies (packages in Depends, Imports or LinkingTo
##'   of the requested packages) will be installed.
##'
##' @param installed_action The behaviour when some packages are
##'   already installed.  Options are \code{"replace"} (will
##'   re-install the package and, for cross-installation, its
##'   dependencies), \code{"upgrade_all"} (upgrade all packages that
##'   lag behind the versions in repositories), \code{"upgrade"}
##'   (upgrade packages listed in \code{"packages"} only, but not
##'   their dependencies) and \code{"skip"} (do not install or upgrade
##'   any package that is already installed).
##'
##' @export
##' @author Rich FitzJohn
provision_library <- function(packages, lib,
                              platform = NULL, version = NULL,
                              src = NULL, path_drat = NULL,
                              check_dependencies = TRUE,
                              installed_action = "replace") {
  assert_scalar_character(lib)
  assert_scalar_logical(check_dependencies)

  dir.create(lib, FALSE, TRUE)
  ## Options for installed_action will be:
  ##
  ## * skip: filter out and skip over installed packages
  ## * upgrade: upgrade packages that are out of date
  ## * upgrade_all: as for upgrade, but also for packages that are
  ##   not *directly* required.
  ## * replace: reinstall packages
  installed_action <-
    match_value(installed_action,
                c("replace", "upgrade_all", "upgrade", "skip"))

  ## TODO: this could be broadened out to detect our current platform
  ## and version as a self install
  self <- is.null(platform) && is.null(version)

  if (!is.null(src)) {
    if (inherits(src, "package_sources")) {
      if (is.null(path_drat)) {
        path_drat <- tempfile()
        on.exit(unlink(path_drat, recursive = TRUE))
      } else {
        assert_scalar_character(path_drat)
      }
      src <- src$build(path_drat)
    }
    if (!inherits(src, "local_drat")) {
      stop("Invalid input for src")
    }
    repos <- c(src$cran, src$repos)
  } else {
    ## TODO: this is not going to be *quite* enough to deal with the
    ## dreaded "not setting CRAN" clusterfuck.
    repos <- getOption("repos", "https://cran.rstudio.com")
  }

  if (check_dependencies) {
    dat <- check_library(packages, lib)
    packages <- union(packages, dat$missing)
  }

  ## The upgrade thing here is very difficult; if someone removes a
  ## dependency, it's pretty difficult to check this.  But we *can*
  ## read package DESCRIPTIUON files (though don't use
  ## packageDescription because that will use a loaded one intead of
  ## the one in the library).  We can use this to recurse through
  ## dependencies fairly happily.  Leaving this alone for now.
  if (self) {
    res <- with_repos(repos,
                      install_packages(packages, lib,
                                       installed_action = installed_action))
  } else {
    res <- with_repos(repos,
                      cross_install_packages(
                        packages, lib, platform, version = version,
                        installed_action = installed_action))
  }

  ## TODO: should report on what was installed perhaps?  We're going to need to
}

with_repos <- function(repos, expr) {
  oo <- options(repos = repos)
  on.exit(options(oo))
  expr
}

check_library <- function(packages, lib) {
  missing <- character(0)
  found <- character(0)

  while (length(packages) > 0L) {
    path <- file.path(lib, packages, "DESCRIPTION")
    present <- file.exists(path)
    if (any(!present)) {
      missing <- c(missing, packages[!present])
      path <- path[present]
    }
    found <- c(found, packages[present])

    deps <- function(p) {
      cols <- c("Depends", "Imports", "LinkingTo")
      setdiff(parse_deps(na.omit(c(read.dcf(p, cols)))),
              c(base_packages(), missing, found))
    }
    packages <- unique(unlist(lapply(path, deps), TRUE, FALSE))
  }

  list(found = found, missing = missing)
}
