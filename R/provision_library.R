##' Create or update a library of packages.
##'
##' Cross installation of binary files is difficult and I need to come
##' up with a way of making that work nicely.
##'
##' @title Create or update a library
##'
##' @param packages A character vector of packages to include
##'
##' @param lib A path to the library; if it does not exist, it will be
##'   created.  If given as a vector of libraries (i.e., with more
##'   than one element) then packages will be installed into the first
##'   library, but subsequent libraries will be checked to make sure
##'   that dependencies are satisfied.
##'
##' @param platform The platform to create the library for.  If
##'   \code{NULL} then we build for the current platform (using
##'   \code{install_packages} if \code{version} is \code{NULL} or
##'   compatible with our current version).  Otherwise this can be one
##'   of \code{"windows"}, \code{"macosx"}, \code{"macosx/mavericks"}
##'   or \code{"linux"}, correspinding to the different directories
##'   that binaries live in (in the case of \code{"linux"} there are
##'   no binaries and things are a little more complicated).
##'
##' @param version The version of R to install packages for.  By
##'   default, we use the same version (major.minor) as the current
##'   running R version.  Otherwise provide the desired version number
##'   as a string or \code{numeric_version} object.
##'
##' @param src An optional description of additional packages, using
##'   \code{\link{package_sources}}.  It will only be rebuilt
##'   (fetching packages) if it has \emph{never} been built, or of
##'   packages listed in the \code{spec} element are not found in the
##'   repository.
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
##' @param allow_missing For cross-installation (via
##'   \code{cross_install} when \code{platform} is non-\code{NULL}),
##'   allow packages to be missing that need to be compiled?  The
##'   interface here is going to change a bunch, so watch out...
##'
##' @param quiet Passed through to to \code{\link{install.packages}},
##'   indicating if package installation should be done quietly.  With
##'   this as \code{FALSE} (the default) rather a lot of output can be
##'   generated.
##'
##' @export
##'
##' @importFrom stats na.omit setNames
##'
##' @importFrom utils available.packages capture.output contrib.url
##'   download.file installed.packages untar unzip
##'
##' @author Rich FitzJohn
provision_library <- function(packages, lib,
                              platform = NULL, version = NULL,
                              src = NULL,
                              check_dependencies = TRUE,
                              installed_action = "upgrade",
                              allow_missing = FALSE,
                              quiet = FALSE) {
  if (length(packages) == 0L) {
    return(NULL)
  }

  ## TODO: standalone argument that has an effect here where 'lib' is
  ## a vector; the checking functions will then look into all
  ## libraries, but install only into the last one.
  assert_scalar_character(lib)
  dir.create(lib, FALSE, TRUE)

  ## TODO: this no longer does what it says on the tin, quite.
  assert_scalar_logical(check_dependencies)
  installed_action <-
    match_value(installed_action,
                c("replace", "upgrade_all", "upgrade", "skip"))

  if (installed_action == "skip") {
    packages <- check_installed_packages(packages, lib)
    if (length(packages) == 0L) {
      return(NULL)
    }
  }

  ## Then we prepare the 'package_sources' object; this will pull all
  ## required packages into the drat repository (but not build binary
  ## packages)
  src <- prepare_package_sources(src)
  repos <- prepare_repos(src)

  db <- available_packages(repos, platform, version)
  plan <- plan_installation(packages, db, lib, installed_action)
  extra <- setdiff(plan$packages, packages)
  if (length(extra) > 0L) {
    provisionr_log("deps", sprintf("%d extra: %s", length(extra),
                                   paste(extra, collapse = ", ")))
    packages <- plan$packages
  }

  self <- is.null(platform) && is.null(version)

  if (self) {
    ## TODO: deal with the case where there are source packages in the
    ## drat that need compilation, if those occur upstream; see the
    ## note below.  Though I think that install.packages will
    ## actually do a decent job here.  It'd be easy enough to check by
    ## downloading a source CRAN package, updating the version number,
    ## adding to the drat and seeing what happens.  Needs testing on
    ## OSX/Windows though.
    install_packages2(packages, lib, repos = repos,
                      error = TRUE, quiet = quiet)
  } else {
    ## These are a bit special, and I don't manage to treat these
    ## correctly with the non-cross install, unless the version
    ## numbers are increased, and even then it may not work.
    if (!is.null(src$local_drat)) {
      special <- unname(
        read.dcf(file.path(contrib_url(src$local_drat, "src", NULL),
                           "PACKAGES"), "Package")[, "Package"])
      plan <- plan_force_binary(special, plan, src$local_drat)
    }

    plan <- cross_install_packages(packages, lib, db, plan,
                                   allow_missing = allow_missing)
  }

  plan$path_lib <- lib

  plan
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

prepare_repos <- function(src) {
  ## This attempts to avoid listing CRAN twice which makes
  ## available.packages quite slow.
  if (is.null(src$cran)) {
    r <- sanitise_options_cran()
  } else {
    r <- c(src$cran)
  }

  ## Repos are ordered from highest to lowest priority;
  ## * local
  ## * provided repos
  ## * cran
  if (!is.null(src$repos)) {
    r <- c(src$repos, r) # consider union()?
  }

  if (!is.null(src$local_drat)) {
    drat_add_empty_bin(contrib.url(src$local_drat, "binary"))
    r <- c("drat" = file_url(src$local_drat), r)
  }

  r
}

sanitise_options_cran <- function() {
  r <- getOption("repos", "https://cran.rstudio.com")
  if ("@CRAN@" %in% r) {
    r[r == "@CRAN@"] <- "https://cran.rstudio.com"
  }
  r
}
