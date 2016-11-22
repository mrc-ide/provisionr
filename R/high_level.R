## The logic here should be to check through a library to check that
## all dependencies are sorted.  I think that we can do that pretty
## easily without hitting the network.
##
## One issue is if install.packages will add all dependencies when
## things are a bit incomplete.
provision_library <- function(packages, lib, upgrade = FALSE, src = NULL,
                              platform = NULL, version = NULL,
                              path_drat = stop("please provide"),
                              verbose = TRUE, check = TRUE,
                              installed_action = "replace") {
  ## Options for installed_action will be:
  ##
  ## * skip: filter out and skip over installed packages
  ## * upgrade: upgrade packages that are out of date
  ## * upgrade_all: as for upgrade, but also for packages that are
  ##   not *directly* required.
  ## * replace: reinstall packages
  installed_action <- match.arg(installed_action,
                                c("replace", "upgrade_all", "upgrade", "skip"))

  ## TODO: this could be broadened out to detect our current platform
  ## and version as a self install
  self <- is.null(platform) && is.null(version)

  if (check) {
    dat <- check_library(packages, lib)
    packages <- union(packages, dat$missing)
  }

  if (!is.null(src)) {
    if (inherits(src, "package_sources")) {
      src <- src$build()
    }
    if (inherits(src, "local_drat")) {
      repos <- c(src$repos, normalizePath(src$path))
    } else {
      stop("Invalid input for src")
    }
  }

  repos <- c(src$cran, src$repos)

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
