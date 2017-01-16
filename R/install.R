## NOTE: this is not a suitable drop-in replacement for
## install.packages as semantics around lib and repos are different.
install_packages <- function(packages, lib, repos, ...,
                             standalone = FALSE,
                             installed_action = "skip",
                             error = TRUE) {
  ## TODO: At the moment, this is going to ignore the installed_action
  ## bit and we'll pick that up by refactoring the plan bits from
  ## cross_install.  This will have the (big) advantage of memoising
  ## the calls to available.packages
  ##
  ## TODO: at the moment, this produces a lot more noise than the
  ## cross_install so I've set it to quiet = TRUE for now.
  if (length(packages) == 0L) {
    return()
  }
  lib <- default_lib(lib)
  dir.create(lib, FALSE, TRUE)
  if (standalone) {
    db <- available_packages(repos, NULL, NULL)
    ## TODO: a different plan will be needed here when not creating a
    ## standalong repo; probably the biggest difference will be that
    ## 'lib' should be a vector of libraries to look in.
    ##
    ## TODO: Obviously rename cross_install_plan
    ##
    ## TODO: probably refactor cross_install_plan as all we care about
    ## here is the installation candidates but the binary/compile bit
    ## can be inferred later from the database given the list.
    ## Probably as useful will be something indicating if things are
    ## being upgraded or installed (which becomes less obvious when
    ## the number of libraries grows)
    ##
    ## TODO: Given the issues with getting the available db passed
    ## into install.packages, it would be better to try and infer
    ## installation candidates here either recursively (install the
    ## package, check for missing deps) or by looking at the current
    ## system (but that has the potential for corner cases where
    ## upstream has changed).  I think that the former is the right
    ## way to do it?
    ##
    ## TODO: if we have the db we can pass it through as available
    ## perhaps (though it interacts in annoying ways with
    ## binary/source installation).
    plan <- cross_install_plan(packages, db, lib, installed_action)
    extra <- setdiff(plan$packages, packages)
    if (length(extra) > 0L) {
      provisionr_log("deps", sprintf("%d extra: %s", length(extra),
                                     paste(extra, collapse = ", ")))
      packages <- plan$packages
    }
  }
  install_packages2(packages, lib, repos = repos, ...,
                    error = error, quiet = TRUE)
  ## TODO: This is not all the packages that were installed...
  invisible(packages)
}

default_lib <- function(lib) {
  if (is.null(lib)) {
    lib <- .libPaths()[[1L]]
  } else if (length(lib) > 1L) {
    lib <- lib[[1L]]
  }
  lib
}

## A version of install.packages() that will *stop on failure*.  Why
## is this needed?
##
## NOTE: rather than stopping on *all* warnings, we'll attempt to
## filter this to be the one that happens when packages are not
## available.
##
## TODO: This might have to route through gettext to work on
## non-English platforms.  Not totally sure how that is meant to be
## set though.
##
## The other option here is to check that all packages are installed
## coming out of this function, which we also attempt to do.
##
## Geting the withCallingHandlers bit correct is super difficult to
## make sure that the warnings are preseved.
##' @importFrom utils install.packages
install_packages2 <- function(pkgs, ..., error = TRUE) {
  e <- NULL
  capture <- function(e) {
    if (error) {
      catch <-
        grepl("package.*(is|are) not available", e$message) ||
        grepl("installation of package.*had non-zero exit status", e$message)
      if (catch) {
        e <<- e
      }
    }
  }
  withCallingHandlers(install.packages(pkgs, ...), warning = capture)
  if (!is.null(e)) {
    stop(e$message, call. = FALSE)
  }
}
