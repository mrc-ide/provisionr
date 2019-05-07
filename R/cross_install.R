## TODO: through here, need a mode where we don't throw errors but
## return information on what could not be installed due to having no
## installation candidates.
cross_install_packages <- function(packages, lib, db, plan, ...,
                                   workdir = NULL,
                                   allow_missing = FALSE, progress = NULL) {
  if (lib %in% .libPaths()) {
    stop("Do not use cross_install_packages to install into current library")
  }
  dir.create(lib, FALSE, TRUE)

  if (any(plan$compile)) {
    needs_compilation <- plan$packages[plan$compile]
    msg <- paste("Packages need compilation; cannot cross-install:",
                 paste(needs_compilation, collapse = ", "))
    if (allow_missing) {
      plan$missing <- db$src[needs_compilation, , drop = FALSE]
      provisionr_log("skip", msg)
    } else {
      stop(msg)
    }
  }

  ## TODO: download the packages here instead, then cross install
  if (is.null(workdir)) {
    workdir <- tempfile()
    on.exit(unlink(workdir, recursive = TRUE))
  }
  dir.create(workdir, FALSE, TRUE)

  install <- plan$packages
  urls <- files <- character(length(install))
  urls[plan$binary] <- package_url(install[plan$binary],
                                   db$bin, binary_type(db$target))
  urls[!plan$binary] <- package_url(install[!plan$binary],
                                    db$src, "source")
  i <- !plan$compile
  if (any(i)) {
    files[i] <- download_files(urls[i], workdir, labels = install[i],
                               progress = progress)
    for (j in which(i)) {
      cross_install_package(install[[j]], files[[j]], lib, plan$binary[[j]])
    }
  }

  plan
}

cross_install_package <- function(package, path, lib, binary) {
  provisionr_log("cross", package)

  dest <- file.path(lib, package)
  if (file.exists(dest)) {
    unlink(dest, recursive = TRUE)
  }
  if (binary) {
    unzip(path, exdir = lib)
  } else {
    ## We need to install the package into a temporary library and
    ## move it into place because otherwise preparing the package for
    ## lazy loading will cause issues if the package triggers loading
    ## a package with compiled dependencies.  This is tested in the
    ## "missing compiled packages" at least.
    ##
    ## TODO: I think that means that we must have dependencies of each
    ## package installed *locally* already, in which case I probably
    ## need to provide better diagnostics.  This will be an issue for
    ## things like testing on CRAN and the like because I'm probably
    ## going to trigger installation failure.
    lib_tmp <- tempfile("cross_tmp_lib", tmpdir = lib)
    dir.create(lib_tmp)
    on.exit(try(unlink(lib_tmp, recursive = TRUE), silent = TRUE), add = TRUE)
    env <- c(R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep),
             CYGWIN = "nodosfilewarning")
    args <- c("CMD", "INSTALL", "--no-test-load",
              paste0("--library=", shQuote(abs_path(lib_tmp)),
              shQuote(abs_path(path)))
    call_r(args, env = env)
    file.rename(file.path(lib_tmp, package), dest)
  }

  invisible(TRUE)
}
