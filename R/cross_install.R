## TODO: through here, need a mode where we don't throw errors but
## return information on what could not be installed due to having no
## installation candidates.
cross_install_packages <- function(packages, lib, db, plan, ...,
                                   allow_missing = FALSE) {
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

  for (i in seq_along(plan$packages)) {
    if (!plan$compile[i]) {
      cross_install_package(plan$packages[[i]], lib, plan$binary[[i]], db)
    }
  }

  plan
}

cross_install_package <- function(package, lib, binary, db) {
  provisionr_log("cross", package)
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  platform <- db$platform
  db_use <- db[[if (binary) "bin" else "src"]]
  x <- as.list(db_use[match(package, db_use[, "Package"]), ])

  ext <- if (!binary) "tar.gz" else if (platform == "windows") "zip" else "tgz"
  url <- sprintf("%s/%s_%s.%s", x$Repository, x$Package, x$Version, ext)

  if (grepl("file://", url)) {
    path <- file_unurl(url)
  } else {
    path <- file.path(tmp, basename(url))
    download_file(url, destfile = path)
  }

  dest <- file.path(lib, x$Package)
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
              paste0("--library=", shQuote(normalizePath(lib_tmp, "/"))),
              shQuote(normalizePath(path)))
    call_r(args, env = env)
    file.rename(file.path(lib_tmp, x$Package), dest)
  }

  invisible(TRUE)
}
