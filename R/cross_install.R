## TODO: through here, need a mode where we don't throw errors but
## return information on what could not be installed due to having no
## installation candidates.
cross_install_packages <- function(packages, lib, db, ...,
                                   installed_action = "skip",
                                   allow_missing = FALSE,
                                   require_compilation = NULL) {
  if (lib %in% .libPaths()) {
    stop("Do not use cross_install_packages to install into current library")
  }
  dir.create(lib, FALSE, TRUE)

  plan <- cross_install_plan(packages, db, lib, installed_action)
  if (any(plan$compile)) {
    needs_compilation <- plan$packages[plan$compile]
    msg <- paste("Packages need compilation; cannot cross-install:",
                 paste(needs_compilation, collapse=", "))
    if (allow_missing) {
      plan$missing <- db$src[needs_compilation, , drop = FALSE]
      message(msg)
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

base_packages <- function() {
  rownames(installed.packages(priority = c("base", "recommended")))
}

recursive_deps <- function(x, db) {
  done <- character()
  base <- base_packages()
  cols <- c("Depends", "Imports", "LinkingTo")

  while (length(x) > 0L) {
    done <- c(done, x)
    deps <- parse_deps(na.omit(c(db[match(x, db[, "Package"]), cols])))
    x <- setdiff(deps, c(x, base))
  }

  sort(unique(done))
}

## TODO: memoise this because it's quite slow
available_packages <- function(repos, platform, version) {
  provisionr_log("download", "package database")

  if (!is.null(platform)) {
    platform <- match_value(platform, valid_platforms())
  }

  is_local <- grepl("^(/|file://)", repos)
  if (any(is_local)) {
    i <- file.exists(repos)
    if (any(i)) {
      repos[i] <- file_url(repos[i])
    }
  }

  url_src <- contrib_url(repos, "src", NULL)
  if (any(is_local)) {
    lapply(file_unurl(url_src[is_local]), drat_ensure_PACKAGES)
  }
  pkgs_src <- available.packages(contrib_url(repos, "src", NULL))
  if (is.null(platform) || platform == "linux") {
    pkgs_bin <- pkgs_src[integer(0), ]
  } else {
    version_str <- r_version_str(check_r_version(version), 2L)
    url_bin <- contrib_url(repos, platform, version_str)
    if (any(is_local)) {
      lapply(file_unurl(url_bin[is_local]), drat_ensure_PACKAGES)
    }
    pkgs_bin <- available.packages(url_bin)
  }
  extra <- setdiff(rownames(pkgs_bin), rownames(pkgs_src))
  if (length(extra) > 0L) {
    pkgs_all <- rbind(pkgs_src, pkgs_bin[extra, ])
  } else {
    pkgs_all <- pkgs_src
  }
  list(all = pkgs_all, src = pkgs_src, bin = pkgs_bin,
       platform = platform, version = version)
}

contrib_url <- function(repos, platform, version_str) {
  if (is.null(version_str)) {
    version_str <- r_version_str(check_r_version(version_str))
  }
  ## platform should be:
  ##   src
  ##   windows
  ##   macosx
  ##   macosx/mavericks
  if (platform == "src") {
    path <- "src/contrib"
  } else {
    path <- file.path("bin", platform, "contrib", version_str)
  }
  file.path(sub("/$", "", repos), path)
}

parse_deps <- function(x) {
  ## TODO: This does not support returning version numbers (so
  ## depending on particular versions of packages is not going to work
  ## here).
  ##
  ## Somewhere I had the version parsing thing; I will need that back
  ## soon.  For now this just strips version information entirely.
  ## This could be something good to push into remotes, perhaps?
  val <- unlist(strsplit(x, ","), use.names=FALSE)
  val <- gsub("(\\s|\\().*", "", trimws(val))
  val[val != "R"]
}

cross_install_plan <- function(packages, db, lib, installed_action) {
  if (installed_action == "skip") {
    skip <- .packages(TRUE, lib)
  } else {
    skip <- NULL
  }
  skip <- c(skip, base_packages())
  requested <- packages

  ## TODO: drop the setdiff here, which also drops the lib argument?
  msg <- setdiff(packages, c(db$all[, "Package"], skip))
  if (length(msg) > 0L) {
    stop(sprintf("Can't find installation candidate for: %s",
                 paste(msg, collapse=", ")))
  }

  packages <- setdiff(recursive_deps(packages, db$all), skip)
  msg <- setdiff(packages, db$all[, "Package"])
  if (length(msg) > 0L) {
    stop(sprintf("Can't find installation candidate for dependencies: %s",
                 paste(msg, collapse=", ")))
  }

  if (installed_action == "skip") {
    packages <- setdiff(packages, .packages(TRUE, lib))
  }

  binary <- packages %in% rownames(db$bin)

  if (installed_action == "upgrade" || installed_action == "upgrade_all") {
    if (installed_action == "upgrade") {
      check <- union(setdiff(packages, .packages(TRUE, lib)), requested)
    } else {
      check <- packages
    }
    packages <- check_version(check, lib, db)
  }

  compile <- rep_len(FALSE, length(packages))
  binary <- packages %in% rownames(db$bin)
  if (any(!binary)) {
    j <- match(packages[!binary], db$src[, "Package"])
    compile[!binary] <- db$src[j, "NeedsCompilation"] == "yes"
  }

  list(packages = packages,
       binary = binary,
       compile = compile)
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
    env <- sprintf("%s=%s", names(env), unname(env))
    args <- c("CMD", "INSTALL", "--no-test-load",
              paste0("--library=", shQuote(normalizePath(lib_tmp, "/"))),
              shQuote(normalizePath(path)))
    call_system(file.path(R.home(), "bin", "R"), args, env = env)
    file.rename(file.path(lib_tmp, x$Package), dest)
  }

  invisible(TRUE)
}

drat_ensure_PACKAGES <- function(path) {
  path_PACKAGES <- file.path(path, "PACKAGES")
  if (!file.exists(path_PACKAGES)) {
    dir.create(path, FALSE, TRUE)
    writeLines(character(0), path_PACKAGES)
  }
}

valid_platforms <- function() {
  c("windows", "macosx", "macosx/mavericks", "linux")
}

check_version <- function(packages, lib, db) {
  installed <- packages %in% .packages(TRUE, lib)
  if (any(installed)) {
    check <- packages[installed]
    v_installed <- setNames(numeric_version(
      vcapply(file.path(lib, check, "DESCRIPTION"), read.dcf, "Version")),
      check)
    binary <- check %in% rownames(db$bin)
    v_db <- setNames(character(length(check)), check)
    v_db[binary] <- db$bin[check[binary], "Version"]
    v_db[!binary] <- db$src[check[!binary], "Version"]
    v_db <- numeric_version(v_db)
    installed[installed] <- v_installed >= v_db
  }
  packages[!installed]
}
