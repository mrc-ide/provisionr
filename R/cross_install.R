cross_install_packages <- function(packages, lib, platform,
                                   ..., repos = NULL, r_version = NULL,
                                   error = TRUE, upgrade = FALSE) {
  if (lib %in% .libPaths()) {
    stop("Do not use cross_install_packages to install into current library")
  }

  r_version <- check_r_version(r_version)

  ## NOTE: Getting linux support in properly requires an abstracted
  ## interface to something like buildr or rhub
  platform <- match.arg(platform,
                        c("windows", "macosx", "macosx/mavericks", "linux"))

  if (is.null(repos)) {
    ## This should probably be https://cran.r-cloud.com
    repos <- getOption("repos", "https://cran.rstudio.com")
  }

  ## Then we get information about packages:
  db <- available_packages(repos, platform, r_version)

  dir.create(lib, FALSE, TRUE)

  ## TODO: work out logic here around package upgrades
  ##
  ##   upgrade = TRUE: check all package versions that can be increased
  ##   upgrade = FALSE: don't do so, but let's check to see if all prereqs
  ##                    are here.
  ##   upgrade = NA/NULL: skip any checking and just do missing?  Or could do
  ##                 with missing_only argument perhaps?

  plan <- cross_install_plan(packages, db, lib)
  if (any(plan$compile)) {
    stop("Packages need compilation; cannot cross-install: ",
         paste(plan$packages[plan$compile], collapse=", "))
  }

  for (i in seq_along(plan$packages)) {
    p <- plan$packages[[i]]
    provisionr_log("cross", p)
    cross_install_package(p, db, lib, plan$binary[[i]], platform)
  }

  plan
}

base_packages <- function() {
  rownames(installed.packages(priority = c("base", "recommended")))
}

check_r_version <- function(r_version) {
  if (is.null(r_version)) {
    r_version <- getRversion()
  } else if (is.character(r_version)) {
    r_version <- numeric_version(r_version)
  } else if (!inherits(r_version, "numeric_version")) {
    stop("Invalid type for r_version")
  }
  if (length(r_version) != 1L) {
    stop("Expected a single version for r_version")
  }
  v <- unclass(r_version)[[1]]
  len <- length(v)
  if (len < 2 || len > 3) {
    stop("Unexpected version length")
  }
  v[1:2]
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

lib_package_version <- function(package, lib) {
  path <- file.path(lib, package, "DESCRIPTION")
  if (file.exists(path)) {
    package_version(read.dcf(path, fields = "Version"))
  } else {
    NA
  }
}

## TODO: memoise this because it's quite slow
available_packages <- function(repos, platform, r_version) {
  provisionr_log("download", "package database")
  pkgs_src <- available.packages(contrib_url(repos, "src", NULL))
  if (platform == "linux") {
    pkgs_bin <- pkgs_src[integer(0), ]
  } else {
    r_version_str <- paste(r_version[1:2], collapse = ".")
    pkgs_bin <- available.packages(contrib_url(repos, platform, r_version_str))
  }
  extra <- setdiff(rownames(pkgs_bin), rownames(pkgs_src))
  if (length(extra) > 0L) {
    pkgs_all <- rbind(pkgs_src, pkgs_bin[extra, ])
  } else {
    pkgs_all <- pkgs_src
  }
  list(all = pkgs_all, src = pkgs_src, bin = pkgs_bin)
}

contrib_url <- function(repos, platform, r_version_str) {
  ## platform should be:
  ##   src
  ##   windows
  ##   macosx
  ##   macosx/mavericks
  if (platform == "src") {
    path <- "src/contrib"
  } else {
    path <- file.path("bin", platform, "contrib", r_version_str)
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

cross_install_plan <- function(packages, db, lib) {
  installed <- c(.packages(TRUE, lib), base_packages())

  msg <- setdiff(packages, c(db$all[, "Package"], installed))
  if (length(msg) > 0L) {
    stop(sprintf("Can't find installation candidate for: %s",
                 paste(msg, collapse=", ")))
  }

  packages <- setdiff(recursive_deps(packages, db$all), installed)
  msg <- setdiff(packages, db$all[, "Package"])
  if (length(msg) > 0L) {
    stop(sprintf("Can't find installation candidate for dependencies: %s",
                 paste(msg, collapse=", ")))
  }

  binary <- packages %in% db$bin
  compile <- rep_len(FALSE, length(packages))
  if (any(!binary)) {
    j <- match(packages[!binary], db$src[, "Package"])
    compile[!binary] <- db$src[j, "NeedsCompilation"] == "yes"
  }

  list(packages = packages,
       binary = binary,
       compile = compile)
}

cross_install_package <- function(package, db, lib, binary, platform) {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add = TRUE)

  db_use <- db[[if (binary) "bin" else "src"]]
  x <- as.list(db_use[match(package, db_use[, "Package"]), ])

  ext <- if (!binary) "tar.gz" else if (platform == "windows") "zip" else "tgz"
  url <- sprintf("%s/%s_%s.%s", x$Repository, x$Package, x$Version, ext)

  if (grepl("file://", url)) {
    path <- file_unurl(url)
  } else {
    path <- file.path(tmp, basename(url))
    download_file(url, path)
  }

  if (binary) {
    unzip(path, exdir = lib)
  } else {
    ## NOTE: This would probably be better done with callr::rcmd_safe
    ## but it does not yet deal with stderr nicely
    env <- c(R_LIBS_USER=paste(c(.libPaths()), collapse = .Platform$path.sep),
             CYGWIN = "nodosfilewarning")
    env <- sprintf("%s=%s", names(env), unname(env))
    args <- c("CMD", "INSTALL", "--no-test-load",
              paste0("--library=", shQuote(normalizePath(lib, "/"))),
              shQuote(normalizePath(path)))
    ok <- system2(file.path(R.home(), "bin", "R"), args, env = env)
    if (ok != 0L) {
      stop(sprintf("Command failed (code: %d)", ok))
    }
  }

  invisible(TRUE)
}
