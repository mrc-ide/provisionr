## TODO: through here, need a mode where we don't throw errors but
## return information on what could not be installed due to having no
## installation candidates.
cross_install_packages <- function(packages, lib, platform,
                                   ..., repos = NULL, version = NULL,
                                   installed_action = "skip") {
  if (lib %in% .libPaths()) {
    stop("Do not use cross_install_packages to install into current library")
  }
  dir.create(lib, FALSE, TRUE)

  version <- check_r_version(version)

  ## NOTE: Getting linux support in properly requires an abstracted
  ## interface to something like buildr or rhub
  platform <- match.arg(platform, valid_platforms())

  ## Hmm, perhaps remove this block:
  if (is.null(repos)) {
    ## This should probably be https://cran.r-cloud.com
    repos <- getOption("repos", "https://cran.rstudio.com")
  }
  ## Then we get information about packages:
  db <- available_packages(repos, platform, version)

  dir.create(lib, FALSE, TRUE)

  plan <- cross_install_plan(packages, db, lib, installed_action)
  if (any(plan$compile)) {
    stop("Packages need compilation; cannot cross-install: ",
         paste(plan$packages[plan$compile], collapse=", "))
  }

  for (i in seq_along(plan$packages)) {
    p <- plan$packages[[i]]
    cross_install_package(p, db, lib, plan$binary[[i]], platform)
  }

  plan
}

base_packages <- function() {
  rownames(installed.packages(priority = c("base", "recommended")))
}

check_r_version <- function(version) {
  if (is.null(version)) {
    version <- getRversion()
  } else if (is.character(version)) {
    version <- numeric_version(version)
  } else if (!inherits(version, "numeric_version")) {
    stop("Invalid type for version")
  }
  if (length(version) != 1L) {
    stop("Expected a single version for version")
  }
  v <- unclass(version)[[1]]
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
available_packages <- function(repos, platform, version) {
  provisionr_log("download", "package database")

  is_local <- grepl("^(/|file://)", repos)
  if (any(is_local)) {
    i <- file.exists(repos)
    if (any(i)) {
      repos[i] <- file_url(repos[i])
    }
  }

  pkgs_src <- available.packages(contrib_url(repos, "src", NULL))
  if (is.null(platform) || platform == "linux") {
    pkgs_bin <- pkgs_src[integer(0), ]
  } else {
    version_str <- paste(version[1:2], collapse = ".")
    repos_bin <- contrib_url(repos, platform, version_str)
    if (any(is_local)) {
      lapply(file_unurl(repos_bin[is_local]), drat_add_empty_bin)
    }
    pkgs_bin <- available.packages(repos_bin)
  }
  extra <- setdiff(rownames(pkgs_bin), rownames(pkgs_src))
  if (length(extra) > 0L) {
    pkgs_all <- rbind(pkgs_src, pkgs_bin[extra, ])
  } else {
    pkgs_all <- pkgs_src
  }
  list(all = pkgs_all, src = pkgs_src, bin = pkgs_bin)
}

contrib_url <- function(repos, platform, version_str) {
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

  if (installed_action == "upgrade") {
    packages <- setdiff(packages, .packages(TRUE, lib))
  }

  binary <- packages %in% db$bin

  if (installed_action == "upgrade" || installed_action == "upgrade_all") {
    check <- intersect(packages, .packages(TRUE, lib))
    v_installed <- setNames(numeric_version(vcapply(
      file.path(lib, check, "DESCRIPTION"), read.dcf, "Version")), check)

    v_db <- setNames(character(length(check)), check)
    i <- match(check, packages)
    j <- binary[i]
    v_db[j] <- db$bin[check[j], "Version"]
    v_db[!j] <- db$src[check[!j], "Version"]
    v_db <- numeric_version(v_db)

    current <- v_installed >= v_db
    drop <- i[current]
    if (length(drop) > 0L) {
      packages <- packages[-drop]
      binary <- binary[-drop]
    }
  }

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
  provisionr_log("cross", package)
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
    download_file(url, destfile = path)
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
    call_system(file.path(R.home(), "bin", "R"), args, env = env)
  }

  invisible(TRUE)
}

drat_add_empty_bin <- function(path) {
  path_PACKAGES <- file.path(path, "PACKAGES")
  if (!file.exists(path_PACKAGES)) {
    dir.create(path, FALSE, TRUE)
    writeLines(character(0), path_PACKAGES)
  }
}

valid_platforms <- function() {
  c("windows", "macosx", "macosx/mavericks", "linux")
}
