## NOTE: this basically reproduces the drat.builder package; consider
## moving it into there, or depending on this package from that one.

## TODO: Now that timestamps are installed, let's enable refresh of
## packages that are too old.

## From each remote I need to get the url of the description and
## organise the name of the function to do the installation with.
## Though realistically all we're really interested in is the system
## dependencies now it's a little easier as we can simply require use
## of Remotes I hope.
parse_remote <- function(spec) {
  stopifnot(length(spec) == 1L)

  re_prefix <- "^([a-z]+)::(?!//)(.+)$"
  if (grepl(re_prefix, spec, perl = TRUE)) {
    src <- sub(re_prefix, "\\1", spec, perl = TRUE)
    spec <- sub(re_prefix, "\\2", spec, perl = TRUE)
  } else {
    src <- "github"
  }

  ## Valid sources:
  ##
  ## * github
  ## * bitbucket
  ## * url
  ## * svn
  ## * local (must be a file path)

  valid_src <- c("local", "url", "github")
  if (!(src %in% valid_src)) {
    stop(sprintf("Invalid remote source '%s'", src))
  }

  if (src == "github") {
    res <- remotes::parse_github_repo_spec(spec)
    res$url_package <- github_url_zip(res)
    res$url_package_api <- github_url_zip_api(res)
    res$url_description <- github_url_description(res)
  } else if (src == "url") {
    res <- list(url_package = spec,
                url_description = NULL)
  } else { # local
    if (!file.exists(spec)) {
      stop("path does not exist: ", spec)
    }
    res <- list(url_package = spec,
                url_description = NULL,
                is_directory = is_directory(spec))
    if (res$is_directory) {
      res$url_description <- file.path(spec, "DESCRIPTION")
    }
  }

  res$type <- src
  res$spec <- sprintf("%s::%s", src, spec)

  res
}

parse_remotes <- function(str) {
  stopifnot(length(str) == 1L)
  f <- function(x) {
    re_prefix <- "^([a-z]+)::(?!//)(.+)$"
    if (grepl(re_prefix, x, perl = TRUE)) x else sprintf("github::%s", x)
  }
  lapply(strsplit(str, "\\s*,\\s*")[[1]], f)
}

drat_storr <- function(path) {
  storr::storr_rds(path_drat_storr(path), mangle_key = TRUE)
}

## TODO: It's possible that this does too much (with the remotes), but
## such is life.
##
## TODO: It might be nice to go through and check to see if it's worth
## refreshing the store here; we could do that if we have a fast way
## of ripping through the directories and determine if any file has
## changed.
drat_build <- function(specs, path, force) {
  drat_repo_init(path)
  desc <- list()

  db <- drat_storr(path)

  while (length(specs) > 0L) {
    for (s in specs) {
      desc[[s]] <- drat_build_package(s, path, db, force)
    }

    ## Then comes a fairly ugly bit of collecting up all the extra bits:
    extra <- lapply(desc, function(x)
      if ("Remotes" %in% names(x)) parse_remotes(x[["Remotes"]]) else NULL)

    specs <- unique(setdiff(unlist(extra, TRUE, FALSE), names(desc)))
  }

  desc
}

drat_build_package <- function(spec, path, db, force = FALSE) {
  x <- parse_remote(spec)

  provisionr_log("drat", x$spec)
  ## Here, for github packages, I think we should see if we can avoid
  ## downloading the package by hitting the github API and getting the
  ## current version.  I don't depend on the necessary packages to do
  ## this though; we'll need to depend on jsonlite and possibly on
  ## curl
  pkg <- download_package(x)
  d <- as.list(extract_DESCRIPTION(pkg)[1L, ])

  ## The rule here needs to be that we update things if either:
  ##
  ## * the version number changes
  ## * some force flag is TRUE
  ## * the contents of the package changes
  if (!force && db$exists(spec)) {
    prev <- db$get(spec)
    ## TODO: This will need testing with binaries and will not work at
    ## present!
    if (numeric_version(prev$Version) == numeric_version(d$Version)) {
      prev_file <- file.path(contrib.url(path, "source"), prev$tgz)
      if (package_same_contents(prev_file, pkg, d$Package)) {
        return(prev)
      }
    }
  }

  d$tgz <- basename(pkg)
  d$md5 <- unname(tools::md5sum(pkg))
  d$timestamp <- Sys.time()
  drat::insertPackage(pkg, path, commit = FALSE)
  ## TODO: I think that I need to put the dependent repos in here
  ## too so that I can query them later?
  db$set(x$spec, d)
  d
}

github_build_package <- function(x) {
  tmp <- tempfile()
  dir.create(tmp)
  pkg <- download_file(x$url_package, keep_ext = TRUE)
  unzip(pkg, exdir = tmp)
  file.remove(pkg)
  unpacked <- target <- dir(tmp, full.names = TRUE)
  on.exit(unlink(unpacked, recursive = TRUE))
  if (!is.null(x$subdir)) {
    target <- file.path(target, x$subdir)
  }
  build_package(target, dest = tmp)
}

github_url_zip <- function(x) {
  sprintf("https://github.com/%s/%s/archive/%s.zip",
          x$username, x$repo, x$ref %||% "master")
}

github_url_zip_api <- function(x) {
  sprintf("https://api.github.com/repos/%s/%s/zipball/%s",
          x$username, x$repo, x$ref %||% "master")
}

github_url_description <- function(x) {
  ret <- sprintf("https://raw.githubusercontent.com/%s/%s/%s",
                 x$username, x$repo, x$ref %||% "master")
  if (!is.null(x$subdir)) {
    ret <- file.path(ret, x$subdir)
  }
  file.path(ret, "DESCRIPTION")
}

download_package <- function(x) {
  ## When downloading files, keep the same extension, or we'll confuse
  ## things later.
  if (x$type == "github") {
    pkg <- github_build_package(x)
  } else if (x$type == "url") {
    pkg <- download_file(x$url_package, keep_ext = TRUE)
  } else if (x$type == "local") {
    if (x$is_directory) {
      ## NOTE: A disadvantage of doing this is that it will rebuild
      ## the package *every time*, adding a Packaged: field to the
      ## description which will change the md5 and make it look like
      ## things have changed when they have not really.  I'll consider
      ## writing something to hash a directory that we can use to
      ## check to see if it's worth rebuilding the package.
      pkg <- build_package(x$url_package, dest = tempfile())
    } else {
      pkg <- x$url_package
    }
  } else {
    stop("[provisionr bug] invalid type") # nocov
  }
  pkg
}

drat_repo_init <- function(path) {
  dir.create(file.path(path, "src", "contrib"), FALSE, TRUE)
  provisionr_log("drat", paste0("@", path))
}

extract_DESCRIPTION <- function(filename) {
  dest <- tempdir()
  if (grepl("\\.zip$", filename)) {
    files <- unzip(filename, list = TRUE)
    file <- grep("^[^/]+/DESCRIPTION$", files[, "Name"], value = TRUE)
    unzip(filename, file, exdir = dest)
  } else {
    files <- untar(filename, list = TRUE)
    file <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
    untar(filename, file, exdir = dest)
  }
  read.dcf(file.path(dest, file))
}

path_drat_storr <- function(path_drat) {
  file.path(path_drat, "storr")
}

split_spec <- function(x) {
  re <- "(^[^:]+)::(.+)$"
  stopifnot(all(grepl(re, x)))
  cbind(type = sub(re, "\\1", x),
        value = sub(re, "\\2", x))
}

build_package <- function(path, vignettes = FALSE, manual = FALSE,
                          dest = dirname(path)) {
  if (!file.exists(dest)) {
    dir.create(dest, FALSE, TRUE)
  } else if (!is_directory(dest)) {
    stop("If it exists already, 'dest' must be a directory")
  }

  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc)) {
    stop("Did not find a valid package at ", path)
  }
  path_abs <- normalizePath(path, mustWork = TRUE)

  dat <- read.dcf(desc, c("Package", "Version"))
  dest_pkg <- sprintf("%s_%s.tar.gz", dat[, "Package"], dat[, "Version"])

  if (dest != ".") {
    owd <- setwd(dest)
    on.exit(setwd(owd))
  }

  opts <- c(character(0),
            if (!vignettes) "--no-build-vignettes",
            if (!manual) "--no-manual")

  cmd <- call_r(c("--vanilla", "CMD", "build", opts, path_abs))
  normalizePath(file.path(dest, dest_pkg), mustWork = TRUE)
}

## This is protection for the case where we read packages from a local
## drat on windows but it tries to read the binary directory and
## fails.  I'm not 100% sure that this should be needed, but it is at
## least on R 3.2.x windows.
drat_add_empty_bin <- function(path) {
  path_PACKAGES <- file.path(path, "PACKAGES")
  if (!file.exists(path_PACKAGES)) {
    dir.create(path, FALSE, TRUE)
    writeLines(character(0), path_PACKAGES)
  }
}

package_same_contents <- function(path1, path2, package) {
  files1 <- extract(path1, list = TRUE)
  files2 <- extract(path2, list = TRUE)
  if (!setequal(files1, files2)) {
    return(FALSE)
  }
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  on.exit(unlink(c(tmp1, tmp2), recursive = TRUE))
  extract(path1, exdir = tmp1)
  extract(path2, exdir = tmp2)

  ## Next, we try and rewrite the DESCRIPTION file a bit.  The key
  ## things I need to remove are "Packaged" but it makes sense to rip
  ## out "Built" too and that might be an issue if injecting a binary.
  clean_description(file.path(tmp1, package, "DESCRIPTION"))
  clean_description(file.path(tmp2, package, "DESCRIPTION"))

  hash1 <- md5sum2(file.path(tmp1, files1))
  hash2 <- md5sum2(file.path(tmp2, files1)) # NOTE: files1 to avoid sorting issues

  all(hash1 == hash2)
}

clean_description <- function(path) {
  dat <- read.dcf(path)
  dat <- dat[, setdiff(colnames(dat), c("Packaged", "Built")), drop = FALSE]
  write.dcf(dat, path)
}

md5sum2 <- function(paths) {
  ret <- character(length(paths))
  i <- !is_directory(paths)
  ret[i] <- unname(tools::md5sum(paths[i]))
  ret
}
