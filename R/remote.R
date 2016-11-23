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
drat_build <- function(specs, path) {
  drat_repo_init(path)
  desc <- list()

  st <- drat_storr(path)

  while (length(specs) > 0L) {
    for (x in lapply(specs, parse_remote)) {
      provisionr_log("drat", x$spec)
      pkg <- download_package(x)
      d <- as.list(extract_DESCRIPTION(pkg)[1L, ])
      d$tgz <- basename(pkg)
      d$md5 <- unname(tools::md5sum(pkg))
      d$timestamp <- Sys.time()
      drat::insertPackage(pkg, path, commit = FALSE)
      desc[[x$spec]] <- d
      ## TODO: I think that I need to put the dependent repos in here
      ## too so that I can query them later?
      st$set(x$spec, d)
    }

    ## Then comes a fairly ugly bit of collecting up all the extra bits:
    extra <- lapply(desc, function(x)
      if ("Remotes" %in% names(x)) parse_remotes(x[["Remotes"]]) else NULL)

    specs <- unique(setdiff(unlist(extra, TRUE, FALSE), names(desc)))
  }

  desc
}

github_build_package <- function(x) {
  tmp <- tempfile()
  dir.create(tmp)
  pkg <- download_file(x$url_package, keep_ext = TRUE)
  unzip(pkg, exdir = tmp)
  file.remove(pkg)
  target <- dir(tmp, full.names = TRUE)
  if (!is.null(x$subdir)) {
    target <- file.path(target, subdir)
  }
  R_build(target)
}

## TODO: bunch of duplication with build_package (in install.R)
R_build <- function(path) {
  owd <- setwd(dirname(path))
  on.exit(setwd(owd))
  prev <- dir(full.names = TRUE)
  opts <- "--no-build-vignettes"
  cmd <- call_r(c("--vanilla", "CMD", "build", opts, basename(path)))
  ## The normalizePath here should not be needed according to ?dir
  ## (with full.names = TRUE) but it does seem to be which is sad.
  normalizePath(setdiff(dir(full.names = TRUE), prev))
}

github_url_zip <- function(x) {
  sprintf("https://github.com/%s/%s/archive/%s.zip",
          x$username, x$repo, x$branch %||% "master")
}

github_url_zip_api <- function(x) {
  sprintf("https://api.github.com/repos/%s/%s/zipball/%s",
          x$username, x$repo, x$ref %||% "master")
}

github_url_description <- function(x) {
  ret <- sprintf("https://raw.githubusercontent.com/%s/%s/%s",
                 x$username, x$repo, x$branch %||% "master")
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
      pkg <- build_package(x$url_package)
    } else {
      pkg <- x$url_package
    }
  } else {
    stop("[provisionr bug] invalid type") # nocov
  }
  pkg
}

remote_url <- function(type, x) {
  switch(type,
         github = remote_url_host(x, "github.com"),
         bitbucket = remote_url_host(x, "bitbucket.com"),
         url = x,
         stop("Can't convert type ", type))
}

remote_url_host <- function(x, host) {
  p <- remotes::parse_github_repo_spec(x)
  ret <- sprintf("https://%s/%s/%s", host, p$user, p$repo)
  if (!is.null(p$pull)) {
    ret <- file.path(ret, "pulls", p$pull)
  } else {
    ret <- file.path(ret, "tree", p$ref %||% "master")
  }
  if (!is.null(p$subdir)) {
    ret <- file.path(p$subdir)
  }
  ret
}

drat_repo_init <- function(path) {
  dir.create(file.path(path, "src", "contrib"), FALSE, TRUE)
}

drat_insert_package <- function(package, path) {
  is_zip <-
  drat::insertPackage(package, path)
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
