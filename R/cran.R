## TODO: decide what a vectorised interface to version and type looks
## like here, especially with the macosx shit show.
download_cran <- function(packages, path, r_version = NULL,
                          type = "windows",
                          suggests = FALSE,
                          package_sources = NULL) {
  version <- check_r_version(r_version)
  version_str <- r_version_str(version)

  dir.create(path, FALSE, TRUE)

  browser()
  src <- prepare_package_sources(package_sources, FALSE)
  repos <- prepare_repos(src)

  if (!identical(as.vector(type), "windows")) {
    stop("not yet implemented")
  }
  binary_type <- binary_type(type)

  ## TODO: change this to suck less.
  url_src <- contrib_url(repos, "src", NULL)
  db <- available_packages(url_src)

  ## TODO: this will fail in the (very) unlikely situation where there
  ## are binary only packages to deal with.
  pkgs <- recursive_deps(packages, db, suggests)
  pkgs <- setdiff(pkgs, base_packages())

  ## source packages:
  dest_src <- contrib_url(path, "src", version_str)
  dir.create(dest_src, FALSE, TRUE)
  res <- download_packages(pkgs, dest_src, db, "source")
  if (res) {
    tools::write_PACKAGES(dest_src, type = "source")
  }

  ## Then binary:
  dest_bin <- contrib_url(path, type, version_str)
  dir.create(dest_bin, FALSE, TRUE)
  res <- download_packages(pkgs, dest_bin, db$bin, binary_type)
  if (res) {
    tools::write_PACKAGES(dest_bin, type = binary_type)
  }

  path
}

download_r <- function(path, type = "windows", r_version = NULL) {
  if (type != "windows") {
    stop("Not yet implemented")
  }
  dir.create(path, FALSE, TRUE)
  r_version <- check_r_version(r_version)
  version_str <- as.character(r_version)

  if (r_version == r_release()) {
    url <- sprintf("https://cran.r-project.org/bin/windows/base/R-%s-win.exe",
                   version_str)
  } else {
    url <- sprintf(
      "https://cran.r-project.org/bin/windows/base/old/%s/R-%s-win.exe",
      version_str, version_str)
  }
  dest <- file.path(path, basename(url))
  if (!file.exists(dest)) {
    download_file2(url, dest, progress = TRUE)
  }
  dest
}

download_rtools <- function(path, r_version = NULL) {
  dir.create(path, FALSE, TRUE)
  r_version <- check_r_version(r_version)

  v <- c("34" = numeric_version("3.3.0"),
         "33" = numeric_version("3.2.0"),
         "32" = numeric_version("3.1.0"))

  i <- which(r_version > v)
  if (length(i) == 0) {
    stop("R version is too old")
  }
  url <- sprintf("https://cran.r-project.org/bin/windows/Rtools/Rtools%s.exe",
                 names(i)[[1L]])
  dest <- file.path(path, basename(url))
  if (!file.exists(dest)) {
    download_file2(url, dest, progress = TRUE)
  }
  dest
}

package_url <- function(pkgs, available, type) {
  find_current <- function(p) {
    i <- which(available[, "Package"] == p)
    if (length(i) == 0L) {
      NA_integer_
    } else if (length(i) > 1L) {
      i[which.max(package_version(available[i, "Version"]))]
    } else {
      as.integer(i)
    }
  }

  idx <- vapply(pkgs, find_current, integer(1))
  ret <- sprintf("%s/%s_%s.%s",
                 available[idx, "Repository"],
                 pkgs,
                 available[idx, "Version"],
                 package_ext(type))
  ret[is.na(idx)] <- NA_character_
  ret
}

download_packages <- function(pkgs, destdir, available, type) {
  url <- package_url(pkgs, available, type)

  dest_file <- file.path(destdir, basename(url))
  if (any(is.na(url))) {
    message(sprintf("Skipping packages (type: %s) for packages: %s",
                    type, paste(pkgs[is.na(url)], collapse = ", ")))
  }
  fetch <- url[!file.exists(dest_file) & !is.na(url)]

  is_local <- grepl("file://", fetch)
  if (any(is_local)) {
    file.copy(file_unurl(fetch[is_local]), destdir)
  }

  is_remote <- !is_local
  if (any(is_remote)) {
    for (u in fetch[is_remote]) {
      download_file2(u, file.path(destdir, basename(u)), progress = TRUE)
    }
  }

  length(fetch) > 0L
}

package_ext <- function(type) {
  if (type == "source") {
    "tar.gz"
  } else if (type == "win.binary") {
    "zip"
  } else {
    "tar.gz"
  }
}

binary_type <- function(type) {
  switch(type,
         src = "source",
         windows = "win.binary",
         "macosx" = "mac.binary",
         "macosx/mavericks" = "mac.binary.mavericks",
         "macosx/el-capitan" = "mac.binary.el-capitan")
}
