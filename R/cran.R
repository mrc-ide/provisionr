download_cran <- function(packages, path, r_version = NULL,
                          platform = "windows",
                          suggests = FALSE,
                          package_sources = NULL) {
  version <- check_r_version(r_version)
  version_str <- r_version_str(version)

  dir.create(path, FALSE, TRUE)

  src <- prepare_package_sources(package_sources, FALSE)
  repos <- prepare_repos(src)

  if (!identical(as.vector(platform), "windows")) {
    stop("not yet implemented")
  }
  binary_type <- "win.binary"

  db <- available_packages(repos, platform, version)

  pkgs <- recursive_deps(packages, db$all, suggests)

  ## source packages:
  dest_src <- contrib_url(path, "src", version_str)
  dir.create(dest_src, FALSE, TRUE)
  res <- download_packages(pkgs, dest_src, db$src, "source")
  if (res) {
    tools::write_PACKAGES(dest_src, type = "source")
  }

  ## Then binary:
  dest_bin <- contrib_url(path, platform, version_str)
  dir.create(dest_bin, FALSE, TRUE)
  res <- download_packages(pkgs, dest_bin, db$bin, binary_type)
  if (res) {
    tools::write_PACKAGES(dest_bin, type = binary_type)
  }

  path
}

download_r <- function(path, platform = "windows", r_version = NULL) {
  if (platform != "windows") {
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
  ## TODO: be noisier about skipping missing URLs!
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
