##' Download a fraction of CRAN to serve locally.
##'
##' @title Download some of CRAN
##' @param packages Character vector of packages to download
##' @param path Destination
##' @param r_version Target R version
##' @param target Target platform (use "ALL" for all platforms)
##' @param suggests Include suggested packages too?
##' @param package_sources A \code{provisionr::package_sources} object
##' @param progress Control progress bar printing
##' @export
download_cran <- function(packages, path, r_version = NULL,
                          target = "windows",
                          suggests = FALSE,
                          package_sources = NULL,
                          progress = NULL) {
  ## TODO: something to organise pruning....
  ## TODO: handle vectorised target and r_version
  ## TODO: sentinal value for all mac (perhaps macosx)
  ## TODO: ensure that "source" is a valid target
  version <- check_r_version(r_version)
  version_str <- r_version_str(version)

  dir.create(path, FALSE, TRUE)

  src <- prepare_package_sources(package_sources, FALSE, progress)
  repos <- prepare_repos(src)

  if (length(target) > 0L) {
    valid <- setdiff(valid_targets(version), "linux")
    if (identical(unname(target), "ALL")) {
      target <- valid
    } else {
      if ("macosx" %in% target) {
        target <- union(setdiff("macosx", target),
                        grep("^macosx", valid, value = TRUE))
      }
      err <- setdiff(target, valid)
      if (length(err)) {
        stop("Invalid target %s for R version %s",
             paste(err, collapse = ", "), as.character(version))
      }
    }
    binary_type <- vcapply(target, binary_type)
  }

  ## TODO: change this to suck less.
  url_src <- contrib_url(repos, "src", NULL)
  db_src <- available_packages(url_src, progress = progress)

  ## TODO: this will fail in the (very) unlikely situation where there
  ## are binary only packages to deal with.
  pkgs <- recursive_deps(packages, db_src, suggests)
  pkgs <- setdiff(pkgs, base_packages())

  ## source packages:
  dest_src <- contrib_url(path, "src", version_str)
  dir.create(dest_src, FALSE, TRUE)
  provisionr_log("download", "source")
  res <- download_packages(pkgs, dest_src, db_src, "source", progress)
  if (res) {
    tools::write_PACKAGES(dest_src, type = "source")
  }

  ## Then binary:
  for (i in seq_along(target)) {
    url_bin <- contrib_url(repos, target[[i]], version_str)
    db_bin <- available_packages(url_bin, progress = progress)
    dest_bin <- contrib_url(path, target[[i]], version_str)
    dir.create(dest_bin, FALSE, TRUE)
    provisionr_log("download", sprintf("binary: %s", target[[i]]))
    res <- download_packages(pkgs, dest_bin, db_bin, binary_type[[i]], progress)
    if (res) {
      ## This is insane:
      type_write <- sub("^mac.binary.*", "mac.binary", binary_type[[i]])
      tools::write_PACKAGES(dest_bin, type = type_write)
    }
  }

  path
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

download_packages <- function(pkgs, destdir, available, type, progress = NULL) {
  url <- package_url(pkgs, available, type)
  dest_file <- file.path(destdir, basename(url))
  if (any(is.na(url))) {
    message(sprintf("Skipping packages (type: %s) for packages: %s",
                    type, paste(pkgs[is.na(url)], collapse = ", ")))
  }
  i <- !file.exists(dest_file) & !is.na(url)
  download_files(url[i], destdir, labels = pkgs[i], progress = progress)
  any(i)
}

package_ext <- function(type) {
  if (type == "source") {
    "tar.gz"
  } else if (type == "win.binary") {
    "zip"
  } else {
    "tgz"
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
