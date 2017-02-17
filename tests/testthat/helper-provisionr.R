alter_package_version <- function(path, increase) {
  desc <- file.path(path, "DESCRIPTION")
  d <- read.dcf(desc)
  v <- alter_version(d[, "Version"], increase)
  d[, "Version"] <- v
  write.dcf(d, desc)
  invisible(numeric_version(v))
}

alter_version <- function(v, increase) {
  if (inherits(v, "numeric_version")) {
    as_version <- TRUE
  } else {
    v <- numeric_version(v)
    as_version <- FALSE
  }
  if (increase) {
    i <- length(unclass(v)[[1L]])
    v[[1L, i]] <- v[[1L, i]] + 1L
  } else {
    for (i in rev(seq_along(unclass(v)[[1L]]))) {
      if (v[[1L, i]] > 0L) {
        v[[1L, i]] <- v[[1L, i]] - 1L
        break
      }
    }
  }
  if (as_version) v else as.character(v)
}

make_local_cran <- function() {
  path <- "local_cran"
  packages <- c("devtools", "progress", "ape")
  dir.create(path, FALSE, TRUE)
  on.exit(unlink(path, recursive = TRUE))

  version <- check_r_version(NULL)
  version_str <- r_version_str(version)
  repo <- "https://cran.rstudio.com"
  db <- available_packages(repo, "windows", NULL)

  pkgs <- recursive_deps(packages, db$all)

  url_src <- contrib_url(repo, "src", version_str)
  url_bin <- contrib_url(repo, "windows", version_str)
  dest_src <- contrib_url(path, "src", version_str)
  dest_bin <- contrib_url(path, "windows", version_str)
  dir.create(dest_src, FALSE, TRUE)
  dir.create(dest_bin, FALSE, TRUE)

  download.packages(pkgs, dest_src, db$src, repo, url_src, type = "source")
  download.packages(pkgs, dest_bin, db$bin, repo, url_bin, type = "win.binary")

  tools::write_PACKAGES(dest_src, type = "source")
  tools::write_PACKAGES(dest_bin, type = "win.binary")
  on.exit()
}

## Try to fetch things from a local cran mirror.  Create this with
## `make_local_cran()` which downloads the packages required to run
## the tests.  There are a few 10s of MB of files (20MB as of Nov
## 2016).
Sys.setenv(R_TESTS = "")
if (!file.exists("local_cran")) {
  message("Building local CRAN repository for tests")
  make_local_cran()
} else {
  message("Found local CRAN repository")
}
options(repos = c(file_url("local_cran"), "https://cran.rstudio.com"))
