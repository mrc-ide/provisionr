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

read_package_version <- function(path) {
  numeric_version(read.dcf(file.path(path, "DESCRIPTION"), "Version")[[1]])
}

make_local_cran <- function(path = "local_cran") {
  packages <- c("devtools", "progress", "ape", "lattice", "nlme")
  src <- package_sources(cran = "https://cran.rstudio.com")
  download_cran(packages, path, NULL, "ALL", FALSE, src)
}

## Try to fetch things from a local cran mirror.  Create this with
## `make_local_cran()` which downloads the packages required to run
## the tests.  There are a few 10s of MB of files (20MB as of Nov
## 2016).
Sys.setenv(R_TESTS = "")
if (is_windows()) {
  Sys.setenv(TAR = "internal")
}
if (!file.exists("local_cran")) {
  message("Building local CRAN repository for tests")
  make_local_cran()
} else {
  message("Found local CRAN repository")
}
options(repos = c(CRAN = file_url("local_cran"),
                  CRAN = "https://cran.rstudio.com"),
        install.packages.check.source = "no")

PROGRESS <- FALSE
