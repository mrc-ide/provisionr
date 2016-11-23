## Try to fetch things from a local cran mirror.  Create this with
## `make_local_cran()` which downloads the packages required to run
## the tests.  There are a few 10s of MB of files (20MB as of Nov
## 2016).
if (file.exists("local_cran")) {
  options(repos = c(file_url("local_cran"), "https://cran.rstudio.com"))
} else {
  options(repos = "https://cran.rstudio.com")
}

Sys.setenv(R_TESTS = "")

alter_package_version <- function(path, increase) {
  desc <- file.path(path, "DESCRIPTION")
  d <- read.dcf(desc)
  v <- numeric_version(d[, "Version"])
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
  d[, "Version"] <- as.character(v)
  write.dcf(d, desc)
  invisible(v)
}
