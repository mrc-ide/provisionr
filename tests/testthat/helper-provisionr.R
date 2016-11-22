options(repos = "https://cran.rstudio.com")
Sys.setenv(R_TESTS="")

drop_package_version <- function(path) {
  desc <- file.path(path, "DESCRIPTION")
  d <- read.dcf(desc)
  v <- numeric_version(d[, "Version"])
  for (i in rev(seq_along(unclass(v)[[1L]]))) {
    if (v[[1L, i]] > 0L) {
      v[[1L, i]] <- v[[1L, i]] - 1L
      break
    }
  }
  d[, "Version"] <- as.character(v)
  write.dcf(d, desc)
}
