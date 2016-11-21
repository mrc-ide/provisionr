file_unurl <- function(url) {
  if (is_windows()) {
    sub("^file:///", "", url)
  } else {
    sub("^file://", "", url)
  }
}

is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}

## TODO: All the usual crap of making this robust enough to rely on.
download_file <- function(url, ..., destfile = NULL, keep_ext = FALSE) {
  if (is.null(destfile)) {
    ext <- if (grepl("\\.tar\\.gz$", url)) "tar.gz" else tools::file_ext(url)
    destfile <- tempfile(fileext = paste0(".", ext))
  }
  code <- download.file(url, destfile, mode = "wb", ...)
  if (code != 0L) {
    stop("error downloading file")
  }
  destfile
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
