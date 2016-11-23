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
download_file <- function(url, ..., progress = NULL, destfile = NULL,
                          keep_ext = FALSE) {
  progress <- progress %||% getOption("provisionr.download.progress", FALSE)
  if (is.null(destfile)) {
    ext <- if (grepl("\\.tar\\.gz$", url)) "tar.gz" else tools::file_ext(url)
    destfile <- tempfile(fileext = paste0(".", ext))
  }
  code <- download.file(url, destfile, mode = "wb", quiet = !progress, ...)
  if (code != 0L) {
    stop("error downloading file")
  }
  destfile
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

## Convert a path to a file:// that R can understand.  Some care is
## needed on windows.  This will create a path with *three* leading
## slashes.
file_url <- function(path) {
  full_path <- normalizePath(path, winslash = "/")
  paste0("file://", if (substr(full_path, 1, 1) == "/") "" else "/", full_path)
}

file_unurl <- function(url) {
  if (is_windows()) {
    sub("^file:///", "", url)
  } else {
    sub("^file://", "", url)
  }
}

is_directory <- function(path) {
  file.info(path, extra_cols = FALSE)$isdir
}
