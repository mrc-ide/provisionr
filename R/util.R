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
download_file <- function(...) {
  download.file(..., mode = "wb")
}
