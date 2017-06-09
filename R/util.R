is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}

is_mac <- function() {
  Sys.info()[["sysname"]] == "Darwin"
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

is_file_url <- function(x) {
  grepl("^file://", x)
}

is_directory <- function(path) {
  file.info(path, extra_cols = FALSE)$isdir
}

check_r_version <- function(version) {
  if (is.null(version)) {
    version <- getRversion()
  } else if (is.character(version)) {
    version <- numeric_version(version)
  } else if (!inherits(version, "numeric_version")) {
    stop("Invalid type for version")
  }
  if (length(version) != 1L) {
    stop("Expected a single version number")
  }
  version
}

r_version_str <- function(version, n = 2L) {
  v <- unclass(check_r_version(version))[[1]]
  if (length(v) < n) {
    stop("Unexpected version length")
  }
  paste(v[seq_len(n)], collapse = ".")
}

extract <- function(file, ...) {
  assert_scalar_character(file)
  if (grepl("\\.zip$", file, ignore.case = TRUE)) {
    unzip(file, ...)
  } else {
    untar(file, ...)
  }
}

string_starts_with <- function(a, b) {
  if (length(b) != 1L) {
    vlapply(b, string_starts_with, a = a)
  } else {
    substr(a, 1, nchar(b)) == b
  }
}

clean_matrix <- function(m, cols) {
  msg <- setdiff(cols, colnames(m))
  if (length(msg) > 0L) {
    extra <- matrix(m[integer(0)], nrow(m), length(msg),
                    dimnames = list(NULL, msg))
    m <- cbind(m, extra)
  }
  m[, cols, drop = FALSE]
}
