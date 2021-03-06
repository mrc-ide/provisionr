assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call. = FALSE)
  }
}

assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("%s must be logical", name), call. = FALSE)
  }
}

assert_numeric <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", name), call. = FALSE)
  }
}
assert_nonmissing <- function(x, name = deparse(substitute(x))) {
  if (any(is.na(x))) {
    stop(sprintf("%s must not be NA", name), call. = FALSE)
  }
}

assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
  assert_nonmissing(x, name)
}

assert_scalar_numeric <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
  assert_nonmissing(x, name)
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("%s must be character", name), call. = FALSE)
  }
}

assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
  assert_nonmissing(x, name)
}

assert_is <- function(x, type, name = deparse(substitute(x))) {
  if (!(inherits(x, type))) {
    stop(sprintf("%s must inherit from %s",
                 name, paste(type, collapse = " / ")))
  }
}

match_value <- function(x, choices, name = deparse(substitute(x))) {
  assert_scalar_character(x, name)
  i <- match(x, choices)
  if (is.na(i)) {
    stop(sprintf("%s must be one of {%s}",
                 name, paste(choices, collapse = ", ")), call. = FALSE)
  }
  choices[[i]]
}
