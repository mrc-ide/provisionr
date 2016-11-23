package_sources <- function(cran = NULL, repos = NULL,
                            github = NULL, local = NULL) {
  if (is.null(cran)) {
    cran <- "https://cran.rstudio.com"
  } else {
    assert_scalar_character(cran)
  }

  if (!is.null(repos)) {
    names(repos) <- repos
    is_drat <- grepl("^drat://", repos)
    repos[is_drat] <- sprintf("https://%s.github.io/drat/",
                              sub("^drat://", "", repos[is_drat]))
    if (!all(grepl("^[a-z]+://", repos))) {
      stop("Missing url scheme")
    }
  }

  ## Collect all the spec information here into a single vector, I
  ## think; that'll be easier to modify.
  spec <- NULL

  if (!is.null(github)) {
    tmp <- lapply(github, parse_remote)
    err <- vcapply(tmp, "[[", "type") != "github"
    if (any(err)) {
      stop("Non-github spec ", paste(github[err], collpse = ", "))
    }
    spec <- c(spec, vcapply(tmp, "[[", "spec"))
  }

  if (!is.null(local)) {
    if (!all(file.exists(local))) {
      stop("Missing local files")
    }
    spec <- c(spec, paste0("local::", local))
  }

  self <- list(cran = cran, repos = repos, spec = spec)
  self$build <- function(path, refresh = FALSE) {
    ret <- local_drat(self, path)
    skip <- !refresh && file.exists(path) && all(vlapply(spec, ret$db$exists))
    if (!skip) {
      ret$build()
    }
    ret
  }
  class(self) <- "package_sources"
  self
}

local_drat <- function(src, path) {
  ## This should keep track of when it was last built, what is in it,
  ## etc.
  self <- list(src = src,
               path = path,
               db = drat_storr(path))
  self$build <- function() {
    drat_build(src$spec, path)
    self
  }

  class(self) <- "local_drat"
  self
}

##' @export
print.package_sources <- function(x, ...) {
  cat(paste(as.character(x), collapse = "\n"), "\n")
  invisible(x)
}

##' @export
print.local_drat <- function(x, ...) {
  cat(paste(as.character(x), collapse = "\n"), "\n")
  invisible(x)
}

##' @export
as.character.package_sources <- function(x, ...) {
  f <- function(nm, list) {
    c(sprintf("  %s:", nm),
      sprintf("    - %s", list[[nm]]))
  }
  str <- c("<package_sources>",
           sprintf("  cran: %s", x$cran),
           if (length(x$repos) > 0L) f("repos", x))
  if (length(x$spec) > 0L) {
    x_spec <- split_spec(x$spec)
    x_spec <- split(x_spec[, "value"], x_spec[, "type"])
    str <- c(str,
             unlist(lapply(names(x_spec), f, x_spec), use.names = FALSE))
  }
  str
}

##' @export
as.character.local_drat <- function(x, ...) {
  k <- x$db$list()
  v <- lapply(k, x$db$get)
  dat <- data.frame(name = vcapply(v, "[[", "Package"),
                    version = vcapply(v, "[[", "Version"),
                    updated = vcapply(v, function(el)
                      prettyunits::time_ago(el$timestamp)),
                    stringsAsFactors = FALSE)
  str <- c("<local_drat>",
           "  source:",
           paste("    ", as.character(x$src)),
           "  contents:",
           paste("    ", capture.output(print(dat, row.names = FALSE))))
  str
}
