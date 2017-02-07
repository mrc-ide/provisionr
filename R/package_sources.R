##' Collect information on package sources
##' @title Collect information on package sources
##'
##' @param cran A single URL for the CRAN repo to use.  If
##'   \code{NULL}, then this will use the rstudio mirror.
##'
##' @param repos A character vector of additional repositories to use.
##'   Repositories of the form \code{drat://username} will be
##'   translated to a drat repository hosted on github (e.g.,
##'   \code{https://username.github.io/drat})
##'
##' @param github A vector of github package specifications (e.g.,
##'   username/password).  The full syntax as implemented in
##'   \code{\link{parse_github_repo_spec}} is supported (packages in
##'   subdirectories, or referencing a particular branch, commit or
##'   tag).
##'
##' @param local A character vector of local files to include.  Can be
##'   directories or built packages (source or binary)
##'
##' @param expire Optional period, in days, to expire the local copy
##'   of the package.  If specified, then if a package was downloaded
##'   more than\code{expire} days ago when the local copy is checked,
##'   a new version will be downloaded.  Can be a fractional value
##'   (e.g., \code{expire = 0.04} for an expiry of around an hour).
##'
##' @export
package_sources <- function(cran = NULL, repos = NULL,
                            github = NULL, local = NULL,
                            expire = NULL) {
  R6_package_sources$new(cran, repos, github, local, expire)
}

R6_package_sources <- R6::R6Class(
  "package_sources",
  public = list(
    cran = NULL,
    repos = NULL,
    spec = NULL,
    local_drat = NULL,
    expire = NULL,

    initialize = function(cran, repos, github, local, expire) {
      if (is.null(cran)) {
        cran <- sanitise_options_cran()
      } else {
        ## No reason why this can't be a vector, though it does get a bit
        ## overlappy with repos
        assert_character(cran)
        if (length(cran) == 0) {
          stop("At least one cran repository must be given")
        }
      }
      self$cran <- cran

      if (!is.null(repos)) {
        assert_character(repos)
        names(repos) <- repos
        is_drat <- grepl("^drat://", repos)
        repos[is_drat] <- sprintf("https://%s.github.io/drat/",
                                  sub("^drat://", "", repos[is_drat]))
        if (!all(grepl("^[a-z]+://", repos))) {
          stop("Missing url scheme")
        }
        self$repos <- repos
      }

      ## Collect all the spec information here into a single vector, I
      ## think; that'll be easier to modify.
      spec <- NULL

      if (!is.null(github)) {
        tmp <- lapply(github, parse_remote)
        err <- vcapply(tmp, "[[", "type") != "github"
        if (any(err)) {
          stop("Non-github spec ", paste(github[err], collapse = ", "))
        }
        spec <- c(spec, vcapply(tmp, "[[", "spec"))
      }

      if (!is.null(local)) {
        if (!all(file.exists(local))) {
          stop("Missing local files: ",
               paste(local[!file.exists(local)], collapse = ", "))
        }
        spec <- c(spec, paste0("local::", local))
      }
      self$spec <- spec

      if (!is.null(expire)) {
        assert_scalar_numeric(expire)
        if (expire <= 0) {
          stop("'expire' must be positive")
        }
        self$expire <- expire
      }
    },

    needs_build = function(path = self$local_drat) {
      rebuild <- length(self$spec) > 0 &&
        (is.null(path) ||
         !all(drat_storr(path)$exists(self$spec)))
      if (!rebuild && !is.null(self$expire)) {
        db <- drat_storr(path)
        k <- db$list()
        v <- db$mget(k)
        t_old <- Sys.time() - self$expire * 24 * 60 * 60
        rebuild <- any(vlapply(v, function(el) el$timestamp < t_old))
      }
      rebuild
    },

    build = function(path, refresh = FALSE) {
      if (is.null(path)) {
        path <- self$local_drat
        if (is.null(path)) {
          stop("FIXME")
        }
      }
      if (length(self$spec) > 0L) {
        if (refresh || self$needs_build(path)) {
          ans <- drat_build(self$spec, path)
          self$local_drat <- path
        }
      }
      invisible(self)
    }
  ))

prepare_package_sources <- function(src, path_drat = NULL) {
  if (inherits(src, "package_sources")) {
    path_drat <- path_drat %||% tempfile()
    if (src$needs_build(path_drat)) {
      src$build(path_drat)
    }
  } else if (!is.null(src)) {
    stop("Invalid input for src")
  }
  src
}

##' @export
print.package_sources <- function(x, ...) {
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
  if (!is.null(x$local_drat)) {
    db <- drat_storr(x$local_drat)
    k <- db$list()
    v <- db$mget(k)
    dat <- data.frame(name = vcapply(v, "[[", "Package"),
                      version = vcapply(v, "[[", "Version"),
                      updated = vcapply(v, function(el)
                        prettyunits::time_ago(el$timestamp)),
                      stringsAsFactors = FALSE)
    str <- c(str,
             "  drat:",
             sprintf("    path: %s", x$local_drat),
             "    contents:",
             paste("    ", capture.output(print(dat, row.names = FALSE))))
    if (!is.null(x$expire) && length(v) > 0L) {
      expire <- lapply(v, function(el) el$timestamp + x$expire * 24 * 60 * 60)
      dt <- expire[[which.max(unlist(expire))]] - Sys.time()
      expire_str <-
        prettyunits::pretty_dt(as.difftime(x$expire, units = "days"))
      if (dt > 0) {
        str <- c(str,
                 sprintf("    expires: %s (valid for %s)",
                         prettyunits::pretty_dt(dt), expire_str))
      } else {
        str <- c(str,
                 sprintf("    expired: %s (valid for %s)",
                         prettyunits::vague_dt(- dt), expire_str))
      }
    }
  } else if (length(x$spec) > 0L) {
    str <- c(str, "  drat: <pending build>")
  }
  str
}
