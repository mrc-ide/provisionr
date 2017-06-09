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
##' @param local_drat Optional location to cache downloaded packages,
##'   when \code{github} or \code{local} packages are used.  If not
##'   given, this can be set at any time by setting the
##'   \code{local_drat} element of the returned element.  If not given
##'   by the time that the packages need to be downloaded then a
##'   session-specific temporary directory will be used.
##'
##' @param data An object of class \code{package_sources_list},
##'   created by running the \code{as_list()} method of a
##'   \code{package_sources} object.  This is useful for serialisation
##'   without saving references to \code{provisionr}.
##'
##' @param spec Raw entries of the form \code{<type>::<user>/<repo>...}
##'
##' @export
package_sources <- function(cran = NULL, repos = NULL,
                            github = NULL, local = NULL,
                            expire = NULL, local_drat = NULL,
                            data = NULL, spec = NULL) {
  R6_package_sources$new(cran, repos, github, local, expire, local_drat,
                         data, spec)
}

##' @importFrom R6 R6Class
R6_package_sources <- R6::R6Class(
  "package_sources",
  public = list(
    cran = NULL,
    repos = NULL,
    spec = NULL,
    local_drat = NULL,
    expire = NULL,

    initialize = function(cran, repos, github, local, expire, local_drat,
                          data = NULL, spec = NULL) {
      if (!is.null(data)) {
        assert_is(data, "package_sources_list")
        ## Could require that none of these other arguments are non-NULL
        self$cran <- data$cran
        self$repos <- data$repos
        self$spec <- data$spec
        self$expire <- data$expire
        self$local_drat <- data$local_drat
        return()
      }

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
      if (!is.null(spec)) {
        spec <- lapply(spec, parse_remote)
      }

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

      if (!is.null(local_drat)) {
        assert_scalar_character(local_drat)
        self$local_drat <- local_drat
      }
    },

    as_list = function() {
      ret <- list(cran = self$cran,
                  repos = self$repos,
                  spec = self$spec,
                  local_drat = self$local_drat,
                  expire = self$expire)
      class(ret) <- "package_sources_list"
      ret
    },

    needs_build = function() {
      path <- self$local_drat
      rebuild <- length(self$spec) > 0 &&
        (is.null(path) || !file.exists(path) ||
         !all(drat_storr(path)$exists(self$spec)))
      if (!rebuild && !is.null(self$expire)) {
        db <- drat_storr(path)
        v <- db$mget(db$list())
        t_old <- Sys.time() - self$expire * 24 * 60 * 60
        rebuild <- any(vlapply(v, function(el) el$timestamp < t_old))
      }
      rebuild
    },

    build = function(refresh = FALSE, force = FALSE, progress = NULL) {
      if (length(self$spec) > 0L) {
        if (is.null(self$local_drat)) {
          ## TODO: this may not always be desirable, because if this
          ## ends up serialised, it will point to a directory that has
          ## disappeared after the session disappears.  I think that
          ## use of clone() should get around this enough.
          self$local_drat <- tempfile()
        }
        path <- self$local_drat
        if (refresh || self$needs_build()) {
          drat_build(self$spec, path, force, progress)
        }
      }
      invisible(self)
    }
  ))

prepare_package_sources <- function(src, refresh = FALSE, progress = NULL) {
  if (inherits(src, "package_sources")) {
    src$build(refresh, progress = progress)
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
