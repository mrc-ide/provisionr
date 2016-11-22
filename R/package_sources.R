package_sources <- function(cran = NULL, repos = NULL,
                            github = NULL, local = NULL) {
  if (is.null(cran)) {
    cran <- "https://cran.rstudio.com"
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
  self$build <- function(path) local_drat(self, path)$build()
  class(self) <- "package_sources"
  self
}

local_drat <- function(src, path) {
  ## This should keep track of when it was last built, what is in it,
  ## etc.
  self <- list(cran = src$cran,
               repos = c(src$repos, path),
               path = path,
               db = drat_storr(path))
  self$build <- function() {
    drat_build(src$spec, path)
    self
  }
  class(self) <- "local_drat"
  self
}
