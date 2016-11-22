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
  spec <- character(0)

  if (!is.null(github)) {
    tmp <- lapply(github, parse_remote)
    err <- vlapply(tmp, "[[", type) != "github"
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

  build <- function(path, verbose = TRUE) {
    local_drat(spec, path)$build(verbose)
  }
  ret <- list(cran = cran, repos = repos, spec = spec, build = build)
  class(ret) <- "package_sources"
  ret
}

local_drat <- function(spec, path) {
  ## This should keep track of when it was last built, what is in it,
  ## etc.
  force(spec)
  force(path)
  self <- list(spec = spec,
               path = path)
  self$build <- function(verbose = TRUE) {
    drat_build(spec, path, verbose)
    self
  }
  class(ret) <- "local_drat"
  ret
}
