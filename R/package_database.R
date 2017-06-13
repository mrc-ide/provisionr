## The term "target" here is our installation target type; it's going to be:
##   src, windows, macos(|/mavericks|/el-capitan)
package_database <- function(repos, target, version, progress = NULL) {
  provisionr_log("download", "package database")

  is_local <- grepl("^(/|[A-Za-z]:|file://)", repos)
  if (any(is_local)) {
    i <- file.exists(repos)
    if (any(i)) {
      repos[i] <- file_url(repos[i])
    }
  }
  err <- !grepl("^[a-z]+://", repos)
  if (any(err)) {
    stop("Not all repos resolvable as urls: ",
         paste(repos[err], collapse = ", "))
  }

  ## This is necessary to avoid trying to build CRAN binaries of
  ## recently updated packages...
  if (is.null(names(repos))) {
    is_cran <- rep(FALSE, length(repos))
  } else {
    is_cran <- names(repos) == "CRAN"
  }

  url_src <- contrib_url(repos, "src", NULL)
  if (any(is_local)) {
    lapply(file_unurl(url_src[is_local]), drat_ensure_PACKAGES)
  }

  if (is.null(target)) {
    os_type <- NULL
    subarch <- NULL
  } else {
    target <- match_value(target, valid_targets(version))
    os_type <- os_type(target)
    subarch <- NULL
  }

  url_src <- contrib_url(repos, "src", NULL)
  pkgs_src <- available_packages(url_src, version, os_type, subarch,
                                 progress = progress)

  x <- pkgs_src[, "Depends"]
  f <- function(x) {
    lapply(strsplit(sub("^[[:space:]]*", "", x), "[[:space:]]*,[[:space:]]*"),
           function(s) s[grepl("^R[[:space:]]*\\(", s)])
  }

  deps <- parse_deps(pkgs_src[, "Depends"])

  if (is.null(target) || target == "linux") {
    pkgs_bin <- pkgs_src[integer(0), ]
  } else {
    version_str <- r_version_str(check_r_version(version), 2L)
    url_bin <- contrib_url(repos, target, version_str)
    if (any(is_local)) {
      lapply(file_unurl(url_bin[is_local]), drat_ensure_PACKAGES)
    }
    pkgs_bin <- available_packages(url_bin, version, os_type, subarch,
                                   progress = progress)

    if (check_r_version(version)[1, 1:2] < r_oldrel_version()[1, 1:2]) {
      ## Here are might have trouble with windows binaries so I am
      ## going to filter out old ones.  This might be too agressive
      ## but it should hopefully do the trick.
      provisionr_log("note",
                     "filtering outdated binary versions for old R version")
      check <- intersect(rownames(pkgs_bin), rownames(pkgs_src))
      outdated <- check[numeric_version(pkgs_bin[check, "Version"]) <
                        numeric_version(pkgs_src[check, "Version"])]
      pkgs_bin <- pkgs_bin[!(rownames(pkgs_bin) %in% outdated), , drop = FALSE]
    }
  }
  extra <- setdiff(rownames(pkgs_bin), rownames(pkgs_src))
  if (length(extra) > 0L) {
    pkgs_all <- rbind(pkgs_src, pkgs_bin[extra, ])
  } else {
    pkgs_all <- pkgs_src
  }
  list(all = pkgs_all, src = pkgs_src, bin = pkgs_bin,
       target = target, version = version,
       repos = repos, is_cran = is_cran)
}

valid_targets <- function(version = NULL) {
  version2 <- check_r_version(version)[1L, 1:2]
  c("windows",
    "linux",
    if (version2 <= numeric_version("3.2")) "macosx",
    if (version2 >= numeric_version("3.1") &&
        version2 <= numeric_version("3.3")) "macosx/mavericks",
    if (version2 >= numeric_version("3.4")) "macosx/el-capitan")
}

os_type <- function(target) {
  if (is.null(target)) {
    NULL
  } else if (target == "windows") {
    "windows"
  } else {
    "unix"
  }
}

contrib_url <- function(repos, target, version) {
  assert_scalar_character("target")
  ## target should be:
  ##   src
  ##   windows
  ##   macosx
  ##   macosx/mavericks
  ##   macosx/el-capitan
  if (target == "src" || target == "linux") {
    path <- "src/contrib"
  } else {
    version <- check_r_version(version)
    version_str <- r_version_str(version)
    target <- match_value(target, valid_targets(version))
    path <- file.path("bin", target, "contrib", version_str)
  }
  file.path(sub("/$", "", repos), path)
}
