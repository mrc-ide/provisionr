make_local_cran <- function() {
  path <- "local_cran"
  packages <- c("devtools", "progress", "ape")

  version <- check_r_version(NULL)
  version_str <- r_version_str(version)
  repo <- "https://cran.rstudio.com"
  db <- available_packages(repo, "windows", NULL)

  tmp <- available_packages(normalizePath(path), "windows", NULL)

  pkgs <- recursive_deps(packages, db$all)

  url_src <- contrib_url(repo, "src", version_str)
  url_bin <- contrib_url(repo, "windows", version_str)

  dest_src <- contrib_url(path, "src", version_str)
  dest_bin <- contrib_url(path, "windows", version_str)
  dir.create(dest_src, FALSE, TRUE)
  dir.create(dest_bin, FALSE, TRUE)

  download.packages(pkgs, dest_src, db$src, repo, url_src, type = "source")
  download.packages(pkgs, dest_bin, db$bin, repo, url_bin, type = "win.binary")

  tools::write_PACKAGES(dest_src, type = "source")
  tools::write_PACKAGES(dest_bin, type = "win.binary")
}
