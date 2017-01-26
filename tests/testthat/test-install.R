context("install")

test_that("throw on unavailable package", {
  lib <- NULL
  repos <- sanitise_options_cran()
  path <- tempfile()
  expect_error(suppressWarnings(
    install_packages("nosuchpackage", lib, repos, quiet = TRUE)),
    "is not available")
  expect_warning(
    install_packages("nosuchpackage", lib, repos, error = FALSE, quiet = TRUE),
    "is not available")
  expect_error(
    suppressWarnings(
      install_packages("nosuchpackage", lib, repos, error = FALSE,
                       quiet = TRUE)),
    NA)
})

test_that("throw on broken package", {
  pkg <- build_package("broken")
  on.exit(file.remove(pkg))
  expect_error(suppressWarnings(
    install_packages(pkg, NULL, NULL, error = TRUE, quiet = TRUE)),
    "had non-zero exit status")
})

test_that("install zero packages", {
  ## Don't even reference any of the other arguments:
  expect_null(install_packages(NULL))
})

test_that("default_lib", {
  expect_equal(default_lib(NULL), .libPaths()[[1L]])
  p1 <- tempfile()
  p2 <- tempfile()
  expect_equal(default_lib(p1), p1)
  expect_equal(default_lib(c(p1, p2)), p1)
})

test_that("sanitise_options_cran", {
  expect_equal(with_repos("@CRAN@", sanitise_options_cran()),
               "https://cran.rstudio.com")
})
