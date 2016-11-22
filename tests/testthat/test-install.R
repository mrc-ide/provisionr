context("install")

test_that("throw on unavailable package", {
  expect_error(suppressWarnings(install_packages("nosuchpackage")),
               "is not available")
  expect_warning(install_packages("nosuchpackage", error = FALSE),
               "is not available")
  expect_error(
    suppressWarnings(install_packages("nosuchpackage", error = FALSE)),
    NA)
})

test_that("throw on broken package", {
  pkg <- build_package("broken")
  on.exit(file.remove(pkg))
  expect_error(suppressWarnings(
    install_packages(pkg, repos = NULL, error = TRUE, quiet = TRUE)),
    "had non-zero exit status")
})
