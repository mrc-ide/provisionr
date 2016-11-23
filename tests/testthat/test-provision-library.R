context("provision_library")

test_that("own platform, single package, not compiled", {
  lib <- tempfile()
  res <- provision_library("R6", lib)
  expect_equal(dir(lib), "R6")
  expect_equal(.packages(TRUE, lib), "R6")
})

test_that("own platform, single package, compiled", {
  lib <- tempfile()
  res <- provision_library("ape", lib)
  expect_equal(dir(lib), "ape")
  expect_equal(.packages(TRUE, lib), "ape")
})

test_that("own platform, multiple packages", {
  lib <- tempfile()
  ## OK, this shows an issue; we want the library to be *standalone*
  res <- provision_library("progress", lib)
  expect_true("progress" %in% dir(lib))
  expect_gt(length(dir(lib)), 1)
})
