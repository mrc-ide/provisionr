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
