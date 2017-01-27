context("provision_library")

test_that("own platform, single package, not compiled", {
  lib <- tempfile()
  res <- provision_library("R6", lib, quiet = TRUE)
  expect_equal(dir(lib), "R6")
  expect_equal(.packages(TRUE, lib), "R6")
  expect_equal(res$packages, "R6")
})

test_that("own platform, single package, compiled", {
  lib <- tempfile()
  res <- provision_library("ape", lib, quiet = TRUE)
  expect_equal(dir(lib), "ape")
  expect_equal(.packages(TRUE, lib), "ape")
  expect_equal(res$packages, "ape")
})

test_that("own platform, multiple packages", {
  lib <- tempfile()
  res <- provision_library("progress", lib, quiet = TRUE)
  expect_true("progress" %in% dir(lib))
  expect_gt(length(dir(lib)), 1)
})

test_that("cross, single package, not compiled", {
  lib <- tempfile()
  res <- provision_library("R6", lib, platform = "windows")
  expect_equal(dir(lib), "R6")
  expect_equal(.packages(TRUE, lib), "R6")
  expect_equal(res$packages, "R6")
})

test_that("cross, single package, compiled", {
  lib <- tempfile()
  res <- provision_library("ape", lib, platform = "windows")
  expect_equal(dir(lib), "ape")
  expect_equal(.packages(TRUE, lib), "ape")
  expect_equal(res$packages, "ape")
})

test_that("cross, multiple packages", {
  lib <- tempfile()
  res <- provision_library("progress", lib, platform = "windows")
  expect_true("progress" %in% dir(lib))
  expect_gt(length(dir(lib)), 1)
  expect_true("progress" %in% res$packages)
})

test_that("use package sources - transient drat", {
  lib <- tempfile()
  src <- package_sources(local = "hello")
  res <- provision_library("hello", lib, src = src)
  expect_equal(dir(lib), "hello")
})

test_that("use package sources - persistent drat", {
  lib <- tempfile()
  drat <- tempfile()
  src <- package_sources(github = "richfitz/kitten")
  res <- provision_library("kitten", lib, src = src, path_drat = drat)
  expect_true(file.exists(drat))
  expect_equal(drat_storr(drat)$list(), "github::richfitz/kitten")
})

test_that("own platform - re-provision", {
  lib <- tempfile()
  res <- provision_library("progress", lib, quiet = TRUE)
  expect_true("progress" %in% dir(lib))
  expect_silent(res <- provision_library("progress", lib,
                                         installed_action = "skip"))
  expect_null(res)
})
