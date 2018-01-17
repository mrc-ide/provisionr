context("provision_library")

test_that("own platform, single package, not compiled", {
  lib <- tempfile()
  res <- provision_library("R6", lib, quiet = TRUE, progress = FALSE)
  expect_equal(dir(lib), "R6")
  expect_equal(.packages(TRUE, lib), "R6")
  expect_equal(res$packages, "R6")
  expect_equal(res$path_lib, lib)
})

test_that("own platform, single package, compiled", {
  skip_on_travis()
  lib <- tempfile()
  res <- provision_library("zip", lib, quiet = TRUE, progress = FALSE)
  expect_equal(dir(lib), "zip")
  expect_equal(.packages(TRUE, lib), "zip")
  expect_equal(res$packages, "zip")
})

test_that("own platform, multiple packages", {
  lib <- tempfile()
  res <- provision_library("progress", lib, quiet = TRUE, progress = FALSE)
  expect_true("progress" %in% dir(lib))
  expect_gt(length(dir(lib)), 1)
})

test_that("no packages", {
  expect_null(provision_library(NULL, stop("don't reference this")))
  expect_null(provision_library(character(0), stop("don't reference this")))
  expect_error(provision_library("x", stop("don't reference this")),
               "don't reference this")
})

test_that("cross, single package, not compiled", {
  lib <- tempfile()
  res <- provision_library("R6", lib, platform = "windows", progress = FALSE)
  expect_equal(dir(lib), "R6")
  expect_equal(.packages(TRUE, lib), "R6")
  expect_equal(res$packages, "R6")
})

test_that("cross, single package, compiled", {
  lib <- tempfile()
  res <- provision_library("zip", lib, platform = "windows", progress = FALSE)
  expect_equal(dir(lib), "zip")
  expect_equal(.packages(TRUE, lib), "zip")
  expect_equal(res$packages, "zip")
})

test_that("cross, multiple packages", {
  lib <- tempfile()
  res <- provision_library("progress", lib, platform = "windows",
                           progress = FALSE)
  expect_true("progress" %in% dir(lib))
  expect_gt(length(dir(lib)), 1)
  expect_true("progress" %in% res$packages)
})

test_that("use package sources - transient drat", {
  lib <- tempfile()
  src <- package_sources(local = "hello")
  res <- provision_library("hello", lib, src = src, quiet = TRUE,
                           progress = FALSE)
  expect_equal(dir(lib), "hello")
})

test_that("use package sources - persistent drat", {
  lib <- tempfile()
  drat <- tempfile()
  src <- package_sources(github = "richfitz/kitten", local_drat = drat)
  res <- provision_library("kitten", lib, src = src, quiet = TRUE,
                           progress = FALSE)
  expect_true(file.exists(drat))
  expect_equal(drat_storr(drat)$list(), "github::richfitz/kitten")
})

test_that("own platform - re-provision", {
  lib <- tempfile()
  res <- provision_library("progress", lib, quiet = TRUE, progress = FALSE)
  expect_true("progress" %in% dir(lib))
  expect_silent(res <- provision_library("progress", lib,
                                         installed_action = "skip",
                                         progress = FALSE))
  expect_null(res)
})

test_that("auxillary library", {
  ## TODO: class the output of provision_library so that the db does
  ## not print!
  lib1 <- tempfile()
  res1 <- provision_library("curl", lib1, "windows", progress = FALSE)

  lib2 <- tempfile()
  res2 <- provision_library("httr", c(lib2, lib1), "windows", progress = FALSE)

  expect_true("curl" %in% dir(lib1))
  expect_false("curl" %in% dir(lib2))
})

test_that("no library", {
  expect_error(provision_library("R6", character(0)),
               "'lib' must have at least one element")
})

test_that("use package sources - refresh drat on version increase", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")
  v0 <- read_package_version(hello)

  lib <- tempfile()
  src <- package_sources(local = hello)
  res <- provision_library("hello", lib, src = src, quiet = TRUE,
                           progress = FALSE)

  ## some basic checking:
  st <- drat_storr(res$package_sources$local_drat)
  expect_equal(numeric_version(st$get(res$package_sources$spec)$Version), v0)
  expect_equal(read_package_version(file.path(lib, "hello")), v0)

  expect_false(res$package_sources$needs_build())
  v1 <- alter_package_version(hello, TRUE)
  expect_false(res$package_sources$needs_build())

  ## Nothing has been changed here:
  ans <- provision_library("hello", lib, src = src, quiet = TRUE,
                           progress = FALSE)
  expect_equal(numeric_version(st$get(res$package_sources$spec)$Version), v0)
  expect_equal(read_package_version(file.path(lib, "hello")), v0)

  ## Then try with refresh but no upgrade; still no change
  ans <- provision_library("hello", lib, src = src, quiet = TRUE,
                           refresh_drat = TRUE, installed_action = "skip",
                           progress = FALSE)
  expect_null(ans)
  expect_equal(numeric_version(st$get(res$package_sources$spec)$Version), v0)
  expect_equal(read_package_version(file.path(lib, "hello")), v0)

  ans <- provision_library("hello", lib, src = src, quiet = TRUE,
                           refresh_drat = TRUE, installed_action = "upgrade",
                           progress = FALSE)
  expect_equal(numeric_version(st$get(res$package_sources$spec)$Version), v1)
  expect_equal(read_package_version(file.path(lib, "hello")), v1)
})

test_that("use package sources - refresh on data change", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")
  v0 <- read_package_version(hello)

  lib <- tempfile()
  src <- package_sources(local = hello)
  res <- provision_library("hello", lib, src = src, quiet = TRUE,
                           progress = FALSE)

  ## some basic checking:
  st <- drat_storr(res$package_sources$local_drat)
  d1 <- st$get(res$package_sources$spec)
  b1 <- drop(read.dcf(file.path(lib, "hello", "DESCRIPTION"),
                      c("Packaged", "Built")))
  ## Check that things look OK
  expect_equal(d1$Packaged, b1[["Packaged"]])

  ## Then tweak the package:
  writeLines("", file.path(hello, "R", "new.R"))
  expect_false(res$package_sources$needs_build())

  ## Nothing has been changed here:
  ans <- provision_library("hello", lib, src = src, quiet = TRUE,
                           progress = FALSE)
  expect_equal(st$get(res$package_sources$spec), d1)

  ## Then try with refresh but no upgrade; still no change
  ans <- provision_library("hello", lib, src = src, quiet = TRUE,
                           refresh_drat = TRUE, installed_action = "skip",
                           progress = FALSE)
  expect_null(ans)
  expect_equal(st$get(res$package_sources$spec), d1)

  ## Then actually do it:
  ans <- provision_library("hello", lib, src = src, quiet = TRUE,
                           refresh_drat = TRUE, installed_action = "upgrade",
                           progress = FALSE)
  ## The drat package has been upgraded:
  d2 <- st$get(res$package_sources$spec)
  expect_false(d1$md5 == d2$md5)
  expect_gt(as.numeric(d2$timestamp), as.numeric(d1$timestamp))

  ## But has the library?
  b2 <- drop(read.dcf(file.path(lib, "hello", "DESCRIPTION"),
                      c("Packaged", "Built")))
  expect_true(!identical(b1, b2))
})

test_that("deal with base packages - only base present", {
  lib <- tempfile()
  res <- provision_library("stats", lib, quiet = TRUE, progress = FALSE)
  expect_equal(res$packages, character(0))
  expect_equal(dir(lib), character(0))
})

test_that("deal with base packages - mix of packages present", {
  lib <- tempfile()
  res <- provision_library(c("R6", "stats"), lib, quiet = TRUE,
                           progress = FALSE)
  expect_equal(res$packages, "R6")
  expect_equal(dir(lib), "R6")
})

test_that("upgrade recommended package", {
  lib <- tempfile()
  src <- package_sources(local = "deprec")
  res <- provision_library("deprec", lib, src = src, quiet = TRUE,
                           progress = FALSE)
  expect_equal(dir(lib), "deprec")

  ## Then we install codetools in there:
  res <- provision_library("codetools", lib, quiet = TRUE, progress = FALSE)
  expect_equal(sort(dir(lib)), sort(c("codetools", "deprec")))

  path <- file.path(lib, "codetools")
  v0 <- read_package_version(path)
  v1 <- alter_package_version(path, FALSE)

  ## 1. skip:
  res <- provision_library("deprec", lib, src = src, quiet = TRUE,
                           progress = FALSE)
  expect_equal(res$packages, character(0))
  expect_equal(read_package_version(path), v1)

  ## 2. upgrade
  res <- provision_library("deprec", lib, src = src, quiet = TRUE,
                           installed_action = "upgrade", progress = FALSE)
  expect_equal(res$packages, character(0))
  expect_equal(read_package_version(path), v1)

  ## 3. upgrade_all
  res <- provision_library("deprec", lib, src = src, quiet = TRUE,
                           installed_action = "upgrade_all", progress = FALSE)
  expect_equal(res$packages, "codetools")
  expect_equal(read_package_version(path), v0)
})
