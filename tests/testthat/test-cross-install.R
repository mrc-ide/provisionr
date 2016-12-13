context("cross_install")

test_that("binary cross install", {
  lib <- tempfile()
  db <- available_packages("https://cran.rstudio.com", "windows", NULL)
  res <- cross_install_packages("ape", lib, db)
  expect_equal(dir(lib), "ape")
  expect_true(file.exists(file.path(lib, "ape", "libs", "x64", "ape.dll")))
})

test_that("binary cross install with deps", {
  lib <- tempfile()
  db <- available_packages("https://cran.rstudio.com", "windows", NULL)
  res <- cross_install_packages("devtools", lib, db)
  expect_true("devtools" %in% dir(lib))
  expect_true("httr" %in% dir(lib))
  expect_true(file.exists(
    file.path(lib, "devtools", "libs", "x64", "devtools.dll")))

  dat <- check_library("devtools", lib)
  expect_equal(dat$missing, character(0))
  expect_true("httr" %in% dat$found)

  unlink(file.path(lib, "httr"), recursive = TRUE)
  unlink(file.path(lib, "curl"), recursive = TRUE)

  dat <- check_library("devtools", lib)
  expect_true("httr" %in% dat$missing)

  p1 <- cross_install_plan("httr", db, lib, "skip")
  expect_equal(p1$packages, c("curl", "httr"))
  p2 <- cross_install_plan("httr", db, lib, "upgrade")
  expect_equal(p2, p1)
  p3 <- cross_install_plan("httr", db, lib, "upgrade_all")
  expect_equal(p3, p1)
  p4 <- cross_install_plan("httr", db, lib, "replace")
  expect_gt(length(p4$packages), length(p1$packages))
  expect_true(all(p1$packages %in% p4$packages))

  ## Bit of fiddling with version numbers:
  alter_package_version(file.path(lib, "openssl"), increase = FALSE)

  q1 <- cross_install_plan("httr", db, lib, "skip")
  expect_equal(q1, p1)
  q2 <- cross_install_plan("httr", db, lib, "upgrade")
  expect_equal(q2, p2)
  q3 <- cross_install_plan("httr", db, lib, "upgrade_all")
  expect_equal(sort(q3$packages), sort(c(p3$packages, "openssl")))
  q4 <- cross_install_plan("httr", db, lib, "replace")
  expect_equal(q4, p4)

  ## And again, with the next package up:
  r1 <- cross_install_plan("devtools", db, lib, "skip")
  expect_equal(r1, p1)
  r2 <- cross_install_plan("devtools", db, lib, "upgrade")
  expect_equal(r2, p2)
  r3 <- cross_install_plan("devtools", db, lib, "upgrade_all")
  expect_equal(r3, q3)
  r4 <- cross_install_plan("devtools", db, lib, "replace")
  expect_gt(length(r4$packages), length(p4$packages))
  expect_true(all(p4$packages %in% r4$packages))
})

test_that("cross install source package", {
  lib <- tempfile()

  ## This is never going on CRAN and has no nasty dependencies:
  src <- package_sources(github = "richfitz/kitten")
  path_drat <- tempfile()
  drat <- src$build(path_drat)
  provision_library("kitten", lib, platform = "windows", src = drat)
  expect_equal(dir(lib), "kitten")
})

## This deals with an issue where an `importFrom` directive in a
## NAMESPACE file causes lazyloading of source files that depend on
## binary files to fail
test_that("cross install package that triggers load", {
  src <- package_sources(local = "lazyproblem")
  drat <- src$build(tempfile())
  path <- tempfile()
  provision_library("lazyproblem", path, platform = "windows", src = drat)
  pkgs <- .packages(TRUE, path)
  expect_equal(sort(pkgs), sort(c("deSolve", "lazyproblem")))
})

test_that("installed_action", {
  lib <- tempfile()

  msgs <- capture_messages(
    provision_library("ape", lib, platform = "windows"))
  expect_true(any(grepl("cross", msgs)))
  expect_equal(dir(lib), "ape")

  ## Skip reinstallation:
  msgs <- capture_messages(
    provision_library("ape", lib, platform = "windows",
                      installed_action = "skip"))
  expect_false(any(grepl("cross", msgs)))

  ## Upgrade (but don't)
  msgs <- capture_messages(
    provision_library("ape", lib, platform = "windows",
                      installed_action = "upgrade"))
  expect_false(any(grepl("cross", msgs)))

  ## Upgrade (but do) -- this does not work!
  alter_package_version(file.path(lib, "ape"), FALSE)
  msgs <- capture_messages(
    provision_library("ape", lib, platform = "windows",
                      installed_action = "upgrade"))
  expect_true(any(grepl("cross", msgs)))

  ## Replace:
  msgs <- capture_messages(
    provision_library("ape", lib, platform = "windows",
                      installed_action = "replace"))
  expect_true(any(grepl("cross", msgs)))
})

test_that("missing compiled packages", {
  ## This is unlikely to resolve itself any time soon; not on CRAN and
  ## dependent on two other packages that are not on CRAN that require
  ## compilation (dde, ring) and one that does not require compilation
  ## (rcmdshlib).
  ##
  ## TODO: cache the calls here, possibly across sessions?
  src <- package_sources(github = "richfitz/dde")
  drat <- src$build(tempfile())

  ## NOTE: This triggers the lazy loading issue that I had in context
  ## (and had solved there at some point) where lazy loading of one
  ## package triggers a failure in the package installation.
  path <- tempfile()
  expect_error(provision_library("dde", path, platform = "windows", src = drat),
               "Packages need compilation")
  ans <- provision_library("dde", path, platform = "windows", src = drat,
                           allow_missing = TRUE)
  expect_equal(sort(rownames(ans$missing)),
               sort(c("dde", "ring")))
})

test_that("prefer drat files", {
  ## TODO: this can actually point at the source file already in the
  ## same repo.
  src <- package_sources(github = "cran/ape")
  drat <- src$build(tempfile())
  path <- tempfile()
  ans <- provision_library("ape", path, platform = "windows", src = drat,
                           allow_missing = TRUE)
  expect_equal(rownames(ans$missing), "ape")
  expect_equal(dir(path), character(0))
})
