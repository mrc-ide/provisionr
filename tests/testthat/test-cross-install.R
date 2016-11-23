context("cross_install")

test_that("binary cross install", {
  lib <- tempfile()
  res <- cross_install_packages("ape", lib, "windows")
  expect_equal(dir(lib), "ape")
  expect_true(file.exists(file.path(lib, "ape", "libs", "x64", "ape.dll")))
})

test_that("binary cross install with deps", {
  lib <- tempfile()
  res <- cross_install_packages("devtools", lib, "windows")
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

  db <- available_packages(getOption("repos", "https://cran.rstudio.com"),
                           "windows", check_r_version(NULL))
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
