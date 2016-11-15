context("cross_install")

test_that("binary cross install", {
  lib <- tempfile()
  res <- cross_install_packages("ape", lib, "windows")
  expect_equal(dir(lib), "ape")
  expect_true(file.exists(file.path(lib, "ape", "libs", "x64", "ape.dll")))
})
