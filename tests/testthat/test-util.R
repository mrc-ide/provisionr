context("util")

test_that("check_r_version", {
  expect_equal(check_r_version(NULL), getRversion())
  v <- numeric_version("3.1.2")
  expect_equal(check_r_version("3.1.2"), v)
  expect_equal(check_r_version(v), v)

  ## Error cases:
  expect_error(check_r_version(1), "Invalid type")
  expect_error(check_r_version("one"))
  expect_error(check_r_version(c(v, v)),
               "Expected a single version")
  expect_error(check_r_version(c("3.1.1", "3.1.2")),
               "Expected a single version")
})

test_that("r_version_str", {
  expect_equal(r_version_str("3.1.1"), "3.1")
  expect_equal(r_version_str("3.1.1", 3), "3.1.1")
  expect_equal(r_version_str("3.1.1", 1), "3")
  expect_equal(r_version_str(numeric_version("3.1.1")), "3.1")

  expect_error(r_version_str("3.1", 3), "Unexpected version length")
})

test_that("file_unurl", {
  testthat::with_mock(
    is_windows = function() FALSE,
    expect_equal(file_unurl("file:///usr/local"), "/usr/local"))
  testthat::with_mock(
    is_windows = function() TRUE,
    expect_equal(file_unurl("file:///c:/foo"), "c:/foo"))
})

test_that("download file error", {
  expect_error(download_file("xxx://asdfa"))
  expect_error(suppressWarnings(download_file("http://asdfa")))
})
