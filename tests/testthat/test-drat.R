context("drat")

test_that("build drat", {
  path <- tempfile()
  dir.create(path)
  specs <- "github::richfitz/odin"
  ans <- drat_build(specs, path)

  expect_is(ans, "list")
  expect_true(specs %in% names(ans))
  expect_equal(unname(ans[[specs]][["Package"]]), "odin")

  expect_true(
    length(dir(file.path(path, "src", "contrib"), "odin_.*\\.tar\\.gz$")) > 0)
})

test_that("directory", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")

  src <- package_sources(local = hello)
  ans <- src$build(tempfile())

  ## TODO: this might be nicer if it dispatched appropriately for
  ## package_sources objects?
  db <- drat_storr(ans$local_drat)
  k <- db$list()
  expect_equal(k, paste0("local::", hello))
  expect_equal(db$get(k)$Package, "hello")
})

test_that("tarball", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")
  pkg <- build_package(hello)

  src <- package_sources(local = pkg)
  ans <- src$build(tempfile())

  db <- drat_storr(ans$local_drat)
  k <- db$list()
  expect_equal(k, paste0("local::", pkg))
  expect_equal(db$get(k)$Package, "hello")
})

test_that("update", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")

  src <- package_sources(local = hello)
  expect_true(src$needs_build())
  expect_true(src$needs_build(tempfile()))

  tmp <- tempfile()
  expect_message(src$build(tmp), "drat")
  expect_false(src$needs_build())
  expect_equal(src$local_drat, tmp)
  expect_true(src$needs_build(tempfile()))

  expect_silent(src$build(tmp))
  expect_silent(src$build(NULL))

  ## Now, we update the package:
  v <- alter_package_version(hello, increase = TRUE)
  expect_silent(src$build(tmp))
  expect_message(dat <- src$build(tmp, TRUE), "drat")
  db <- drat_storr(dat$local_drat)
  expect_equal(db$get(db$list())$Version, as.character(v))
})

test_that("binary package", {
  tmp <- tempfile()
  dir.create(tmp)

  url <- contrib_url(sanitise_options_cran()[[1]], "windows", NULL)
  res <- download.packages("ape", tmp, contriburl = url, type = "win.binary")
  pkg <- res[[2]]

  src <- package_sources(local = pkg)
  tmp <- tempfile()
  expect_message(dat <- src$build(tmp), "drat")
  expect_silent(src$build(tmp))

  db <- available_packages(tmp, "windows", NULL)
  expect_equal(nrow(db$src), 0)
  expect_equal(nrow(db$bin), 1)
  expect_equal(rownames(db$bin), "ape")
})
