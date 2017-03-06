context("drat")

test_that("build drat", {
  path <- tempfile()
  dir.create(path)
  specs <- "github::richfitz/odin"
  ans <- drat_build(specs, path, FALSE)

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
  expect_null(src$local_drat)
  ans <- src$build()
  expect_is(src$local_drat, "character")
  expect_true(file.exists(src$local_drat))

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
  ans <- src$build()

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

  expect_message(src$build(), "drat")
  expect_false(src$needs_build())

  expect_silent(src$build())

  ## Now, we update the package:
  v <- alter_package_version(hello, increase = TRUE)
  expect_silent(src$build())
  expect_message(dat <- src$build(TRUE), "drat")
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
  expect_message(src$build(), "drat")
  expect_silent(src$build())

  db <- available_packages(src$local_drat, "windows", NULL)
  expect_equal(nrow(db$src), 0)
  expect_equal(nrow(db$bin), 1)
  expect_equal(rownames(db$bin), "ape")
})

test_that("local_drat in constructor", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")
  tmp <- tempfile()

  src <- package_sources(local = hello, local_drat = tmp)
  expect_equal(src$local_drat, tmp)
  expect_false(file.exists(tmp))
  expect_true(src$needs_build())
  expect_false(file.exists(tmp))

  src$build()
  expect_equal(src$local_drat, tmp)
  expect_false(src$needs_build())
  expect_true(file.exists(tmp))
})

test_that("update", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")
  tmp <- tempfile()

  src <- package_sources(local = hello, local_drat = tmp)
  expect_equal(src$local_drat, tmp)
  expect_false(file.exists(tmp))
  expect_true(src$needs_build())
  expect_false(file.exists(tmp))

  src$build()
  db <- drat_storr(tmp)
  d1 <- db$get(src$spec)
  md5 <- d1$md5
  tools::md5sum(file.path(contrib.url(tmp, "source"), d1$tgz))

  src$build(TRUE)
  expect_equal(db$get(src$spec), d1)

  src$build(TRUE, TRUE)
  d2 <- db$get(src$spec)
  expect_false(identical(d1, d2))
  expect_false(identical(d1$md5, d2$md5))
})
