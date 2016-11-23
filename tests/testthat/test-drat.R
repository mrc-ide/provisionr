context("drat")

## This will only work as long as odin, dde, ring stay off of CRAN.
## I'll want to swap this out for some disposable packages, perhaps
test_that("parse", {
  expect_equal(parse_remote("github::richfitz/odin"),
               parse_remote("richfitz/odin"))
})

test_that("parse simple case", {
  spec <- "username/repo"
  x <- parse_remote(spec)
  expect_equal(x$username, "username")
  expect_equal(x$repo, "repo")
  expect_null(x$subdir)
  expect_null(x$ref)
  expect_equal(x$type, "github")
  expect_equal(x$spec, paste0("github::", spec))
})

test_that("parse complicated case", {
  spec <- "username/repo/subdir@ref"
  x <- parse_remote(spec)
  expect_equal(x$username, "username")
  expect_equal(x$repo, "repo")
  expect_equal(x$subdir, "subdir")
  expect_equal(x$ref, "ref")
  expect_equal(x$type, "github")
  expect_equal(x$spec, paste0("github::", spec))
})

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

  k <- ans$db$list()
  expect_equal(k, paste0("local::", hello))
  expect_equal(ans$db$get(k)$Package, "hello")
})

test_that("tarball", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")
  pkg <- build_package(hello)

  src <- package_sources(local = pkg)
  ans <- src$build(tempfile())

  k <- ans$db$list()
  expect_equal(k, paste0("local::", pkg))
  expect_equal(ans$db$get(k)$Package, "hello")
})

test_that("update", {
  path <- tempfile()
  dir.create(path)
  file.copy("hello", path, recursive = TRUE)
  hello <- file.path(path, "hello")

  src <- package_sources(local = hello)
  tmp <- tempfile()
  expect_message(dat <- src$build(tmp), "drat")
  expect_silent(src$build(tmp))

  ## Now, we update the package:
  v <- alter_package_version(hello, increase = TRUE)
  expect_silent(src$build(tmp))
  expect_message(dat <- src$build(tmp, TRUE), "drat")
  expect_equal(dat$db$get(dat$db$list())$Version, as.character(v))
})
