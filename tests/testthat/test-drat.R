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
  expect_equal(unname(ans[[specs]][, "Package"]), "odin")

  expect_true(
    length(dir(file.path(path, "src", "contrib"), "odin_.*\\.tar\\.gz$")) > 0)
})
