context("remote")

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

test_that("url", {
  spec <- "url::http://foo.bar"
  x <- parse_remote(spec)
  expect_equal(x$url_package, "http://foo.bar")
  expect_equal(x$type, "url")
  expect_equal(x$spec, spec)
})

test_that("local", {
  spec <- "local::hello"
  x <- parse_remote(spec)
  expect_equal(x$url_package, "hello")
  expect_true(x$is_directory)
  expect_equal(x$type, "local")
  expect_equal(x$spec, spec)
})

test_that("local - missing files", {
  expect_error(parse_remote("local::missing"),
               "path does not exist")
  expect_error(parse_remote(paste0("local::", tempfile())),
               "path does not exist")
})

test_that("errors", {
  ## This will get implemented soon, but realistically few people are
  ## using bitbucket right now.
  expect_error(parse_remote("bitbucket::foo/bar"),
               "Invalid remote source 'bitbucket'")
})

test_that("split spec", {
  expect_equal(split_spec("foo::bar"), cbind(type = "foo", value = "bar"))
  expect_equal(split_spec("url::http://foo"),
               cbind(type = "url", value = "http://foo"))

  expect_equal(split_spec(character()),
               cbind(type = character(0), value = character(0)))

  expect_equal(split_spec(c("foo::bar", "baz::frob")),
               cbind(type = c("foo", "baz"), value = c("bar", "frob")))
})

test_that("build_package - error cases", {
  path <- tempfile()
  writeLines(character(0), path)
  expect_error(build_package("hello", dest = path), "must be a directory")
  expect_error(build_package(path), "Did not find a valid package at")
})
