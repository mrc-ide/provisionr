context("package_sources")

test_that("defaults", {
  src <- package_sources()
  expect_equal(src$cran, "https://cran.rstudio.com")
  expect_null(src$repos)
  expect_null(src$spec)
})

test_that("github", {
  src <- package_sources(github = "richfitz/kitten")
  expect_is(src, "package_sources")
  expect_equal(src$spec, "github::richfitz/kitten")
})

test_that("build", {
  src <- package_sources(github = "richfitz/kitten")
  tmp <- tempfile()
  ret <- src$build(tmp)
  expect_is(ret, "local_drat")
  expect_true(file.exists(tmp))
  path <- file.path(tmp, "src", "contrib")
  expect_true(file.exists(file.path(path, "PACKAGES")))
  pkgs <- read.dcf(file.path(path, "PACKAGES"))
  expect_equal(unname(pkgs[, "Package"]), "kitten")

  spec <- "github::richfitz/kitten"
  expect_equal(ret$db$list(), spec)
  dat <- ret$db$get(spec)
  tgz <- file.path(path, dat$tgz)
  expect_true(file.exists(tgz))
  expect_equal(unname(tools::md5sum(tgz)), dat$md5)
  expect_equal(dat$Package, "kitten")

  expect_equal(ret$src, src)
  expect_equal(ret$path, tmp)
})
