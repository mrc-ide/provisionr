context("package_sources")

test_that("defaults", {
  src <- package_sources()
  expect_equal(src$cran, sanitise_options_cran())
  expect_null(src$repos)
  expect_null(src$spec)
})

test_that("github", {
  src <- package_sources(github = "richfitz/kitten")
  expect_is(src, "package_sources")
  expect_equal(src$spec, "github::richfitz/kitten")

  expect_error(package_sources(github = "local::hello"),
               "Non-github spec")
})

test_that("local", {
  src <- package_sources(local = "hello")
  expect_equal(src$spec, "local::hello")

  expect_error(package_sources(local = tempfile()),
               "Missing local files")
})

test_that("build (local)", {
  src <- package_sources(local = "hello")
  expect_null(src$local_drat)

  tmp <- tempfile()
  expect_true(src$needs_build())
  src$build(tmp)
  expect_equal(src$local_drat, tmp)
  expect_true(file.exists(src$local_drat))

  path <- file.path(src$local_drat, "src", "contrib")
  pkgs <- read.dcf(file.path(path, "PACKAGES"))
  expect_equal(unname(pkgs[, "Package"]), "hello")

  dat <- drat_storr(src$local_drat)$get(src$spec)
  tgz <- file.path(path, dat$tgz)
  expect_true(file.exists(tgz))
  expect_equal(unname(tools::md5sum(tgz)), dat$md5)
  expect_equal(dat$Package, "hello")
})

test_that("build (github)", {
  src <- package_sources(github = "richfitz/kitten")
  expect_null(src$local_drat)

  tmp <- tempfile()
  ret <- src$build(tmp)
  expect_equal(src$local_drat, tmp)
  expect_true(file.exists(src$local_drat))

  path <- file.path(src$local_drat, "src", "contrib")
  pkgs <- read.dcf(file.path(path, "PACKAGES"))
  expect_equal(unname(pkgs[, "Package"]), "kitten")

  dat <- drat_storr(src$local_drat)$get(src$spec)
  tgz <- file.path(path, dat$tgz)
  expect_true(file.exists(tgz))
  expect_equal(unname(tools::md5sum(tgz)), dat$md5)
  expect_equal(dat$Package, "kitten")
})

test_that("supplied cran", {
  src <- package_sources(cran = "http://mycran.com")
  expect_equal(src$cran, "http://mycran.com")
  expect_error(
    package_sources(cran = character()),
    "At least one cran repository must be given")
})

test_that("repos", {
  url1 <- "http://myrepo.com"
  url2 <- "http://another.com"
  expect_equal(package_sources(repos = url1)$repos, setNames(url1, url1))
  expect_equal(package_sources(repos = c(url1, url2))$repos,
               setNames(c(url1, url2), c(url1, url2)))

  expect_error(package_sources(repos = "repo.com"),
               "Missing url scheme")
})

test_that("repos - drat repos expand", {
  src <- package_sources(repos = "drat://richfitz")
  expect_equal(src$repos,
               setNames("https://richfitz.github.io/drat/", "drat://richfitz"))
  url1 <- "http://myrepo.com"
  url2 <- "http://another.com"
  drat <- "drat://richfitz"
  src <- package_sources(repos = c(url1, drat, url2))
  expect_equal(src$repos,
               setNames(c(url1, "https://richfitz.github.io/drat/", url2),
                        c(url1, drat, url2)))
})

test_that("print", {
  x <- package_sources(local = "hello")
  expect_match(as.character(x), "<package_sources>", fixed = TRUE, all = FALSE)
  expect_output(print(x), "<package_sources>", fixed = TRUE)
  expect_output(print(x), "drat: <pending build>")
  y <- x$build(tempfile())
  expect_match(as.character(y), "<package_sources>", fixed = TRUE, all = FALSE)
  expect_output(print(x), paste("path:", tempdir()), fixed = TRUE)
})

test_that("prepare_repos", {
  expect_equal(prepare_repos(NULL), sanitise_options_cran())
  src <- package_sources()
  expect_equal(prepare_repos(src), src$cran)

  src <- package_sources(repos = "https://foo.com")
  expect_equal(prepare_repos(src), c(src$repos, src$cran))

  src <- package_sources(repos = "https://foo.com", local = "hello")
  dat <- src$build(tempfile())
  expect_equal(unname(prepare_repos(dat)),
               unname(c(file_url(dat$local_drat), dat$repos,
                        sanitise_options_cran())))
})

test_that("prepare_package_sources", {
})

## fails because the github parse fails:
## x <- package_sources(github = "hello")$build(tempfile())

## make sure that we parse things properly on entry and then this is
## not a problem.  Same with the local file presence issue.
