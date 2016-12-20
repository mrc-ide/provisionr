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

  expect_error(package_sources(github = "local::hello"),
               "Non-github spec")
})

test_that("local", {
  src <- package_sources(local = "hello")
  expect_equal(src$spec, "local::hello")

  expect_error(package_sources(local = tempfile()),
               "Missing local files")
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

test_that("supplied cran", {
  src <- package_sources(cran = "http://mycran.com")
  expect_equal(src$cran, "http://mycran.com")
  expect_error(
    package_sources(cran = c("http://cran1.com", "http://cran2.com")),
    "cran must be a scalar")
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
  y <- x$build(tempfile())
  expect_match(as.character(y), "<local_drat>", fixed = TRUE, all = FALSE)
  expect_output(print(y), "<local_drat>", fixed = TRUE)
})
