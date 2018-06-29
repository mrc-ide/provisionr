context("provision_dependencies")

test_that("read basic dependencies", {
  path <- tempfile()
  dir.create(path)

  writeLines("Imports: a, b", file.path(path, "DESCRIPTION"))
  expect_equal(provision_dependencies_read(path),
               list(packages = c("a", "b"), src = NULL))

  writeLines(c("Imports: a, b",
               "Depends: c"),
               file.path(path, "DESCRIPTION"))
  expect_equal(provision_dependencies_read(path),
               list(packages = c("c", "a", "b"), src = NULL))

  writeLines(c("Imports: a, b",
               "Depends: c",
               "LinkingTo: d"),
               file.path(path, "DESCRIPTION"))
  expect_equal(provision_dependencies_read(path),
               list(packages = c("c", "a", "b", "d"), src = NULL))
})


test_that("add github packages", {
  path <- tempfile()
  dir.create(path)

  writeLines("Imports: a, b", file.path(path, "DESCRIPTION"))
  src <- package_sources(github = "foo/bar")
  expect_equal(provision_dependencies_read(path, src = src),
               list(packages = c("a", "b"), src = src))

  writeLines('r_github_packages: ["foo/bar", "another/packages"]',
             file.path(path, ".travis.yml"))
  ## don't read travis by default
  expect_equal(provision_dependencies_read(path, src = src),
               list(packages = c("a", "b"), src = src))

  tmp <- provision_dependencies_read(path, src = src, read_travis = TRUE)
  expect_equal(tmp$src$spec,
               c("github::foo/bar", "github::another/packages"))

  tmp <- provision_dependencies_read(path, src = src, read_travis = TRUE)
  expect_equal(tmp$src$spec,
               c("github::foo/bar", "github::another/packages"))
})


test_that("error when no description present", {
  expect_error(provision_dependencies_read(tempfile()),
               "Did not find a DESCRIPTION file at")
})


test_that("bootstrap", {
  path <- tempfile()
  dir.create(path)
  writeLines(c("Imports: R6"), file.path(path, "DESCRIPTION"))

  withr::with_dir(path,
                  provision_dependencies_bootstrap(strict_lib = FALSE))
  expect_true(file.exists(file.path(path, "bootstrap")))
})
