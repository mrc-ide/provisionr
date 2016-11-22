context("package_sources")

test_that("defaults", {
  src <- package_sources()
  expect_equal(src$cran, "https://cran.rstudio.com")
  expect_null(src$repos)
  expect_null(src$github)
  expect_null(src$local)
})
