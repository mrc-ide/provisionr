context("available_packages")

test_that("comparible to base: http", {
  cran <- "https://cran.rstudio.com"
  url <- contrib_url(cran, "src", NULL)
  cmp <- available.packages(url, filters = "duplicates")
  res <- available_packages(url, drop_duplicates = TRUE, progress = FALSE)
  expect_equal(cmp, res)
})

test_that("comparible to base: file", {
  cran <- file_url("local_cran")
  url <- contrib_url(cran, "src", NULL)
  cmp <- available.packages(url, filters = "duplicates")
  res <- available_packages(url, drop_duplicates = TRUE, progress = FALSE)
  expect_equal(cmp, res)
})
