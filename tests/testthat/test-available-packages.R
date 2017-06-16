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

test_that("missing index: file", {
  cran <- file_url("local_cran")
  url <- contrib_url(cran, "windows", "3.2.1")

  d <- available_packages(url, progress = FALSE, r_version = "3.2.1",
                          missing_index_is_error = FALSE)
  expect_is(d, "matrix")
  expect_equal(nrow(d), 0)
  expect_true(all(c("Package", "Repository") %in% colnames(d)))

  expect_error(available_packages(url, progress = FALSE, r_version = "3.2.1"),
               "No package index")
})

test_that("missing index: http", {
  cran <- "https://dide-tools.github.com/drat"
  url <- contrib_url(cran, "windows", "3.2.1")

  d <- available_packages(url, progress = FALSE, r_version = "3.2.1",
                          missing_index_is_error = FALSE)
  expect_is(d, "matrix")
  expect_equal(nrow(d), 0)
  expect_true(all(c("Package", "Repository") %in% colnames(d)))

  expect_error(available_packages(url, progress = FALSE, r_version = "3.2.1"))
})
