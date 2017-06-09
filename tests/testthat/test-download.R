context("download")

test_that("error", {
  path <- tempfile()
  dir.create(path, FALSE, TRUE)
  on.exit(unlink(path, recursive = TRUE))
  expect_error(download_file1("https://httpbin.org/status/404", path,
                              progress = FALSE),
               "Downloading.+failed with code 404")
  expect_equal(dir(path), character(0))
})
