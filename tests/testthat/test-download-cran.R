context("download-cran")

test_that("download source", {
  src <- package_sources(cran = "https://cran.rstudio.com")
  path <- tempfile()
  download_cran("devtools", path, "3.4.0", "ALL", FALSE, src, progress = FALSE)

  db_win <- package_database(file_url(path), "windows", "3.4.0")
  db_mac <- package_database(file_url(path), "macosx/el-capitan", "3.4.0")
  db_src <- package_database(file_url(path), NULL, "3.4.0")

  expect_true("curl" %in% rownames(db_win$all))
  expect_true("curl" %in% rownames(db_mac$all))
  expect_true("curl" %in% rownames(db_src$all))

  path2 <- tempfile()
  src2 <- package_sources(cran = file_url(path))
  download_cran("devtools", path2, "3.4.0", "windows", FALSE, src2,
                progress = FALSE)
  db2_win <- package_database(file_url(path2), "windows", "3.4.0")
  db2_src <- package_database(file_url(path2), NULL, "3.4.0")
  expect_equal(dimnames(db2_win$all), dimnames(db_win$all))
  expect_equal(dimnames(db2_src$all), dimnames(db_src$all))
})

test_that("missing index", {
  src <- package_sources(repos = "https://dide-tools.github.io/drat")
  path <- tempfile()
  available_packages(contrib_url(src$repos, "src", NULL))

  ## this does not download things properly and I don't see why
  download_cran("syncr", path, NULL, "windows", FALSE, src,
                missing_index_is_error = FALSE, progress = PROGRESS)
  db <- available_packages(file_url(contrib_url(path, "src", NULL)))
  expect_true("syncr" %in% rownames(db))

  ## This should fail because of a missing index
  expect_error(download_cran("syncr", path, "3.2.1", "windows", FALSE, src,
                             progress = PROGRESS))
})
