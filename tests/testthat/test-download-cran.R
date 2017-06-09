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
