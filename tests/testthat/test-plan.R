context("plan")

test_that("upgraded CRAN package", {
  lib <- tempfile()

  db <- available_packages("https://cran.rstudio.com", "windows", NULL)

  packages <- "devtools"
  plan <- plan_installation(packages, db, lib, "upgrade")
  expect_true(all(plan$binary))

  db2 <- db
  db2$src["curl", "Version"] <- alter_version(db2$src["curl", "Version"], TRUE)

  plan2 <- plan_installation(packages, db2, lib, "upgrade")
  expect_false(all(plan2$binary))
  i <- plan2$packages == "curl"
  expect_false(plan2$binary[i])
  expect_true(all(plan2$binary[!i]))
})
