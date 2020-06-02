
library(testthat)

context("test-opts")

def <- pt_opts$defaults
pt_opts$reset()

test_that("opt objects", {
  expect_is(pt_opts, "environment")
  expect_is(pmplots:::opts, "environment")
})

test_that("opt defaults", {
  expect_is(pt_opts$defaults, "list")
})

test_that("opt get", {
  dvcol_a <- pt_opts$dv_col
  dvcol_b <- pt_opts$get("dv_col")
  expect_identical(dvcol_a,dvcol_b)
  expect_identical(dvcol_a,"DV")
})

test_that("opt set", {
  pt_opts$set(id_col = "SUBJ")
  expect_identical(pt_opts$id_col,"SUBJ")
  pt_opts$dv_col <- "dv"
  expect_identical(pt_opts$dv_col,"dv")
})

test_that("opt reset", {
  a <- pt_opts$dv_col
  pt_opts$reset()
  b <- pt_opts$dv_col
  expect_identical(b,def$dv_col)
  expect_false(a==b)
})

test_that("set internal data", {
  expect_warning(pt_opts$self <- "foobar", "is not a valid option to set")
  expect_warning(pt_opts$set <- 1235, "is not a valid option to set")
})

test_that("as.list", {
  a <- as.list(pt_opts)
  b <- pt_opts$defaults[names(a)]
  expect_identical(a,b)
})

test_that("bracket dot pm_opts", {
  a <- pt_opts["dv_col", "bq_col"]
  b <- pt_opts$defaults[names(a)]
  expect_identical(a,b)
})

