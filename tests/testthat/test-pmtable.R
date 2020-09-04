library(testthat)
library(dplyr)

context("test-pmtable")

test_that("test-pmtable as_stable long", {
  out <- pt_cont_wide(pmt_first, cols = "WT")
  expect_is(out, "pmtable")
  expect_is(out, "list")
  ans <- stable(out, long = TRUE)
  expect_is(ans, "stable_long")
})

test_that("test-pmtable as_stable", {
  out <- pt_cont_wide(pmt_first, cols = "WT")
  expect_is(out, "pmtable")
  expect_is(out, "list")
  ans <- stable(out)
  expect_is(ans, "stable")
})
