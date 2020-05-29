
context("test-discrete-table")

data <- pmtables:::data("id")

test_that("discrete data table - long", {
  ans <- pt_cat_long(data, cols = "SEXf,RFf,CPf", by = "STUDYf")
  expect_is(ans,"gt_tbl")
})

test_that("discrete data table - wide", {
  ans <- pt_cat_wide(data, cols = "SEXf,RFf,CPf", by = "STUDYf")
  expect_is(ans,"gt_tbl")
})

