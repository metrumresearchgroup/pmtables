
context("test-discrete-table")

test_that("discrete data table - long", {
  data <- pmt_first
  ans <- pt_cat_long(data, cols = "SEXf,RFf,CPf", by = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("discrete data table - wide", {
  data <- pmt_first
  ans <- pt_cat_wide(data, cols = "SEXf,RFf,CPf", by = "STUDYf")
  expect_is(ans,"pmtable")
})

