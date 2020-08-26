library(testthat)

context("test-discrete-table")

test_that("discrete data table - long", {
  data <- pmt_first
  ans <- pt_cat_long(data, cols = "SEXf,RFf,CPf", span = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("discrete - long, summaries", {
  data <- pmt_first
  ans <- pt_cat_long(data, cols = "SEXf,RFf,CPf", span = "STUDYf")
  expect_is(ans,"pmtable")
  res <- as.data.frame(ans$data)
  ns <- length(levels(data$SEXf))
  nrf <- length(levels(data$RFf))
  ncp <- length(levels(data$CPf))
  nr <- ns + nrf + ncp
  expect_equal(nrow(res), nr)
  nc <- ncol(res)
  ans <- pt_cat_long(
    data, cols = "SEXf,RFf,CPf", span = "STUDYf",
    summarize = "right"
  )
  expect_equal(ncol(ans$data), nc)
  expect_equal(nrow(ans$data), nr)
  expect_equal(names(ans$data)[(nc)], "Summary")
  ans <- pt_cat_long(
    data, cols = "SEXf,RFf,CPf", span = "STUDYf",
    summarize = "none"
  )
  expect_equal(nrow(ans$data), nr)
  expect_equal(ncol(ans$data), nc-1)
})

test_that("discrete data table - wide", {
  data <- pmt_first
  ans <- pt_cat_wide(data, cols = "SEXf,RFf,CPf", by = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("discrete - wide, summaries", {
  data <- pmt_first
  ans <- pt_cat_wide(data, cols = "SEXf,RFf,CPf", by = "STUDYf")
  nr <- length(unique(data$STUDYf))
  expect_equal(nrow(ans$data), nr+1)
  expect_match(ans$data$STUDYf[nr+1], "All data", fixed = TRUE)
  ans <- pt_cat_wide(
    data, cols = "SEXf,RFf,CPf", by = "STUDYf",
    summarize = "none"
  )
  expect_equal(nrow(ans$data), nr)
})

test_that("notes - cat-wide", {
  ans <- pt_cat_wide(pmt_first, cols = "FORMf,SEXf")$notes
  expect_is(ans, "character")
  expect_length(ans,2)
  expect_match(ans[1], "count (percent)", fixed = TRUE)
  expect_match(ans[2], "number of records summarized", fixed = TRUE)
})

test_that("notes - cat-long", {
  ans <- pt_cat_long(pmt_first, cols = "STUDYf")$notes
  expect_is(ans, "character")
  expect_length(ans,2)
  expect_match(ans[1], "count (percent)", fixed = TRUE)
  expect_match(ans[2], "number of records summarized", fixed = TRUE)
})

test_that("cat wide table has n", {
  ans <- pt_cat_wide(pmt_first, cols = "FORMf,STUDYf")$data
  expect_true("n" %in% names(ans))
})

test_that("cat long table has cols_extra", {
  ans <- pt_cat_long(pmt_first, cols = "FORMf,STUDYf", span = "SEXf")$cols_extra
  expect_is(ans,"data.frame")
  expect_equal(ans$Summary ,"n = 160")
  expect_equal(ans$male ,"n = 80")
  expect_equal(ans$female ,"n = 80")
})
