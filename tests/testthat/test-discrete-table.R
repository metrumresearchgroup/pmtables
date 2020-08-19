
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
  expect_equal(nrow(res), nr + 1)
  expect_equal(res[nr+1,"\\ "], "All data")
  nc <- ncol(res)
  ans <- pt_cat_long(
    data, cols = "SEXf,RFf,CPf", span = "STUDYf",
    summarize = "both"
  )
  expect_equal(ncol(ans$data), nc + 1)
  expect_equal(nrow(ans$data), nr + 1)
  expect_equal(names(ans$data)[(nc+1)], "\\textbf{All Groups}")
  ans <- pt_cat_long(
    data, cols = "SEXf,RFf,CPf", span = "STUDYf",
    summarize = "none"
  )
  expect_equal(nrow(ans$data), nr)
  expect_equal(ncol(ans$data), nc)
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

