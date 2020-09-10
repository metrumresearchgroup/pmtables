library(testthat)
library(dplyr)

context("test-as_stable")

test_that("pass con to st_wrap in as_stable", {
  x <- pt_cont_long(pmt_first, cols = "WT")
  expect_silent(as_stable(x))
  expect_output(as_stable(x, wrapw = TRUE))
  expect_silent(as_stable(x, wrap = TRUE))
  x <- capture.output(as_stable(x, wrapw = TRUE))
  n <- sum(grepl("begin{threeparttable", x, fixed = TRUE))
  expect_equal(n,1)
})
