library(testthat)
library(dplyr)

context("test-as_stable")

inspect_long <- function(...) {
  get_stable_data(stable_long(..., inspect = TRUE))
}

test_that("pass con to st_wrap in as_stable", {
  x <- pt_cont_long(pmt_first, cols = "WT")
  expect_silent(as_stable(x))
  expect_output(as_stable(x, wrapw = TRUE))
  expect_silent(as_stable(x, wrap = TRUE))
  x <- capture.output(as_stable(x, wrapw = TRUE))
  n <- sum(grepl("begin{threeparttable", x, fixed = TRUE))
  expect_equal(n,1)
})

test_that("pass args to stable_long when coercing pmtable", {
  x <- pt_cont_long(pmt_first, cols = "WT,ALB,SCR")
  y <- stable_long(x, lt_cap_text = "test pass caption")
  n <- grep("caption", y)
  expect_length(n, 1)
  expect_match(y[n], "test pass caption")
})

test_that("pass args to stable when coercing pmtable", {
  x <- pt_cont_wide(pmt_first, cols = "WT,ALB,SCR")
  y <- stable(x, notes = "test pass notes")
  expect_match(y, "item test pass notes", all=FALSE)
})
