
library(testthat)
library(pmtables)

context("test-preview.R")

test_that("wrap stable output in landscape", {
  out <- stable(stdata())
  out.ls <- as_lscape(out)
  expect_true(is_lscape(out.ls))
  tex <- pt_wrap(out.ls, con = NULL)
  expect_match(tex[1], "=latex", fixed = TRUE)
  expect_match(tex[2], "begin{landscape}", fixed = TRUE)
})

