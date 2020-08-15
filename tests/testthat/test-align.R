library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-align")

test_that("align helpers", {
  expect_identical(cols_center(), cols_align(.default = 'c'))
  expect_identical(cols_left(), cols_align(.default = 'l'))
  expect_identical(cols_right(), cols_align(.default = 'r'))
  ali <- cols_center(FOO = 'l', bar = 'r')
  expect_identical(ali$update, list(FOO = 'l', bar = 'r'))
})
