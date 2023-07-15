library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-align")

test_that("align helpers [PMT-TEST-0001]", {
  expect_identical(cols_center(), cols_align(.default = 'c'))
  expect_identical(cols_left(), cols_align(.default = 'l'))
  expect_identical(cols_right(), cols_align(.default = 'r'))
  ali <- cols_center(FOO = 'l', bar = 'r')
  expect_identical(ali$update, list(FOO = 'l', bar = 'r'))
})

test_that("update align objects", {
  a <- cols_align(A = "l", .r = "B,C,D", .default = "c")
  expect_is(a, "aligncol")

  b <- update(a, .default = "l")
  expect_is(b, "aligncol")
  expect_identical(a$default, "c")
  expect_identical(b$default, "l")

  b <- update(a, .coltype = "m")
  expect_is(b, "aligncol")
  expect_identical(a$coltype, "p")
  expect_identical(b$coltype, "m")

  b <- update(a, .outer = "lr")
  expect_is(b, "aligncol")
  expect_identical(a$outer, "none")
  expect_error(update(a, .outer = "kyle"), "'arg' should be")

  b <- update(a, C = "c", E = "r")
  expect_is(b, "aligncol")
  expect_identical(
    names(b$update),
    c("A", "B", "C", "D", "E")
  )
  expect_identical(b$update$A, "l")
  expect_identical(b$update$B, "r")
  expect_identical(b$update$C, "c")
  expect_identical(b$update$D, "r")
  expect_identical(b$update$E, "r")
})
