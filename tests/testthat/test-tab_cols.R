library(testthat)
library(pmtables)


context("test-tab_cols")

# issue-62
test_that("underscore doesn't get escaped in rename", {
  cols <- c("A", "B", "C_f", "D")
  renam <- c(X = "C_f")
  out <- tab_cols(cols, col_rename = renam)
  expect_identical(out$new[3], "X")
})

test_that("cols are not renamed when no match", {
  cols <- letters[1:5]
  re_label <- LETTERS[1:5]
  out <- pmtables:::rename_cols(cols, re_label)
  expect_identical(out, cols)
  re_label <- c("A" = "AA", "B" = "BB")
  out <- pmtables:::rename_cols(cols, re_label)
  expect_identical(out, cols)
})

test_that("cols are renamed", {
  cols <- letters[1:5]
  re_label <- c("A" = "a", "DD" = "d")
  out <- pmtables:::rename_cols(cols, re_label)
  expect_identical(out, c("A", "b", "c", "DD", "e"))
})
