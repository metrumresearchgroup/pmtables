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

