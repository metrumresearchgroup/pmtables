
library(testthat)
library(pmtables)

context("test-utils-table.R")

test_that("tex_bold and tex_it", {
  expect_error(tex_bold(1))
  expect_error(tex_it(1))
  ans <- tex_bold("a")
  expect_identical(ans, "{\\bf a}")
  ans <- tex_it("a")
  expect_identical(ans, "{\\it a}")
  ans <- tex_bold(c("a", "bxyz", "c"), "b")
  expect_identical(ans[1], "a")
  expect_identical(ans[2], "{\\bf bxyz}")
  expect_identical(ans[3], "c")
  ans <- tex_bold(c("a", "", "c"))
  expect_identical(ans[1], "{\\bf a}")
  expect_identical(ans[2], "")
  expect_identical(ans[3], "{\\bf c}")
})

