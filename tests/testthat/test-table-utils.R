
library(testthat)
library(pmtables)

context("test-utils-table.R")

test_that("tex_bold and tex_it", {
  expect_error(tex_bold(1))
  expect_error(tex_it(1))
  ans <- tex_bold("a")
  expect_identical(ans, "\\textbf{a}")
  ans <- tex_it("a")
  expect_identical(ans, "\\textit{a}")
  ans <- tex_bold(c("a", "bxyz", "c"), "b")
  expect_identical(ans[1], "a")
  expect_identical(ans[2], "\\textbf{bxyz}")
  expect_identical(ans[3], "c")
  ans <- tex_bold(c("a", "", "c"))
  expect_identical(ans[1], "\\textbf{a}")
  expect_identical(ans[2], "")
  expect_identical(ans[3], "\\textbf{c}")
})

test_that("test-table-utils-stable_save", {
  tmp <- tempfile()
  x <- stable(data.frame(a = 1), output_file = tmp)
  stable_save(x)
  expect_true(file.exists(tmp))
  read <- readLines(tmp)
  expect_identical(as.character(x), read)
  expect_error(stable_save(as.character(x)), "x is not an 'stable' object")
  expect_error(stable_save(x, file = NULL), "the value of 'file' is NULL")
  x <- stable(data.frame(a = 1))
  expect_error(stable_save(x), "and the 'file' argument is missing")
})

test_that("util - paste units", {
  cols <- LETTERS[c(2,5,4,3,1)]
  units <- list(C = "pounds", X = "none", B = "mg", D = "kg", Z = "liters")
  cols_new <- pmtables:::paste_units(cols, units)
  expect_equal(
    cols_new,
    c("B mg", "E", "D kg", "C pounds", "A")
  )
})
