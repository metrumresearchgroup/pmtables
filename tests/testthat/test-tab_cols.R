library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

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

test_that("cols are replaced", {
  data <- data.frame(A = 1, B = 2, C = 3)
  out <- inspect(data, col_replace = c("X", "Y", "Z"))
  expect_equal(out$cols_new, c("X", "Y", "Z"))
  expect_error(stable(data, col_replace = c("X", "Y")))
})

test_that("cols are bold", {
  data <- tibble(a = 1, b = 2, c = 3)
  x <- inspect(data, col_bold = TRUE)
  expect_true(grepl("textbf", x$cols_tex))
  x <- inspect(data)
  expect_false(grepl("textbf", x$cols_tex))
})

test_that("units", {
  u <- list(b = "in",kyle = "baron", dd = 5, a = "mg")
  data <- tibble(a = 1, b = 2, c = 3)
  x <- inspect(data,units = u)
  expect_match(x$cols_tex[2], "mg & in & c")
  expect_warning(
    inspect(data, units = list(foo = "bar")),
    "no valid units"
  )
})

test_that("col title breaks", {
  data <- tibble(a = 1, b = 2, c = 3)
  x <- inspect(
    data,
    col_rename  =  c("Metrum RG" = "b")
  )
  expect_length(x$cols_tex,1)
  expect_match(x$cols_tex, "a & Metrum RG", fixed = TRUE)
  x <- inspect(
    data,
    col_rename  =  c("Metrum  ... RG...CT" = "b"),
    units = list(a = "A", b  = "B")
  )
  expect_length(x$cols_tex,4)
  expect_match(x$cols_tex[1], " & Metrum & ")
  expect_match(x$cols_tex[2], " & RG & ")
  expect_match(x$cols_tex[3], "a & CT & ")
  expect_match(x$cols_tex[4], "A & B & c")
})

test_that("column is dropped", {
  data <- tibble(a = 1, b = 2, z = 25, c = 3)
  x <- inspect(data, drop = "z")
  expect_identical(x$cols, c("a", "b", "c"))
})

