library(testthat)
library(pmtables)

inspect_long <- function(...) {
  get_stable_data(stable_long(..., inspect = TRUE))
}

context("test-longtable")

test_that("test-longtable stable_long", {
  notes <- letters[1:3]
  out <- inspect_long(data = mtcars, notes = notes)
  expect_match(out$output, "\\begin{longtable}", fixed = TRUE, all=FALSE)
})

test_that("longtable - caption text", {
  out <- inspect_long(data = mtcars, lt_cap_text = "Text caption")
  test <- "\\caption{Text caption}"
  expect_match(out$output, test, fixed = TRUE, all=FALSE)
})

test_that("longtable - caption macro", {
  out <- inspect_long(data = mtcars, lt_cap_macro = "mylongtable")
  test <- "\\caption{\\mylongtable}"
  expect_match(out$output, test, fixed = TRUE, all = FALSE)
  out <- inspect_long(data = mtcars, lt_cap_macro = "\\mylongtable")
  expect_match(out$output, test, fixed = TRUE, all = FALSE)
  expect_error(stable_long(data = mtcars, lt_cap_macro = "foo123",
                           "invalid for use in latex"))
})
