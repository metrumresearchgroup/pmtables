library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-span")

test_that("span split", {
  data <- data.frame(a.A = 1, a.B = 2, C = 3, z.Y = 4, z.Z = 5)
  out <- inspect(data, span_split = colsplit(sep = '.'))
  expect_equal(out$cols_new, c("A", "B", "C", "Y", "Z"))
  expect_equal(out$span_data$span[[1]]$title, c("a", "a", "", "z", "z"))
})

test_that("span split with title", {
  data <- data.frame(a.A = 1, a.B = 2, C = 3, z.Y = 4, z.Z = 5)
  title <- list(a = "First Split", z = "Last Split")
  out <- inspect(data, span_split = colsplit(sep = '.', title = title))
  expect_equal(out$cols_new, c("A", "B", "C", "Y", "Z"))
  utitle <- unique(out$span_data$span[[1]]$title)
  expect_equal(utitle, c("First Split", "", "Last Split"))
})

