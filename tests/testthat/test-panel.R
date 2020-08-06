library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-panel")

test_that("panel duplicates", {
  data <- data.frame(A = c(1,1,2,2,1,1), B = 3)
  expect_error(stable(data, panel = as.panel("A")))
  out <- inspect(data, panel = as.panel("A", duplicates_ok = TRUE))
  expect_equal(ncol(out$data),1L)
})

test_that("can't panel with one column", {
  expect_error(
    stable(data.frame(A = 1), panel = as.panel("A")),
    "must have more than one column"
  )
})

test_that("span split with title", {
  data <- data.frame(a.A = 1, a.B = 2, C = 3, z.Y = 4, z.Z = 5)
  title <- list(a = "First Split", z = "Last Split")
  out <- inspect(data, span_split = colsplit(sep = '.', title = title))
  expect_equal(out$cols_new, c("A", "B", "C", "Y", "Z"))
  utitle <- unique(out$span_data$span[[1]]$title)
  expect_equal(utitle, c("First Split", "", "Last Split"))
})

