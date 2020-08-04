library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-hline")

data <- tibble(
  a = c(1,2,3,1,2),
  b = c(4,5,6,1,2),
  c = c(7,8,9,1,2),
  d = c("a", "a", "b", "a", "b")
  )

test_that("hline at", {
  x <- inspect(data = data, hline_at = c(2,4))
  ans <- grep("hline", x$tab)
  expect_equal(c(2,4)-1, ans)
})

test_that("hline from", {
  x <- inspect(data = data, hline_from = "d")
  ans <- grep("hline", x$tab)
  expect_equal(c(3,4,5)-1, ans)
  expect_error(inspect(data = data, hline_from = "kyle"))
})

