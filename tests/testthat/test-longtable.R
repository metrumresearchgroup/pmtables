library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-longtable")

test_that("longtable", {
  out <- get_stable_data(stable_long(data = mtcars, inspect = TRUE))
  expect_match(out$output, "\\begin{longtable}", fixed = TRUE, all=FALSE)
})
