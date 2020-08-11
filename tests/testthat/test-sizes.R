library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-sizes")

test_that("test-sizes-fontsize", {
  out <- inspect(ptdata(), sizes = tab_size(font = "tiny"))
  expect_match(out$output[1], "\\tiny")
})

