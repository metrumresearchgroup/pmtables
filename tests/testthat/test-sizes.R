library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-sizes")

test_that("test-sizes-fontsize", {
  out <- inspect(ptdata(), sizes = tab_size(font = "tiny"))
  expect_match(out$output[1], "\\tiny", fixed = TRUE)
})

test_that("test-sizes-rowspace", {
  out <- inspect(ptdata(), sizes = tab_size(row = 1.6))
  expect_match(out$output, "renewcommand{\\arraystretch}{1.6}", fixed = TRUE, all=FALSE)
})
