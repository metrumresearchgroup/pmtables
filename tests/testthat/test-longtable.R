library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-longtable")

test_that("longtable", {
  notes <- letters[1:3]
  out <- get_stable_data(stable_long(data = mtcars, inspect = TRUE, notes = notes))
  expect_match(out$output, "\\begin{longtable}", fixed = TRUE, all=FALSE)
})

