library(testthat)
library(pmtables)

context("test-yaml_as_df")

sysfile <- function(x) {
  system.file(
    "yaml",
    x,
    package = "pmtables"
  )
}

test_that("read simple table", {
  file <- sysfile("table.yml")
  expect_is(yaml_as_df(file), "data.frame")
})

test_that("read study summary table", {
  file <- sysfile("studySummary.yaml")
  expect_is(yaml_as_df(file), "data.frame")
})

test_that("read prototyped table", {
  file <- sysfile("prototype.yaml")
  expect_is(yaml_as_df(file), "data.frame")
  file <- sysfile("prototype-error.yaml")
  expect_error(yaml_as_df(file))
})
