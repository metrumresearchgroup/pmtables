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

test_that("read simple table [PMT-TEST-0246]", {
  file <- sysfile("table.yml")
  expect_is(yaml_as_df(file), "data.frame")
  file <- sysfile("prototype-error-2.yml")
  expect_error(suppressMessages(yaml_as_df(file)), msg = "invalid yaml input")
  expect_message(try(yaml_as_df(file), silent = TRUE), msg = "row data in yaml")
})

test_that("read study summary table [PMT-TEST-0247]", {
  file <- sysfile("studySummary.yaml")
  expect_is(yaml_as_df(file), "data.frame")
})

test_that("read prototyped table [PMT-TEST-0248]", {
  file <- sysfile("prototype.yaml")
  expect_is(yaml_as_df(file), "data.frame")
  file <- sysfile("prototype-error.yaml")
  expect_error(yaml_as_df(file))
})

test_that("table contains .row names", {
  file <- sysfile("table.yml")
  df <- yaml_as_df(file)
  expect_is(df, "data.frame")
  expect_true(names(df)[1] == ".row")
  df <- yaml_as_df(file, row_var = "ROW")
  expect_true(names(df)[1] == "ROW")
  df <- yaml_as_df(file, row_var = NULL)
  expect_true(names(df)[1] == "study")

})


test_that(".row is renamed if it exists", {
  yaml <- '
  a:
    b: 2
    .row: 1
    .row_1: 11
  aa:
    b: 3
    .row: 2
    .row_1: 22
  '
  expect_message(
    df <- yaml_as_df(textConnection(yaml)),
    "already exists; saving row names to"
  )
  expect_is(df, "data.frame")
  expect_true(names(df)[1] == ".row_2")
})

