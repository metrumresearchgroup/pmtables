
library(testthat)
library(pmtables)

context("test-opts")

def <- pt_opts$defaults
pt_opts$reset()

test_that("opt objects", {
  expect_is(pt_opts, "environment")
  expect_is(pmtables:::opts, "environment")
})

test_that("opt defaults", {
  expect_is(pt_opts$defaults, "list")
})

test_that("opt get", {
  esc_a <- pt_opts$escape
  esc_b <- pt_opts$get("escape")
  expect_identical(esc_a,esc_b)
  expect_identical(esc_a,"_")
})

test_that("opt set", {
  pt_opts$set(escape = "__")
  expect_identical(pt_opts$escape,"__")
  pt_opts$escape <- "----"
  expect_identical(pt_opts$escape,"----")
})

test_that("opt reset", {
  a <- pt_opts$escape
  pt_opts$reset()
  b <- pt_opts$escape
  expect_identical(b,def$escape)
  expect_false(a==b)
})

test_that("set internal data", {
  expect_warning(pt_opts$self <- "foobar", "is not a valid option to set")
  expect_warning(pt_opts$set <- 1235, "is not a valid option to set")
})

test_that("as.list", {
  a <- as.list(pt_opts)
  b <- pt_opts$defaults[names(a)]
  expect_identical(a,b)
})

test_that("names", {
  a <- names(pt_opts)
  b <- names(pt_opts$defaults)
  expect_identical(a,b)
})

test_that("bracket dot pm_opts", {
  a <- pt_opts["escape"]
  b <- pt_opts$defaults[names(a)]
  expect_identical(a,b)
  pt_opts$reset()
})

test_that("new object", {
  x <- pmtables:::pt_options()
  form <- formals(pmtables:::pt_options)
  expect_equal(names(form), names(x$defaults))
  rm(x)
})
