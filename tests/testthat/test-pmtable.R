library(testthat)
library(pmtables)

context("test-pmtable")

test_that("test-pmtable as_stable", {
  pmtab <- pt_cont_long(pmt_first, cols = "WT,SCR")
  expect_is(pmtab, "pmtable")
  expect_is(pmtab, "list")
  expect_is(pmtab$data, "data.frame")
  ans <- as_stable(pmtab)
  expect_is(ans, "stable")
})

test_that("test-pmtable as_stable long", {
  pmtab <- pt_cont_long(pmt_first, cols = "WT,SCR")
  expect_is(pmtab, "pmtable")
  expect_is(pmtab, "list")
  expect_is(pmtab$data, "data.frame")
  ans <- as_stable(pmtab)
  expect_is(ans, "stable")
})


