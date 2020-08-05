
context("test-cont-table")

test_that("continuous data table - long", {
  data <- pmt.first
  table <- list(WT = "Weight")
  ans <- pt_cont_long(data,cols="WT,ALB,SCR",panel="STUDYf",table=table)
  expect_is(ans,"pmtable")
})

test_that("continuous study table - long", {
  data <- pmt.first
  ans <- pt_cont_study(data, cols = "WT,ALB,SCR", study_col = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("continuous data table - wide", {
  data <- pmt.first
  ans <- pt_cont_wide(data, cols = "WT,ALB,SCR", panel = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("continuous study table - wide", {
  data <- pmt.first
  ans <- pt_cont_study(data, cols = "WT,ALB,SCR", study_col = "STUDYf")
  expect_is(ans,"pmtable")
})

