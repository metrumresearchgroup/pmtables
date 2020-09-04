library(testthat)
library(pmtables)

context("test-cont-table")

test_that("continuous data table - long", {
  data <- pmt_first
  table <- list(WT = "Weight")
  ans <- pt_cont_long(data,cols="WT,ALB,SCR",panel="STUDYf",table=table)
  expect_is(ans,"pmtable")
})

test_that("invert panel and cols", {
  data <- pmt_first
  table <- list(WT = "weight", ALB = "albumin", SCR = "creat")
  ans1 <- pt_cont_long(
    data, col = "WT,ALB,SCR", panel = vars(Study = "STUDYf"),
    table = table
  )
  ans2 <- pt_cont_long(
    data, col = "WT,ALB,SCR",
    by = "STUDYf",
    table = table
  )
  u_var1 <- unique(ans1$data$Variable)
  expect_identical(u_var1, c("weight", "albumin", "creat"))
  u_var2 <- unique(ans2$data$Variable)
  expect_identical(levels(u_var2), c("weight", "albumin", "creat"))
  expect_is(ans2$data[[1]], "factor")
  expect_is(ans2$data[[2]], "factor")
  expect_equal(ans1$panel$col, c(STUDYf = "STUDYf"))
  expect_equal(ans1$panel$prefix, "Study")
  expect_equal(ans2$panel$col, "Variable")
  expect_null(ans1$sumrows)
  expect_null(ans2$panel$prefix)
  expect_is(ans2$sumrows, "sumrow")
})

test_that("continuous data table - wide", {
  data <- pmt_first
  ans <- pt_cont_wide(data, cols = "WT,ALB,SCR", panel = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("notes - cont-wide", {
  ans <- pt_cont_wide(pmt_first, cols = "WT,ALB")$notes
  expect_is(ans, "character")
  expect_length(ans,1)
  expect_match(ans, "mean (sd) [count]", fixed = TRUE)
})

test_that("notes - cont-long", {
  ans <- pt_cont_long(pmt_first, cols = "WT,ALB")$notes
  expect_is(ans, "character")
  expect_length(ans,3)
  expect_match(ans[1], "number of records", fixed = TRUE)
  expect_match(ans[2], "standard deviation", fixed = TRUE)
  expect_match(ans[3], "minimum", fixed = TRUE)
})

test_that("cont long table has n", {
  ans <- pt_cont_long(pmt_first, cols = "WT,ALB")$data
  expect_true("n" %in% names(ans))
})

