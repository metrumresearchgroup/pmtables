#
# context("test-cont-table")
#
# data <- pmtables:::data("id")
#
# test_that("continuous data table - long", {
#   table <- list(WT = "Weight")
#   ans <- pt_cont_long(data,cols="WT,ALB,SCR",panel="STUDYf",table=table)
#   expect_is(ans,"gt_tbl")
# })
#
# test_that("continuous study table - long", {
#   ans <- pt_cont_study(data, cols = "WT,ALB,SCR", study_col = "STUDYf")
#   expect_is(ans,"gt_tbl")
# })
#
# test_that("continuous data table - wide", {
#   ans <- pt_cont_wide(data, cols = "WT,ALB,SCR", panel = "STUDYf")
#   expect_is(ans,"gt_tbl")
# })
#
# test_that("continuous study table - wide", {
#   ans <- pt_cont_study(data, cols = "WT,ALB,SCR", study_col = "STUDYf")
#   expect_is(ans,"gt_tbl")
# })
#
