library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-cont-table")

test_that("continuous data table - long [PMT-TEST-0017]", {
  data <- pmt_first
  table <- list(WT = "Weight")
  ans <- pt_cont_long(data,cols="WT,ALB,SCR",panel="STUDYf",table=table)
  expect_is(ans,"pmtable")
})

test_that("invert panel and cols [PMT-TEST-0018]", {
  data <- pmt_first
  table <- list(WT = "weight", ALB = "albumin", SCR = "creat")
  ans1 <- pt_cont_long(
    data, col = "WT,ALB,SCR", panel = dplyr::vars(Study = "STUDYf"),
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

test_that("pass by to pt_cont_long with no all data summary gh-329", {
  table <- list(WT = "weight", ALB = "albumin", SCR = "creat")
  ans <- pt_cont_long(
    pmt_first,
    col = "WT,ALB,SCR",
    by = c(Study = "STUDYf"),
    summarize_all = FALSE,
    table = table
  )
  expect_is(ans, "pmtable")
  expect_identical(ans$panel$col, "Variable")
})

test_that("continuous data table - wide [PMT-TEST-0019]", {
  data <- pmt_first
  ans <- pt_cont_wide(data, cols = "WT,ALB,SCR", panel = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("cont wide with renamed cols [PMT-TEST-0020]", {
  data <- pmt_first
  ans1 <- pt_cont_wide(data, cols = c(wt = "WT", alb = "ALB"))
  tb <-  list(WT = "wt", ALB = "alb")
  ans2 <- pt_cont_wide(data, cols = "WT,ALB", table = tb)
  expect_identical(ans1, ans2)
  ans1 <- inspect(ans1)
  expect_equal(ans1$cols_new, c("wt", "alb"))
  expect_equal(names(ans1$data), c("WT", "ALB"))
})

test_that("cont long with renamed cols [PMT-TEST-0021]", {
  data <- pmt_first
  ans1 <- pt_cont_long(data, cols = c(`a a g` = "AAG", `b m i` = "BMI"))
  tb <-  list(BMI = "b m i", AAG = "a a g")
  ans2 <- pt_cont_long(data, cols = "AAG,BMI", table = tb)
  expect_identical(ans1, ans2)
  ans1 <- inspect(ans1)
  variable <- substr(ans1$tab, 1, 5)
  expect_equal(variable[1], c("a a g"))
  expect_equal(variable[2], c("b m i"))
})

test_that("notes - cont-wide [PMT-TEST-0022]", {
  ans <- pt_cont_wide(pmt_first, cols = "WT,ALB")$notes
  expect_is(ans, "character")
  expect_length(ans,1)
  expect_match(ans, "mean (sd) [count]", fixed = TRUE)
})

test_that("notes - cont-long [PMT-TEST-0023]", {
  ans <- pt_cont_long(pmt_first, cols = "WT,ALB")$notes
  expect_is(ans, "character")
  expect_length(ans,3)
  expect_match(ans[1], "number of records", fixed = TRUE)
  expect_match(ans[2], "standard deviation", fixed = TRUE)
  expect_match(ans[3], "minimum", fixed = TRUE)
})

test_that("cont long table has n [PMT-TEST-0024]", {
  ans <- pt_cont_long(pmt_first, cols = "WT,ALB")$data
  expect_true("n" %in% names(ans))
})

test_that("cont table all missing [PMT-TEST-0025]", {
  data <- dplyr::tibble(ID = 1:30,WT = runif(30,1,10), SCR = NA_real_, ALB = WT)
  a <- pt_cont_wide(data, cols = "WT,SCR,ALB", na_fill = NULL)
  b <- pt_cont_long(data, cols = "WT,SCR,ALB", na_fill = NULL)
  expect_equal(names(a$data), c("WT", "ALB"))
  expect_equal(b$data$Variable, c("WT", "ALB"))
  c <- pt_cont_wide(data, cols = "WT,SCR,ALB")
  d <- pt_cont_long(data, cols = "WT,SCR,ALB")
  expect_equal(names(c$data), c("WT", "SCR", "ALB"))
  expect_equal(c$data$SCR, "--")
  expect_equal(d$data$Variable, c("WT", "SCR", "ALB"))
  expect_equal(d$data$n[2], "--")
  expect_equal(d$data$Mean[2], "--")
  expect_equal(d$data$Median[2], "--")
  expect_equal(d$data$SD[2], "--")
  expect_equal(d$data$n[2], "--")
  expect_equal(d$data$`Min / Max`[2], "--")
  e <- pt_cont_wide(data, cols = "WT,SCR,ALB", na_fill = "NA")
  expect_equal(e$data$SCR, "NA")
  f <- pt_cont_long(data, cols = "WT,SCR,ALB", na_fill = "NA")
  expect_equal(f$data$n[2], "NA")
  expect_equal(f$data$Mean[2], "NA")
  expect_equal(f$data$Median[2], "NA")
  expect_equal(f$data$SD[2], "NA")
  expect_equal(f$data$`Min / Max`[2], "NA")
})

test_that("pt_cont_x run with no ID column", {
  data <- pmt_first
  tab_wide_1 <- pt_cont_wide(data, cols = c("WT", "AGE", "CRCL"))
  tab_long_1 <- pt_cont_long(data, cols = c("WT", "AGE", "CRCL"))
  expect_is(tab_wide_1, "pmtable")
  expect_is(tab_long_1, "pmtable")

  data$ID <- NULL
  tab_wide_2 <- pt_cont_wide(data, cols = c("WT", "AGE", "CRCL"))
  tab_long_2 <- pt_cont_long(data, cols = c("WT", "AGE", "CRCL"))
  expect_is(tab_wide_2, "pmtable")
  expect_is(tab_long_2, "pmtable")

  expect_identical(tab_wide_1$data, tab_wide_2$data)
  expect_identical(tab_long_1$data, tab_long_2$data)
})
