library(testthat)

context("test-discrete-table")

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

test_that("discrete data table - long [PMT-TEST-0073]", {
  data <- pmt_first
  ans <- pt_cat_long(data, cols = "SEXf,RFf,CPf", span = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("discrete - long, summaries [PMT-TEST-0074]", {
  data <- pmt_first
  ans <- pt_cat_long(data, cols = "SEXf,RFf,CPf", span = "STUDYf")
  expect_is(ans,"pmtable")
  res <- as.data.frame(ans$data)
  ns <- length(levels(data$SEXf))
  nrf <- length(levels(data$RFf))
  ncp <- length(levels(data$CPf))
  nr <- ns + nrf + ncp
  expect_equal(nrow(res), nr)
  nc <- ncol(res)
  ans <- pt_cat_long(
    data, cols = "SEXf,RFf,CPf", span = "STUDYf",
    summarize = "right"
  )
  expect_equal(ncol(ans$data), nc)
  expect_equal(nrow(ans$data), nr)
  expect_equal(names(ans$data)[(nc)], "Summary")
  ans <- pt_cat_long(
    data, cols = "SEXf,RFf,CPf", span = "STUDYf",
    summarize = "none"
  )
  expect_equal(nrow(ans$data), nr)
  expect_equal(ncol(ans$data), nc-1)
})

test_that("discrete data table - wide [PMT-TEST-0075]", {
  data <- pmt_first
  ans <- pt_cat_wide(data, cols = "SEXf,RFf,CPf", by = "STUDYf")
  expect_is(ans,"pmtable")
})

test_that("discrete - wide, summaries [PMT-TEST-0076]", {
  data <- pmt_first
  ans <- pt_cat_wide(data, cols = "SEXf,RFf,CPf", by = "STUDYf")
  nr <- length(unique(data$STUDYf))
  expect_equal(nrow(ans$data), nr+1)
  expect_match(ans$data$STUDYf[nr+1], "All data", fixed = TRUE)
  ans <- pt_cat_wide(
    data, cols = "SEXf,RFf,CPf", by = "STUDYf",
    summarize = "none"
  )
  expect_equal(nrow(ans$data), nr)
})

test_that("notes - cat-wide [PMT-TEST-0077]", {
  ans <- pt_cat_wide(pmt_first, cols = "FORMf,SEXf")$notes
  expect_is(ans, "character")
  expect_length(ans,2)
  expect_match(ans[1], "count (percent)", fixed = TRUE)
  expect_match(ans[2], "number of records summarized", fixed = TRUE)
})

test_that("notes - cat-long [PMT-TEST-0078]", {
  ans <- pt_cat_long(pmt_first, cols = "STUDYf")$notes
  expect_is(ans, "character")
  expect_length(ans,2)
  expect_match(ans[1], "count (percent)", fixed = TRUE)
  expect_match(ans[2], "number of records summarized", fixed = TRUE)
})

test_that("cat wide table has n [PMT-TEST-0079]", {
  ans <- pt_cat_wide(pmt_first, cols = "FORMf,STUDYf")$data
  expect_true("n" %in% names(ans))
})

test_that("cat long table has cols_extra [PMT-TEST-0080]", {
  ans <- pt_cat_long(pmt_first, cols = "FORMf,STUDYf", span = "SEXf")$cols_extra
  expect_is(ans,"data.frame")
  expect_equal(ans$Summary ,"n = 160")
  expect_equal(ans$male ,"n = 80")
  expect_equal(ans$female ,"n = 80")
})

test_that("cat wide with spanner breaks [PMT-TEST-0081]", {
  ans <- pt_cat_wide(
    pmt_first,
    cols = c("Formulation ... type" = "FORMf", "SEX" = "SEXf")
  )
  out <- inspect(ans)
  expect_is(ans, "pmtable")
  expect_length(out$span_data$tex, 4)
  expect_match(out$span_data$tex[1], "{Formulation}", fixed = TRUE)
  expect_match(out$span_data$tex[2], "{type}", fixed = TRUE)
  lvls <- c(levels(pmt_first$FORMf),levels(pmt_first$SEXf))
  expect_equal(out$span_data$cols, c("n", lvls))
})

test_that("cat table with missing value [PMT-TEST-0082]", {
  data <- pmt_first
  data$SEXf[20] <- NA_character_
  expect_warning(
    ans <- pt_cat_wide(data, cols = "FORMf,SEXf"),
    regexp = "col `SEXf`: missing values replaced with "
  )
  expect_is(ans, "pmtable")
})

test_that("cat wide with renamed cols [PMT-TEST-0083]", {
  data <- pmt_first
  ans1 <- pt_cat_wide(data, cols = c(Form = "FORMf", Sex = "SEXf"))
  tb <-  list(FORMf = "Form", SEXf = "Sex")
  ans2 <- pt_cat_wide(data, cols = "FORMf,SEXf", table = tb)
  expect_identical(ans1, ans2)
  ans1 <- inspect(ans1)
  sp <- ans1$span_data$span[[1]]
  a <- levels(data$FORMf)
  b <- levels(data$SEXf)
  an <- length(a)
  bn <- length(b)
  expect_equal(sp$newcol, c("n", c(a, b)))
  expect_equal(sp$title, c("", rep("Form", an), rep("Sex", bn)))
})

test_that("cat long with renamed cols [PMT-TEST-0084]", {
  data <- pmt_first
  ans1 <- pt_cat_long(data, cols = c(Form = "FORMf", Sex = "SEXf"))
  tb <-  list(FORMf = "Form", SEXf = "Sex")
  ans2 <- pt_cat_long(data, cols = "FORMf,SEXf", table = tb)
  expect_identical(ans1, ans2)
  ans1 <- inspect(ans1)
  expect_match(ans1$tab[1], "Form")
  expect_match(ans1$tab[5], "Sex")
})
