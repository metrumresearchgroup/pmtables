library(testthat)
library(pmtables)

context("test-cat-data")

cols <- pmtables:::new_names(c("CPf", "SEXf", "RFf"))

test_that("discrete data summary long - simple [PMT-TEST-0065]", {
  data <- pmt_first
  ans <- cat_data(data, cols = cols)
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"name")
  expect_identical(names(ans)[2],"level")
  nr <- sum(
    length(unique(data[[cols[1]]])),
    length(unique(data[[cols[2]]])),
    length(unique(data[[cols[3]]]))
  )
  expect_equal(nrow(ans),nr)
  expect_equal(ncol(ans),3)
})

test_that("discrete data summary long - by [PMT-TEST-0066]", {
  data <- pmt_first
  ans <- cat_data(data, cols = cols, by = "STUDY")
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"name")
  expect_identical(names(ans)[2],"level")
  l <- sum(
    length(unique(data[[cols[1]]])),
    length(unique(data[[cols[2]]])),
    length(unique(data[[cols[3]]]))
  )
  expect_equal(nrow(ans),l)
  nby <- length(unique(data[["STUDY"]]))
  expect_equal(ncol(ans),nby + 2)
})

test_that("discrete data summary wide - simple [PMT-TEST-0067]", {
  data <- pmt_first
  ans <- cat_data(data, cols = cols, wide = TRUE)
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],".total")
  l <- sum(
    length(unique(data[[cols[1]]])),
    length(unique(data[[cols[2]]])),
    length(unique(data[[cols[3]]]))
  )
  expect_equal(nrow(ans),1)
  # add 1 for summary, 1 for N
  expect_equal(ncol(ans),l + 1 + 1)
})

test_that("discrete data summary wide - by [PMT-TEST-0068]", {
  data <- pmt_first
  ans <- cat_data(data, cols = cols, wide = TRUE, by = "STUDY")
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"STUDY")
  l <- sum(
    length(unique(data[[cols[1]]])),
    length(unique(data[[cols[2]]])),
    length(unique(data[[cols[3]]]))
  )
  expect_equal(nrow(ans),length(unique(data[["STUDY"]])))
  # add 1 for summary, 1 for N
  expect_equal(ncol(ans),l + 1 + 1)
})

extract_pct <- function(x) {
  x <- strsplit(x, "\\(|\\)")
  x <- sapply(x, "[[", 2)
  as.numeric(x)
}

test_that("discrete data summary - with group denominator [PMT-TEST-0069]", {
  data <- pmt_first
  cols <- c("FORMf")
  # The long summary sums by column
  ans <- cat_data(data, cols = cols, wide = FALSE, by = "STUDYf", denom = "group")
  expect_is(ans,"data.frame")
  ans <- ans[,seq(3, ncol(ans))]
  ans[] <- lapply(ans, extract_pct)
  expect_true(all(colSums(ans)==100))
  # The wide summary sums by row
  ans <- cat_data(data, cols = cols, wide = TRUE, by = "STUDYf", denom = "group")
  expect_is(ans,"data.frame")
  ans <- ans[,seq(3, ncol(ans))]
  ans[] <- lapply(ans, extract_pct)
  expect_true(all(rowSums(ans)==100))
})

test_that("discrete data summary - with total denominator [PMT-TEST-0070]", {
  data <- pmt_first
  cols <- c("FORMf")
  # The long table summs to 100% across everything
  ans <- cat_data(
    data, cols = cols, wide = FALSE, by = "STUDYf", denom = "total",
    panel = "ASIANf"
  )
  expect_is(ans,"data.frame")
  ans <- ans[, seq(4, ncol(ans))]
  ans[] <- lapply(ans, extract_pct)
  summ <- sum(ans)
  expect_true(summ > 99.5 & summ < 100.2)
  # The wide table summs to 100% within column group
  ans <- cat_data(
    data,
    cols = c(cols, "ASIANf"), panel = "SEXf",
    wide = TRUE, by = "STUDYf", denom = "total",
  )
  expect_is(ans,"data.frame")
  ans <- ans[,grepl("ASIAN", names(ans))]
  ans[] <- lapply(ans, extract_pct)
  summ <- sum(ans)
  expect_true(summ > 99.5 & summ < 100.2)
})

test_that("discrete data - wide summary is completed [PMT-TEST-0071]", {
  data <- data.frame(
    ID = 1,
    A = as.character(c(1, 2, 3, 4)),
    B = as.character(c(1, 2, 1, 2)),
    C = as.character(c("a", "b", "a", "b")),
    stringsAsFactors = FALSE
  )
  ans1 <- pt_cat_wide(
    data,
    cols = "A",
    by = "C",
    panel = "B",
    complete = FALSE,
    summarize = "none"
  )$data
  expect_equal(nrow(ans1), 2L)
  expect_equal(ans1$B, as.character(c(1,2)))
  expect_equal(ans1$C, c("a", "b"))

  ans2 <- pt_cat_wide(
    data,
    cols = "A",
    by = "C",
    panel = "B",
    complete = TRUE,
    summarize = "none"
  )$data
  expect_equal(nrow(ans2), 4L)
  expect_equal(ans2$B, as.character(rep(c(1,2), each = 2)))
  expect_equal(ans2$C, rep(c("a", "b"), times = 2))
})

test_that("discrete data - factor levels preserved in completed data [PMT-TEST-0072]", {
  data <- data.frame(
    ID = 1,
    A = as.character(c(1, 2, 3, 4)),
    B = factor(c(1, 2, 1, 2), levels = c(2,1)),
    C = as.character(c("a", "b", "a", "b")),
    stringsAsFactors = FALSE
  )
  ans <- pt_cat_wide(
    data,
    cols = "A",
    by = "C",
    panel = "B",
    complete = TRUE,
    summarize = "none"
  )$data
  expect_equal(as.character(ans$B), as.character(rep(c(2,1), each = 2)))
  expect_equal(ans$C, rep(c("a", "b"), times = 2))
})
