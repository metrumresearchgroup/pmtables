library(testthat)
library(pmtables)

context("test-cat-data")

cols <- pmtables:::new_names(c("CPf", "SEXf", "RFf"))

test_that("discrete data summary long - simple", {
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

test_that("discrete data summary long - by", {
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

test_that("discrete data summary wide - simple", {
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

test_that("discrete data summary wide - by", {
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

test_that("discrete data summary - with group denominator", {
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

test_that("discrete data summary - with total denominator", {
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
