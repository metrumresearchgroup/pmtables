library(testthat)
library(pmtables)
library(dplyr)

context("test-summary-functions.R")

test_that("summary cont_long_fun", {
  x <- rnorm(1000)
  ans <- pmtables:::cont_long_fun(x)
  expect_is(ans,"data.frame")
  expect_named(ans, c("n", "Mean", "Median", "SD", "Min / Max"))
  expect_equal(ans$n, 1000)
  expect_equal(ans$Mean, pmtables:::sig(mean(x)))
  expect_equal(ans$Median, pmtables:::sig(median(x)))
  expect_equal(ans$SD, pmtables:::sig(sd(x)))
  rng <- paste0(pmtables::sig(range(x)), collapse = " / ")
  expect_equal(ans$`Min / Max`, rng)
})

test_that("summary cont_wide_fun", {
  x <- rnorm(1000)
  ans <- pmtables:::cont_wide_fun(x)
  expect_is(ans,"data.frame")
  expect_named(ans, "summary")
  expect_equal(dim(ans), c(1,1))
  txt <- strsplit(ans[[1]], "\\s+")[[1]]
  expect_equal(txt[1], pmtables:::sig(mean(x)))
  SD <- paste0("(", pmtables:::sig(sd(x)), ")")
  expect_equal(txt[2], SD)
  expect_equal(txt[3], "[1000]")
})

test_that("summary n missing", {
  x <- rnorm(1000)
  miss <- sample(seq_len(1000), 10)
  bq <- sample(miss, 5)
  x[miss] <- NA_real_
  bql <- vector("integer", 1000)
  bql[bq] <- 1
  ans1 <- pmtables:::n_missing(x, bql)
  ans2 <- length(miss) - sum(bql)
  expect_equal(ans1, ans2)
})
