library(testthat)
library(pmtables)
library(dplyr)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-clear_reps")

test_that("clear replicates", {
  data <- data.frame(A = c(rep("a", 3), rep("b",2), rep('c', 3)))
  data$B <- letters[seq(nrow(data))]
  out <- tab_clear_reps(data, "A")
  ans <- c("a", rep("",2), "b", "", "c", rep("",2))
  expect_equal(out$A,ans)
})

test_that("clear grouped replicates - 1", {
  data <- pmt_first
  data <- count(data, STUDYf, FORMf, SEXf)
  data <- mutate(data, across(c(STUDYf,FORMf,SEXf),as.character))
  ans <- tab_clear_reps(
    data[seq(6),],
    clear_reps = "FORMf",
    panel = rowpanel("STUDYf")
  )
  expect_identical(
    ans$STUDYf,
    c(rep(data$STUDYf[1],5), data$STUDYf[6])
  )
  expect_identical(
    ans$FORMf,
    c("tablet", "", "capsule", "", "troche", "tablet")
  )
})

test_that("clear grouped replicates - 2", {
  data <- pmt_first
  data <- count(data,STUDYf,FORMf,SEXf)
  data <- mutate(data, across(STUDYf:SEXf, as.character))
  ans <- clear_grouped_values(data[1:6,], cols = c("STUDYf", "FORMf"))
  expect_equal(ans$STUDYf, c(data$STUDYf[1], rep("",4), data$STUDYf[6]))
  expect_equal(ans$FORMf, c(data$FORMf[1], "", data$FORMf[3], "",
                            data$FORMf[5], data$FORMf[6]))
})

