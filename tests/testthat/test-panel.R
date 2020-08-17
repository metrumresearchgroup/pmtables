library(testthat)
library(pmtables)
library(dplyr)
library(stringr)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-panel")

test_that("panel duplicates", {
  data <- data.frame(A = c(1,1,2,2,1,1), B = 3)
  expect_error(stable(data, panel = as.panel("A")))
  out <- inspect(data, panel = as.panel("A", duplicates_ok = TRUE))
  expect_equal(ncol(out$data),1L)
})

test_that("can't panel with one column", {
  expect_error(
    stable(data.frame(A = 1), panel = as.panel("A")),
    "must have more than one column"
  )
})

test_that("span split with title", {
  data <- data.frame(a.A = 1, a.B = 2, C = 3, z.Y = 4, z.Z = 5)
  title <- list(a = "First Split", z = "Last Split")
  out <- inspect(data, span_split = colsplit(sep = '.', title = title))
  expect_equal(out$cols_new, c("A", "B", "C", "Y", "Z"))
  utitle <- unique(out$span_data$span[[1]]$title)
  expect_equal(utitle, c("First Split", "", "Last Split"))
})

test_that("panel with sumrow", {
  data <- ptdata()
  data$STUDY[6] <- "Summary"
  data$STUDY[13] <- "Summary"
  out <- inspect(
    data,
    panel = "STUDY",
    sumrows = sumrow(data$STUDY=="Summary",label="All")
  )
  tab <- out$tab
  multi <- grepl("multicolumn",tab)
  expect_equal(sum(multi),2)
  all <- grepl("^All", tab)
  expect_equal(sum(all),2)
})

test_that("panel with drop", {
  data <- ptdata()
  out <- inspect(data, panel = "STUDY", drop  = "N,WT,DOSE")
  n <- ncol(data) - 3 - 1
  expect_equal(n, out$nc)
  expect_equal(n, length(out$cols_new))
  expect <- paste0("\\multicolumn{",n,"}")
  ans <- sum(stringr::str_count(out$tab, stringr::fixed(expect)))
  expect_equal(ans,length(unique(data[["STUDY"]])))
})

test_that("panel invalid regex in panel_skip", {
  x <- "\\textbf{c}"
  data <- data.frame(A = c("a", "b", x), B = "B", C = "C")
  out <- inspect(data, panel  = as.panel("A",skip  = x))
  expect_equal(sum(stringr::str_count(out$tab, "multicolumn")),2)

  out <- inspect(data, panel  = as.panel("A", prefix_skip  = x, prefix = "FOO"))
  expect_match(out$tab, "\\textbf{FOO a}", all = FALSE, fixed = TRUE)
  expect_match(out$tab, "\\textbf{FOO b}", all = FALSE, fixed = TRUE)
  expect_match(out$tab, "\\textbf{c}", all = FALSE, fixed = TRUE)
})
