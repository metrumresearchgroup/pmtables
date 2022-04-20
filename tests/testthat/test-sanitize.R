library(testthat)
library(pmtables)
library(dplyr)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-sanitize")

test_that("tab-escape [PMT-TEST-0177]", {
  x <- c("a", "a_b", "a $\\Sigma$ b")
  ans <- pmtables:::tab_escape(x)
  expect_equal(ans, c(x[1], "a\\_b", x[3]))
})

test_that("col names are sanitized [PMT-TEST-0178]", {
  data <- tibble(A = 1, "B %" = 2, "C_3" = 3)
  x <- inspect(data)
  expect_equal(x$cols_tex, "A & B \\% & C\\_3 \\\\")
})

test_that("units are sanitized [PMT-TEST-0179]", {
  data <- tibble(A = 1, "B %" = 2, "C_3" = 3)
  x <- inspect(data, units = list(C_3  = "$\\mu$g"))
  expect_match(x$cols_tex[1], " &  & C\\_3", fixed = TRUE)
  expect_match(x$cols_tex[2], "A & B \\% & $\\mu$g", fixed = TRUE)
})

test_that("files are sanitized [PMT-TEST-0180]", {
  data <- tibble(A = 1, "B %" = 2, "C_3" = 3)
  x <- inspect(data, r_file = "foo_bar.R", output_file = "foo\\_bar.tex")
  expect_match(x$notes[1], "foo\\_bar.R", fixed  = TRUE)
  expect_match(x$notes[2], "foo\\_bar.tex", fixed  = TRUE)
})

test_that("notes are sanitized [PMT-TEST-0181]", {
  data <- tibble(A = 1, "B %" = 2, "C_3" = 3)
  notes <- c("foo_bar note", "foo_bar %", "$\\mu$ %")
  x <- inspect(data, notes = notes)
  expect_match(x$notes[1], "foo\\_bar note", fixed  = TRUE)
  expect_match(x$notes[2], "foo\\_bar \\%", fixed  = TRUE)
  expect_match(x$notes[3], notes[3], fixed  = TRUE)
})

test_that("span titles are sanitized [PMT-TEST-0182]", {
  data <- ptdata()
  sp <- list(
    colgroup("Percent_true (%)", CRCL:AGE),
    colgroup("Prcnt (\\%)", FORM:N, level = 2)
  )
  x <- inspect(data, span = sp)
  ans <- x$span_data$tex
  expect_match(ans[1], "Prcnt (\\%)", fixed = TRUE)
  expect_match(ans[3], "Percent\\_true (\\%)", fixed  = TRUE)
})

test_that("table contents are sanitized [PMT-TEST-0183]", {
  data <- as.data.frame(matrix(letters[1:16], ncol = 4), stringsAsFactors=FALSE)
  data[2,3] <- "foo_bar"
  data[4,3] <- "percent (%)"
  data[2,2] <- "$\\mug$"
  data[1,1] <- "foo\\_bar [%]"
  out <- inspect(data = data)$tab
  expect_match(out[2], "foo\\_bar", fixed = TRUE, all = FALSE)
  expect_match(out[4], "\\%", fixed = TRUE, all = FALSE)
  expect_match(out[2],"$\\mug$", fixed = TRUE, all = FALSE)
  expect_match(out[1], "foo\\_bar", fixed = TRUE, all = FALSE)
  expect_match(out[1], "[%]", fixed = TRUE, all = FALSE)
})
