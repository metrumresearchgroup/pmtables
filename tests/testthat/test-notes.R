library(testthat)
library(pmtables)
library(tibble)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-notes")

test_that("tpt notes", {
  data <- tibble(a = 1)
  x <- inspect(data = data, notes = c("abcd", "xyz"))
  expect_equal(x$notes, c("abcd", "xyz"))
  expect_true(exists("tpt_notes", x))
  expect_match(x$tpt_notes,"begin{tablenotes}", fixed = TRUE, all = FALSE)
  expect_match(x$tpt_notes,"item abcd", fixed = TRUE, all = FALSE)
  expect_match(x$tpt_notes,"item xyz", fixed = TRUE, all = FALSE)
})

test_that("mini notes", {
  data <- tibble(a = 1)
  conf <- noteconf(
    type = "minipage", table_skip = 0.6, note_skip = 0.1,
    width = 0.62
  )
  x <- inspect(
    data = data, notes = c("abcd", "xyz"),
    note_config = conf
  )
  expect_equal(x$notes, c("abcd", "xyz"))
  expect_true(exists("mini_notes", x))
  expect_match(x$mini_notes,"begin{minipage}{0.62\\linewidth", fixed = TRUE, all = FALSE)
  expect_match(x$mini_notes,"abcd \\newline", fixed = TRUE, all = FALSE)
  expect_match(x$mini_notes,"xyz \\newline", fixed = TRUE, all = FALSE)
  expect_match(x$mini_notes,"end{minipage}", fixed = TRUE, all = FALSE)
  expect_equal(x$mini_notes[1], "\\vskip 0.6cm", fixed = TRUE, all = FALSE)
  expect_match(x$mini_notes, "\\vskip 0.1cm", fixed = TRUE, all = FALSE)
})

test_that("notes escape", {
  data <- tibble(a = 1)
  x <- inspect(data = data, notes = "abc_def")
  expect_equal(x$notes, c("abc\\_def"))
})

test_that("files", {
  data <- tibble(a = 1)
  x <- inspect(
    data = data,
    notes = "abcdef",
    r_file = "foo.R",
    output_file = "foo.tex"
  )
  no <- c(
    "abcdef",
    "Source code: foo.R",
    "Source file: foo.tex"
  )
  expect_equal(x$notes, no)
  expect_match(
    x$tpt_notes, "begin{tablenotes}",
    fixed = TRUE, all = FALSE
  )
  expect_match(
    x$tpt_notes, "item abcdef",
    fixed = TRUE, all = FALSE
  )
  expect_match(
    x$tpt_notes, "item Source code: foo.R",
    fixed = TRUE, all = FALSE
  )
  expect_match(
    x$tpt_notes,"item Source file: foo.tex",
    fixed = TRUE, all = FALSE
  )
})
