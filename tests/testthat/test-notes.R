library(testthat)
library(pmtables)
library(tibble)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-notes")

test_that("tpt notes [PMT-TEST-0153]", {
  data <- tibble(a = 1)
  x <- inspect(data = data, notes = c("abcd", "xyz"))
  expect_equal(x$notes, c("abcd", "xyz"))
  expect_true(exists("tpt_notes", x))
  expect_match(x$tpt_notes,"begin{tablenotes}", fixed = TRUE, all = FALSE)
  expect_match(x$tpt_notes,"item abcd", fixed = TRUE, all = FALSE)
  expect_match(x$tpt_notes,"item xyz", fixed = TRUE, all = FALSE)
})

test_that("mini notes [PMT-TEST-0154]", {
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
  expect_equal(x$mini_notes[2], "\\vspace{0.6cm}", fixed = TRUE, all = FALSE)
  expect_match(x$mini_notes, "\\vspace{0.1cm}", fixed = TRUE, all = FALSE)
})

test_that("notes escape [PMT-TEST-0155]", {
  data <- tibble(a = 1)
  x <- inspect(data = data, notes = "abc_def")
  expect_equal(x$notes, c("abc\\_def"))
})

test_that("test-notes-files [PMT-TEST-0156]", {
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

test_that("test-notes-basename-only [PMT-TEST-0157]", {
  data <- tibble(a = 1)
  x <- inspect(
    data = data,
    notes = "abcdef",
    r_file = "./script/foo.R",
    output_file = "../data/table/foo.tex"
  )
  expect_match(
    x$tpt_notes, "item Source file: foo.tex",
    fixed = TRUE, all = FALSE
  )
  expect_match(
    x$tpt_notes, "item Source code: ./script/foo.R",
    fixed = TRUE, all = FALSE
  )
})

test_that("brackets in table notes are sanitized", {
  notes <- c("[1,2]", "(3,4]", "(9,10)")
  tab <- stable(stdata(), notes = notes, inspect = TRUE)
  tabnotes <- get_stable_data(tab)$notes
  m <- sum(grepl("lbrack", tabnotes))
  expect_equal(m, 1)
  m <- sum(grepl("rbrack", tabnotes))
  expect_equal(m, 2)
  tab <- stable(stdata(), notes = notes, inspect = TRUE, mask_bracket = "none")
  tabnotes <- get_stable_data(tab)$notes
  m <- sum(grepl("lbrack", tabnotes))
  expect_equal(m, 0)
  m <- sum(grepl("rbrack", tabnotes))
  expect_equal(m, 0)
})
