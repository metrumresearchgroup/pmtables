library(testthat)
library(dplyr)

context("test-glossary")

glofile <- system.file("tex", "glossary.tex", package = "pmtables")

test_that("read a glossary file", {
  tex <- read_glossary(glofile)
  expect_named(tex)
  expect_is(tex, "list")
  expect_true("WT" %in% names(tex))
  expect_true("QF" %in% names(tex))

  foo <- tempfile()
  expect_error(read_glossary(foo), "does not exist.")

  cat("a\n", file = foo)
  expect_error(read_glossary(foo), "no acronym entries were found")
})

test_that("load glossary notes", {
  notes <- glossary_notes(glofile, WT, CLF)
  expect_is(notes, "character")
  expect_length(notes, 1)
  expect_true(grepl("WT: ", notes, fixed = TRUE))
  expect_true(grepl("CLF: ", notes, fixed = TRUE))

  notes <- glossary_notes(glofile, WT, CLF, collapse = NULL)
  expect_is(notes, "character")
  expect_length(notes, 2)
  expect_true(any(grepl("WT", notes)))

  notes <- glossary_notes(glofile, WT, CLF, sep = "&  ")
  expect_is(notes, "character")
  expect_true(any(grepl("WT&  ", notes, fixed = TRUE)))

  notes <- glossary_notes(glofile, WT, CLF, collapse = NULL, labels = "VF,NPDE")
  expect_is(notes, "character")
  expect_length(notes, 4)
  expect_true(any(grepl("WT: ", notes, fixed = TRUE)))
  expect_true(any(grepl("CLF: ", notes, fixed = TRUE)))
  expect_true(any(grepl("VF: ", notes, fixed = TRUE)))
  expect_true(any(grepl("NPDE: ", notes, fixed = TRUE)))
})

test_that("parse a glossary entry", {
  txt <- "\\newacronym{a}{b}{c} % comment"
  x <- pmtables:::parse_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_identical(x$a, "c")

  txt <- "\\newacronym[options]{a}{b}{c} % comment"
  x <- pmtables:::parse_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_identical(x$a, "c")

  txt <- "\\newacronym[options]{a}{b_\\mathrm{d}}{\\mathit{c}} % comment"
  x <- pmtables:::parse_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_identical(x$a, "\\mathit{c}")

  txt <- "%\\newacronym{a}{b}{c}"
  expect_error(pmtables:::parse_glossary(txt), "no acronym entries")
})
