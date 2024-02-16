library(testthat)
library(dplyr)

context("test-glossary")

glofile <- system.file("tex", "glossary.tex", package = "pmtables")

test_that("read a glossary file", {
  tex <- read_glossary(glofile)
  expect_named(tex)
  expect_is(tex, "tex_glossary")
  expect_is(tex, "glossary")
  expect_is(tex, "list")
  expect_true("WT" %in% names(tex))
  expect_true("QF" %in% names(tex))

  type_list <- vapply(tex, inherits, what = "list", FUN.VALUE = TRUE)
  expect_true(all(type_list))

  foo <- tempfile()
  expect_error(read_glossary(foo), "does not exist.")

  cat("a\n", file = foo)
  expect_error(read_glossary(foo), "No acronym entries were found")
})

test_that("load glossary notes", {
  notes <- glossary_notes(glofile, WT, NPDE)
  expect_is(notes, "character")
  expect_length(notes, 1)
  expect_true(grepl("WT: ", notes, fixed = TRUE))
  expect_true(grepl("NPDE: ", notes, fixed = TRUE))

  notes <- glossary_notes(glofile, WT, CLF, collapse = NULL)
  expect_is(notes, "character")
  expect_length(notes, 2)
  expect_true(any(grepl("WT", notes)))

  notes <- glossary_notes(glofile, WT, CLF, sep = "&  ")
  expect_is(notes, "character")
  expect_true(any(grepl("WT&  ", notes, fixed = TRUE)))

  notes <- glossary_notes(glofile, WT, CLF, collapse = NULL, labels = "QD,NPDE")
  expect_is(notes, "character")
  expect_length(notes, 4)
  expect_true(any(grepl("WT: ", notes, fixed = TRUE)))
  expect_true(any(grepl("CL/F", notes, fixed = TRUE)))
  expect_true(any(grepl("QD: ", notes, fixed = TRUE)))
  expect_true(any(grepl("NPDE: ", notes, fixed = TRUE)))
})

test_that("make glossary notes from glossary list", {
  x <- read_glossary(glofile)
  notes <- glossary_notes(x, WT, NPDE, CWRES, collapse = NULL)
  expect_length(notes, 3)
})

test_that("parse a glossary entry", {
  txt <- "\\newacronym{a}{b}{c} % comment"
  x <- pmtables:::parse_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_equivalent(x$a, list(abbreviation = "b", definition = "c"))

  txt <- "\\newacronym[options]{a}{b}{c} % comment"
  x <- pmtables:::parse_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_equivalent(x$a, list(abbreviation = "b", definition = "c"))

  txt <- "\\newacronym[options]{a}{b_\\mathrm{d}}{\\mathit{c}} % comment"
  x <- pmtables:::parse_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_equivalent(
    x$a, list(abbreviation = "b_\\mathrm{d}", definition = "\\mathit{c}")
  )

  txt <- "%\\newacronym{a}{b}{c}"
  expect_error(pmtables:::parse_glossary(txt), "No acronym entries")
})

test_that("corece list to glossary object", {
  g <- as_glossary(list(a = "b", c = "d"))
  expect_length(g, 2)
  expect_is(g, "glossary")

  expect_error(as_glossary(list("a", c = "d")), "must be a named list")
  expect_error(as_glossary(c(a = "b", c = "d")), "must be a named list")
})
