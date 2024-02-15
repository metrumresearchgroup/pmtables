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

  notes <- glossary_notes(glofile, WT, CLF, collapse = NULL, labels = "VF")
  expect_is(notes, "character")
  expect_length(notes, 3)
  expect_true(any(grepl("WT: ", notes, fixed = TRUE)))
  expect_true(any(grepl("CLF: ", notes, fixed = TRUE)))
  expect_true(any(grepl("VF: ", notes, fixed = TRUE)))
})
