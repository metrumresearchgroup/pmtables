library(testthat)
library(dplyr)

context("test-glossary")

glofile <- system.file("glo", "glossary.tex", package = "pmtables")
gloyaml <- system.file("glo", "glossary.yaml", package = "pmtables")
gloyml <- system.file("glo", "glossary.yml", package = "pmtables")


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

  foo <- tempfile(fileext = ".tex")
  expect_error(read_glossary(foo), "does not exist.")

  cat("a\n", file = foo)
  expect_error(read_glossary(foo), "No acronym entries were found")
})

test_that("read a glossary file - yaml", {
  yml <- read_glossary(gloyml)
  expect_named(yml)
  expect_is(yml, "tex_glossary")
  expect_is(yml, "glossary")
  expect_is(yml, "list")
  expect_true("wt" %in% names(yml))
  expect_true("cmin" %in% names(yml))

  type_list <- vapply(yml, inherits, what = "list", FUN.VALUE = TRUE)
  expect_true(all(type_list))

  globad <- system.file("glo", "glossary-bad.yml", package = "pmtables")

  expect_error(
    suppressMessages(read_glossary(globad)),
    "Failed to parse glossary file"
  )
})

test_that("read a glossary file - pass format", {
  glof <- system.file("glo", "glossary-yml", package = "pmtables")
  expect_error(
    read_glossary(glof),
    "Could not guess"
  )
  yml <- read_glossary(glof, format = "yaml")
  expect_is(yml, "tex_glossary")
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

test_that("load glossary notes - yaml", {
  notes <- glossary_notes(gloyaml, wt, auc)
  expect_is(notes, "character")
  expect_length(notes, 1)
  expect_true(grepl("WT: ", notes, fixed = TRUE))
  expect_true(grepl("AUC: ", notes, fixed = TRUE))
})

test_that("glossary notes - pass format", {
  glof <- system.file("glo", "glossary-yml", package = "pmtables")
  notes <- glossary_notes(glof, wt, auc, format = "yaml")
  expect_is(notes, "character")
  expect_true(grepl("AUC: ", notes, fixed = TRUE))
})

test_that("make glossary notes from glossary list", {
  x <- read_glossary(glofile)
  notes <- glossary_notes(x, WT, NPDE, CWRES, collapse = NULL)
  expect_length(notes, 3)

  expect_error(
    glossary_notes(x, MetrumRG),
    "Can't subset columns that"
  )
})

test_that("parse a glossary entry", {
  txt <- "\\newacronym{a}{b}{c} % comment"
  x <- pmtables:::parse_tex_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_equivalent(x$a, list(abbreviation = "b", definition = "c"))

  txt <- "\\newacronym[options]{a}{b}{c} % comment"
  x <- pmtables:::parse_tex_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_equivalent(x$a, list(abbreviation = "b", definition = "c"))

  txt <- "\\newacronym[options]{a}{b_\\mathrm{d}}{\\mathit{c}} % comment"
  x <- pmtables:::parse_tex_glossary(txt)
  expect_length(x, 1)
  expect_named(x)
  expect_identical(names(x), "a")
  expect_equivalent(
    x$a, list(abbreviation = "b_\\mathrm{d}", definition = "\\mathit{c}")
  )

  txt <- "%\\newacronym{a}{b}{c}"
  expect_error(pmtables:::parse_tex_glossary(txt), "No acronym entries")
})

test_that("corece list to glossary object", {
  g <- as_glossary(list(a = "b", c = "d"))
  expect_length(g, 2)
  expect_is(g, "glossary")

  expect_error(as_glossary(list("a", c = "d")), "must be a named list")
  expect_error(as_glossary(c(a = "b", c = "d")), "must be a named list")

  g <- as_glossary(list(a = "b", c = "d"), c = "dd")
  expect_is(g, "glossary")
})

test_that("require glossary", {
  expect_error(
    pmtables:::require_glossary(letters),
    "is not a glossary object"
  )
})

test_that("update an abbreviation", {
  glo <- read_glossary(glofile)
  expect_equal(glo$WT$abbreviation, "WT")

  glo2 <- update_abbrev(glo, WT = "WGT")
  expect_equal(glo2$WT$abbreviation, "WGT")

  glo3 <- update_abbrev(glo2, WT)
  expect_equal(glo3$WT$abbreviation, "WT")

  expect_error(
    update_abbrev(glo2, FOO),
    "Requested definitions not found"
  )
})
