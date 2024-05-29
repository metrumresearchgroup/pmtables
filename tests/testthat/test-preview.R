
library(testthat)
library(pmtables)

context("test-preview.R")

test_that("wrap stable output in landscape [PMT-TEST-0169]", {
  out <- stable(stdata())
  out.ls <- as_lscape(out)
  expect_true(is_lscape(out.ls))
  tex <- pt_wrap(out.ls, con = NULL)
  expect_match(tex[1], "=latex", fixed = TRUE)
  expect_match(tex[2], "begin{landscape}", fixed = TRUE)
})

test_that("use latex dependencies for knit [PMT-TEST-0170]", {
  expect_identical(st_using_knit_deps(), FALSE)
  st_use_knit_deps()
  expect_identical(st_using_knit_deps(), FALSE)
  st_use_knit_deps(force = TRUE)
  expect_identical(st_using_knit_deps(), TRUE)
  meta <- knitr::knit_meta()
  deps <- purrr::map_chr(meta, "name")
  test <- st_knit_deps()
  expect_true(all(test %in% deps))
  pmtables:::st_reset_knit_deps()
})

test_that("st-wrap table placement H [PMT-TEST-0171]", {
  tab <- stable(stdata())
  out1 <- st_wrap(tab, con = NULL, float = "!ht")
  expect_match(out1[2], "{table}[!ht]", fixed = TRUE)
  st_use_knit_deps(force = TRUE)
  out2 <- st_wrap(tab, con = NULL)
  expect_match(out2[2], "{table}[H]", fixed = TRUE)
  pmtables:::st_reset_knit_deps()
})

test_that("error to try to view long table [PMT-TEST-0172]", {
  expect_error(st2viewer(stable_long(stdata)))
})

test_that("pass a list of tables to st2report [PMT-TEST-0173]", {
  tab <- stable(stdata())
  tabs <- list(tab,tab,tab)
  ans1 <- st2report(tabs, dry_run = TRUE)
  expect_is(ans1, "list")
  expect_length(ans1, 3)
  ans2 <- st2report(tab,tab,tab, dry_run = TRUE)
  expect_is(ans2, "list")
  expect_length(ans2, 3)
})

test_that("call st_asis on a pmtable object [PMT-TEST-0174]", {
  x <- pt_cont_long(pmt_first, cols = "WT")
  ans <- st_asis(x)
  expect_is(ans, "knit_asis")
})

test_that("error to call st_asis on non-stable object [PMT-TEST-0175]", {
  expect_error(
    st_asis(stdata()),
    msg = "x does not inherit from class stable"
  )
})

test_that("st2report - list names are escaped [PMT-TEST-0176]", {
  l <- list(a = stable(stdata()), `a_b` = stable(stdata()))
  a <- st2report(l, dry_run = TRUE)
  expect_equal(names(a), c("a", "a\\_b"))
})

test_that("st2report respects pmtables.dir option", {
  tab <- stable(stdata())
  dir <- tempfile()
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  options(pmtables.dir = dir)
  x <- st2report(tab, show_pdf = FALSE, stem = "st2report-test-pmtables-dir")
  expect_true(file.exists(file.path(dir, "st2report-test-pmtables-dir.pdf")))
  options(pmtables.dir = NULL)
})
