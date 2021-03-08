library(testthat)
library(pmtables)

inspect_long <- function(...) {
  get_stable_data(stable_long(..., inspect = TRUE))
}

context("test-longtable")

test_that("test-longtable stable_long", {
  notes <- letters[1:3]
  out <- inspect_long(data = mtcars, notes = notes)
  expect_match(out$output, "\\begin{longtable}", fixed = TRUE, all=FALSE)
})

test_that("longtable - caption text", {
  out <- inspect_long(data = mtcars, lt_cap_text = "Text caption")
  test <- "\\caption{Text caption}"
  expect_match(out$output, test, fixed = TRUE, all=FALSE)
})

test_that("longtable - caption with short", {
  out <- inspect_long(
    data = mtcars,
    lt_cap_text = "Text caption",
    lt_cap_short = "Short text"
  )
  test <- "\\caption[Short text]{Text caption}"
  expect_match(out$output, test, fixed = TRUE, all=FALSE)
})

test_that("longtable - caption macro", {
  out <- inspect_long(data = mtcars, lt_cap_macro = "mylongtable")
  test <- "\\caption{\\mylongtable}"
  expect_match(out$output, test, fixed = TRUE, all = FALSE)
  out <- inspect_long(data = mtcars, lt_cap_macro = "\\mylongtable")
  expect_match(out$output, test, fixed = TRUE, all = FALSE)
  expect_error(stable_long(data = mtcars, lt_cap_macro = "foo123",
                           "invalid for use in latex"))
})

test_that("longtable - row spacing is set", {
  out <- inspect_long(data = mtcars, sizes = tab_size(lt_row = 0.15))
  expect_match(out$tab,"extrarowheight}{0.15em}", all = FALSE, fixed  = TRUE)
  out <- inspect_long(data = mtcars, sizes = tab_size(lt_row = -0.11, row = 1.3))
  expect_match(out$tab,"extrarowheight}{-0.11em}", all = FALSE, fixed  = TRUE)
  expect_match(out$tab,"renewcommand{\\arraystretch}{1.3}", all = FALSE, fixed  = TRUE)
})

test_that("longtable - output file is saved", {
  out <- stable_long(ptdata(), output_file = "../path/foo.tex")
  expect_equal(attributes(out)$stable_file, "../path/foo.tex")
})

test_that("longtable - with span", {
  data <- stdata()
  span <- colgroup("a group of cols", 4:9)
  out <- inspect_long(data, span = span)
  expect_match(
    out$output,
    "multicolumn{6}{c}{a group of cols}",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("longtable - with units", {
  data <- stdata()
  units <- list(WT = "(kg)", AGE = "(years)")
  out <- inspect_long(data, units = units )
  expect_match(
    out$output,
    "(kg)",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    out$output,
    "(years)",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("longtable - with span and units", {
  data <- stdata()
  units <- list(WT = "(kg)", AGE = "(years)")
  span <- colgroup("a group of cols", 4:9)
  out <- inspect_long(data, units = units, span = span )
  expect_match(
    out$output,
    "(kg)",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    out$output,
    "multicolumn{6}{c}{a group of cols}",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("render long table from pmtable", {
  a <- stable_long(pt_cont_long(pmt_first, cols = "WT,ALB,SCR"))
  expect_is(a, "stable_long")
  expect_is(a, "stable")
})

test_that("render long table from stobject", {
  a <- st_new(stdata()) %>% stable_long()
  expect_is(a, "stable_long")
  expect_is(a, "stable")
})

test_that("set font size in longtable", {
  a <- stable_long(stdata(), inspect = TRUE)
  expect_match(a[1], "\\arraystretch", fixed = TRUE)
  sz <- tab_size(font = "scriptsize")
  b <- stable_long(stdata(), sizes = sz, inspect = TRUE)
  expect_match(b[1], "\\scriptsize", fixed = TRUE)
})
