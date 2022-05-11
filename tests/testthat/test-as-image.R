
do <- function(fn, ...) {
  output_dir <- tempfile("pmtables-test-")
  dir.create(output_dir)
  on.exit(unlink(output_dir), add = TRUE)
  old_dir <- setwd(output_dir)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)

  args <- list(..., dir = output_dir)
  do.call(fn, args)

  return(readLines("standalone-preview.tex"))
}

test_that("standalone tex file looks as expected: stable", {
  tab <- stable(stdata())
  x <- do(st_as_image, tab, stem = "convert-stable")
  expect_match(x, "convert-stable.tex", all = FALSE, fixed = TRUE)
  expect_match(x, "helvet", all = FALSE, fixed = TRUE)
  expect_match(x, "\\textwidth}{6.5in}", all = FALSE, fixed = TRUE)
  expect_match(x, "varwidth=6.5in", all = FALSE, fixed = TRUE)
  expect_match(x, "\\usepackage{longtable}[=v4.13]", all = FALSE, fixed = TRUE)
  expect_match(x, "\\rule{6.5in}{0pt}", all = FALSE, fixed = TRUE)
  expect_match(x, "border={0.2cm 0.7cm}", all = FALSE, fixed = TRUE)
})

test_that("standalone tex file looks as expected: long stable", {
  longtab <- stable_long(stdata())
  x <- do(st_as_image, longtab, stem = "convert-longtable", ntex = 2)
  expect_match(x, "\\input{convert-longtable.tex}", all = FALSE, fixed = TRUE)
})

test_that("standalone tex file looks as expected: pmtable", {
  pttab <- pt_cat_long(pmt_first, span = "STUDYf", cols = "SEXf")
  x <- do(st_as_image, pttab, stem = "convert-pttab")
  expect_match(x, "\\input{convert-pttab.tex}", all = FALSE, fixed = TRUE)
})

test_that("standalone tex file looks as expected: stobject", {
  sttab <- st_new(stdata())
  x <- do(st_as_image, sttab, stem = "convert-sttab")
  expect_match(x, "\\input{convert-sttab.tex}", all = FALSE, fixed = TRUE)
})

test_that("standalone tex file looks as expected: png", {
  tab <- stable(stdata())
  x <- do(st_aspng, tab, stem = "include-from-png")
  expect_match(x, "\\input{include-from-png.tex}", all = FALSE, fixed = TRUE)
})

test_that("standalone tex file looks as expected: smaller page", {
  tab <- stable(stdata())
  x <- do(st_as_image, tab, stem = "smaller-page", textwidth = 4)
  expect_match(x, "\\textwidth}{4in}", all = FALSE, fixed = TRUE)
  expect_match(x, "varwidth=4in", all = FALSE, fixed = TRUE)
  expect_match(x, "\\rule{4in}{0pt}", all = FALSE, fixed = TRUE)
  expect_match(x, "\\input{smaller-page.tex}", all = FALSE, fixed = TRUE)
})

test_that("standalone tex file looks as expected: smaller width", {
  tab <- stable(stdata())
  x <- do(st_as_image, tab, stem = "smaller-width", width = 0.5)
  expect_match(x, "\\textwidth}{6.5in}", all = FALSE, fixed = TRUE)
  expect_match(x, "varwidth=6.5in", all = FALSE, fixed = TRUE)
  expect_match(x, "\\rule{6.5in}{0pt}", all = FALSE, fixed = TRUE)
  expect_match(x, "\\input{smaller-width.tex}", all = FALSE, fixed = TRUE)
})

test_that("standalone tex file looks as expected: bigger border", {
  tab <- stable(stdata())
  x <- do(st_as_image, tab, stem = "bigger-border", border = "0.2cm 3cm")
  expect_match(x, "border={0.2cm 3cm}", all = FALSE, fixed = TRUE)
  expect_match(x, "\\input{bigger-border.tex}", all = FALSE, fixed = TRUE)
})

test_that("standalone tex file looks as expected: change font", {
  tab <- stable(stdata())

  x <- do(st_as_image, tab, stem = "test-utopia", font = "utopia")
  expect_match(x, "utopia", all = FALSE, fixed = TRUE)

  x <- do(st_as_image, tab, stem = "test-roboto", font = "roboto")
  expect_match(x, "roboto", all = FALSE, fixed = TRUE)
})
