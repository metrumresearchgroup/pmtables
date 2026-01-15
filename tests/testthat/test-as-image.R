library(testthat)
library(pmtables)

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
  expect_match(x, "\\usepackage{helvet}", all = FALSE, fixed = TRUE)
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
  expect_match(x, "\\usepackage[adobe-utopia]{mathdesign}", all = FALSE, fixed = TRUE)

  x <- do(st_as_image, tab, stem = "test-roboto", font = "roboto")
  expect_match(x, "\\usepackage[sfdefault]{roboto}", all = FALSE, fixed = TRUE)
})

test_that("stable_save_image renders and saves with stem", {
  where <- file.path(tempdir(), "test-as-image")
  unlink(where, recursive = TRUE)
  dir.create(where)
  tab <- stable(stdata())
  ans <- stable_save_image(tab, stem = "test-as-image-1", dir = where)
  expect_true(file.exists(ans))
  expect_equal(basename(ans), "test-as-image-1.png")

  ans <- stable_save_image(tab, stem = "test-as-image-2", dir = where, format = "pdf")
  expect_true(file.exists(ans))
  expect_equal(basename(ans), "test-as-image-2.pdf")
})

test_that("stable_save_image renders and saves with file", {
  where <- file.path(tempdir(), "test-as-image")
  unlink(where, recursive = TRUE)
  dir.create(where)
  tab <- stable(stdata())
  ans <- stable_save_image(tab, file = "test-as-image-1.foo", dir = where)
  expect_true(file.exists(ans))
  expect_equal(basename(ans), "test-as-image-1.foo")
})

test_that("stable_save_image renders and saves with file as full path", {
  where <- file.path(tempdir(), "test-as-image")
  unlink(where, recursive = TRUE)
  dir.create(where)
  tab <- stable(stdata())
  file <- file.path(where, "test-as-image-1.foo")
  ans <- stable_save_image(tab, file = file)
  expect_true(file.exists(ans))
  expect_equal(basename(ans), "test-as-image-1.foo")

  expect_warning(
    stable_save_image(tab, file = file, dir = tempdir()),
    "overriding `dir` argument with path information found in `file`"
  )
})

test_that("stable_save_image changes .tex to .<output-format>", {
  where <- file.path(tempdir(), "test-as-image")
  unlink(where, recursive = TRUE)
  dir.create(where)
  tab <- stable(stdata(), output_file = "test-as-image-3.tex")
  ans <- stable_save_image(tab, dir = where)
  expect_true(file.exists(ans))
  expect_equal(basename(ans), "test-as-image-3.png")
})

test_that("stable_save_image requires stable object", {
  tab <- unclass(stable(stdata()))
  expect_error(stable_save_image(tab), "is not an 'stable' object")
})

test_that("stable_save_image takes a long table", {
  tab <- stable_long(stdata())
  expect_silent(stable_save_image(tab, stem = "foo2", dir = tempdir()))
})


test_that("render a table with unmatched brackets", {
  data <- data.frame(a = c("[123, 456)", "[9,10)"))
  tab <- stable(st_new(data))

  expect_equal(sum(grepl("lbrack", tab)), 2)
  expect_equal(sum(grepl("rbrack", tab)), 0)

  where <- file.path(tempdir(), "test-unmatch-bracket")
  unlink(where, recursive = TRUE)
  dir.create(where)

  ans <- stable_save_image(tab, stem = "test-unmatch-bracket", dir = where)
  expect_true(file.exists(ans))
  expect_equal(basename(ans), "test-unmatch-bracket.png")

})
