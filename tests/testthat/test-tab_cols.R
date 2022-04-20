library(testthat)
library(pmtables)
library(dplyr)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-tab_cols")

# issue-62
test_that("underscore doesn't get escaped in rename [PMT-TEST-0200]", {
  cols <- c("A", "B", "C_f", "D")
  renam <- c(X = "C_f")
  out <- tab_cols(cols, cols_rename = renam)
  expect_identical(out$new[3], "X")
})

test_that("cols are not renamed when no match [PMT-TEST-0201]", {
  cols <- letters[1:5]
  re_label <- LETTERS[1:5]
  out <- pmtables:::rename_cols(cols, re_label)
  expect_identical(out, cols)
  re_label <- c("A" = "AA", "B" = "BB")
  out <- pmtables:::rename_cols(cols, re_label)
  expect_identical(out, cols)
})

test_that("cols are renamed [PMT-TEST-0202]", {
  cols <- letters[1:5]
  re_label <- c("A" = "a", "DD" = "d")
  out <- pmtables:::rename_cols(cols, re_label)
  expect_identical(out, c("A", "b", "c", "DD", "e"))
})

test_that("cols are replaced [PMT-TEST-0203]", {
  data <- data.frame(A = 1, B = 2, C = 3)
  out <- inspect(data, cols_replace = c("X", "Y", "Z"))
  expect_equal(out$cols_new, c("X", "Y", "Z"))
  expect_error(stable(data, cols_replace = c("X", "Y")))
})

test_that("cols are bold [PMT-TEST-0204]", {
  data <- tibble(a = 1, b = 2, c = 3)
  x <- inspect(data, cols_bold = TRUE)
  expect_true(grepl("textbf", x$cols_tex))
  x <- inspect(data)
  expect_false(grepl("textbf", x$cols_tex))
})

test_that("cols are bold after sanitizing [PMT-TEST-0205]", {
  data <- tibble(a_z = 1, b = 2, c = 3)
  x <- inspect(data, cols_bold = TRUE)
  cols <- str_split(x$cols_tex, " *& *")
  expect_equal(cols[[1]][1], "\\textbf{a\\_z}")
})

test_that("units [PMT-TEST-0206]", {
  u <- list(b = "in",kyle = "baron", dd = 5, a = "mg")
  data <- tibble(a = 1, b = 2, c = 3)
  x <- inspect(data,units = u)
  expect_match(x$cols_tex[2], "mg & in & c")
  expect_warning(
    inspect(data, units = list(foo = "bar")),
    "no valid units"
  )
})

test_that("col title breaks [PMT-TEST-0207]", {
  data <- tibble(a = 1, b = 2, c = 3)
  x <- inspect(
    data,
    cols_rename  =  c("Metrum RG" = "b")
  )
  expect_length(x$cols_tex,1)
  expect_match(x$cols_tex, "a & Metrum RG", fixed = TRUE)
  x <- inspect(
    data,
    cols_rename  =  c("Metrum  ... RG...CT" = "b"),
    units = list(a = "A", b  = "B")
  )
  expect_length(x$cols_tex,4)
  expect_match(x$cols_tex[1], " & Metrum & ")
  expect_match(x$cols_tex[2], " & RG & ")
  expect_match(x$cols_tex[3], "a & CT & ")
  expect_match(x$cols_tex[4], "A & B & c")
})

test_that("column is dropped [PMT-TEST-0208]", {
  data <- tibble(a = 1, b = 2, z = 25, c = 3)
  x <- inspect(data, drop = "z")
  expect_identical(x$cols, c("a", "b", "c"))
})

test_that("de-tag column labels [PMT-TEST-0209]", {
  data <- tibble(x.a = 1, x.b = 2, y.a = 25, z.a = 3)
  x <- inspect(data, cols_split  = '.')
  expect_identical(x$cols_new, c("a", "b", "a", "a"))
})

test_that("tab-cols cols_extra [PMT-TEST-0210]", {
  x <- letters[1:10]
  data <- tibble(a = x, b = x, c = x)
  xtra <- data[1,]
  xtra$b <- "10%"
  ans <- inspect(data, cols_extra  = xtra)
  expect_length(ans$cols_tex, 2)
  expect_match(ans$cols_tex[2], "10\\%", fixed = TRUE)
  ans <- inspect(data)
  expect_length(ans$cols_tex, 1)
  expect_error(
    inspect(data, cols_extra = xtra[,1:2]),
    msg = "ncol(extra) not equal to length(cols)"
  )
})

test_that("cols_omit drops column names - stable [PMT-TEST-0211]", {
  a <- stable(stdata(), cols_omit = FALSE)
  b <- stable(stdata(), cols_omit = TRUE)
  expect_equal(length(a) - length(b), 2)
  expect_match(a[1:7], "STUDY &", all = FALSE, fixed = TRUE)
  expect_match(a[1:7], " & CRCL &", all = FALSE, fixed = TRUE)
  expect_false(any(grepl("STUDY", b)))
  expect_false(any(grepl("CRCL", b)))
})

test_that("cols_omit drops column names - longtable [PMT-TEST-0212]", {
  a <- stable_long(stdata(), cols_omit = FALSE)
  b <- stable_long(stdata(), cols_omit = TRUE)
  expect_match(a[1:20], "STUDY &", all = FALSE, fixed = TRUE)
  expect_match(a[1:20], " & CRCL &", all = FALSE, fixed = TRUE)
  expect_false(any(grepl("STUDY", b)))
  expect_false(any(grepl("CRCL", b)))
})

test_that("cols_omit drops units [PMT-TEST-0213]", {
  u <- list(WT = "kg")
  a <- stable(stdata(), units = u)
  b <- stable(stdata(), cols_omit = TRUE, units = u)
  expect_match(a, "kg", all = FALSE, fixed = TRUE)
  expect_false(any(grepl("kg", b)))
})

test_that("cols_omit keeps span data [PMT-TEST-0214]", {
  sp <- as.span("title", WT:SCR)
  a <- stable(stdata(), span = sp)
  b <- stable(stdata(), cols_omit = TRUE,  span = sp)
  expect_match(a, "title", all = FALSE, fixed = TRUE)
  expect_match(b, "title", all = FALSE, fixed = TRUE)
})
