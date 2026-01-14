library(testthat)
library(pmtables)
library(dplyr)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-sanitize")

test_that("tab-escape [PMT-TEST-0177]", {
  x <- c("a", "a_b", "a $\\Sigma$ b")
  ans <- pmtables:::tab_escape(x)
  expect_equal(ans, c(x[1], "a\\_b", x[3]))
})

test_that("col names are sanitized [PMT-TEST-0178]", {
  data <- tibble(A = 1, "B %" = 2, "C_3" = 3)
  x <- inspect(data)
  expect_equal(x$cols_tex, "A & B \\% & C\\_3 \\\\")
})

test_that("units are sanitized [PMT-TEST-0179]", {
  data <- tibble(A = 1, "B %" = 2, "C_3" = 3)
  x <- inspect(data, units = list(C_3  = "$\\mu$g"))
  expect_match(x$cols_tex[1], " &  & C\\_3", fixed = TRUE)
  expect_match(x$cols_tex[2], "A & B \\% & $\\mu$g", fixed = TRUE)
})

test_that("files are sanitized [PMT-TEST-0180]", {
  data <- tibble(A = 1, "B %" = 2, "C_3" = 3)
  x <- inspect(data, r_file = "foo_bar.R", output_file = "foo\\_bar.tex")
  expect_match(x$notes[1], "foo\\_bar.R", fixed  = TRUE)
  expect_match(x$notes[2], "foo\\_bar.tex", fixed  = TRUE)
})

test_that("notes are sanitized [PMT-TEST-0181]", {
  data <- tibble(A = 1, "B %" = 2, "C_3" = 3)
  notes <- c("foo_bar note", "foo_bar %", "$\\mu$ %")
  x <- inspect(data, notes = notes)
  expect_match(x$notes[1], "foo\\_bar note", fixed  = TRUE)
  expect_match(x$notes[2], "foo\\_bar \\%", fixed  = TRUE)
  expect_match(x$notes[3], notes[3], fixed  = TRUE)
})

test_that("span titles are sanitized [PMT-TEST-0182]", {
  data <- ptdata()
  sp <- list(
    colgroup("Percent_true (%)", CRCL:AGE),
    colgroup("Prcnt (\\%)", FORM:N, level = 2)
  )
  x <- inspect(data, span = sp)
  ans <- x$span_data$tex
  expect_match(ans[1], "Prcnt (\\%)", fixed = TRUE)
  expect_match(ans[3], "Percent\\_true (\\%)", fixed  = TRUE)
})

test_that("table contents are sanitized [PMT-TEST-0183]", {
  data <- as.data.frame(matrix(letters[1:16], ncol = 4), stringsAsFactors=FALSE)
  data[2,3] <- "foo_bar"
  data[4,3] <- "percent (%)"
  data[2,2] <- "$\\mug$"
  data[1,1] <- "foo\\_bar [%]"
  out <- inspect(data = data, sub_bracket = "none")$tab
  expect_match(out[2], "foo\\_bar", fixed = TRUE, all = FALSE)
  expect_match(out[4], "\\%", fixed = TRUE, all = FALSE)
  expect_match(out[2],"$\\mug$", fixed = TRUE, all = FALSE)
  expect_match(out[1], "foo\\_bar", fixed = TRUE, all = FALSE)
  expect_match(out[1], "[%]", fixed = TRUE, all = FALSE)
})

test_that("brackets are sanitized via sub_bracket", {
  data <- data.frame(A = "[2,3]", B = "(9,10]", C = "[[22,33)")

  ans <- tab_prime(data)
  expect_equal(ans[1,1], "\\lbrack{}2,3\\rbrack{}")
  expect_equal(ans[1,2], "(9,10\\rbrack{}")
  expect_equal(ans[1,3], "\\lbrack{}\\lbrack{}22,33)")

  ans <- tab_prime(data, sub_bracket = "left")
  expect_equal(ans[1,1], "\\lbrack{}2,3]")
  expect_equal(ans[1,2], "(9,10]")
  expect_equal(ans[1,3], "\\lbrack{}\\lbrack{}22,33)")

  ans <- tab_prime(data, sub_bracket = "right")
  expect_equal(ans[1,1], "[2,3\\rbrack{}")
  expect_equal(ans[1,2], "(9,10\\rbrack{}")
  expect_equal(ans[1,3], "[[22,33)")

  ans <- tab_prime(data, sub_bracket = "none")
  expect_equal(ans[1,1], "[2,3]")
  expect_equal(ans[1,2], "(9,10]")
  expect_equal(ans[1,3], "[[22,33)")

  a <- c("[1,2]", "(22,55]", "2 to 4", "[[22,33))", "(11,99]]", "(5,6)")
  data <- data.frame(A = a, b = 2, c = seq(length(a)))

  ans <- tab_prime(data)
  expect_equal(ans[1,1], "\\lbrack{}1,2\\rbrack{}")
  expect_equal(ans[2,1], "(22,55\\rbrack{}")
  expect_equal(ans[3,1], "2 to 4")
  expect_equal(ans[4,1], "\\lbrack{}\\lbrack{}22,33))")
  expect_equal(ans[5,1], "(11,99\\rbrack{}\\rbrack{}")
  expect_equal(ans[6,1], "(5,6)")
  expect_true(all(ans[,2]=="2"))
  expect_identical(ans[,3],as.character(seq(nrow(ans))))

  data <- data.frame(A = a)
  ans1 <- tab_prime(data)
  ans2 <- make_tabular(data)
  ans2 <- sub(" \\\\\\\\$", "", ans2)
  expect_identical(ans1$A, ans2)

  tab1 <- stable(data)
  expect_equal(sum(grepl("lbrack", tab1)), 2)
  expect_equal(sum(grepl("rbrack", tab1)), 3)

  tab2 <- stable(data, sub_bracket = "left")
  expect_equal(sum(grepl("lbrack", tab2)), 2)
  expect_equal(sum(grepl("rbrack", tab2)), 0)

  tab3 <- stable(data, sub_bracket = "right")
  expect_equal(sum(grepl("lbrack", tab3)), 0)
  expect_equal(sum(grepl("rbrack", tab3)), 3)

  tab4 <- stable(data, sub_bracket = "none")
  expect_equal(sum(grepl("lbrack", tab4)), 0)
  expect_equal(sum(grepl("rbrack", tab4)), 0)

  expect_error(
    stable(data, sub_bracket = "frb"),
    "should be one of"
  )

  tab1 <- stable_long(data)
  expect_equal(sum(grepl("lbrack", tab1)), 2)
  expect_equal(sum(grepl("rbrack", tab1)), 3)

  tab2 <- stable_long(data, sub_bracket = "left")
  expect_equal(sum(grepl("lbrack", tab2)), 2)
  expect_equal(sum(grepl("rbrack", tab2)), 0)

  tab3 <- stable_long(data, sub_bracket = "right")
  expect_equal(sum(grepl("lbrack", tab3)), 0)
  expect_equal(sum(grepl("rbrack", tab3)), 3)

  tab4 <- stable_long(data, sub_bracket = "none")
  expect_equal(sum(grepl("lbrack", tab4)), 0)
  expect_equal(sum(grepl("rbrack", tab4)), 0)

  expect_error(
    stable_long(data, sub_bracket = "frb"),
    "should be one of"
  )
})
