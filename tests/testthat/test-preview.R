
library(testthat)
library(pmtables)

context("test-preview.R")

test_that("wrap stable output in landscape", {
  out <- stable(stdata())
  out.ls <- as_lscape(out)
  expect_true(is_lscape(out.ls))
  tex <- pt_wrap(out.ls, con = NULL)
  expect_match(tex[1], "=latex", fixed = TRUE)
  expect_match(tex[2], "begin{landscape}", fixed = TRUE)
})

test_that("use latex dependencies for knit", {
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


test_that("st-wrap table placement H", {
  tab <- stable(stdata())
  out1 <- st_wrap(tab, con = NULL, float = "!ht")
  expect_match(out1[2], "{table}[!ht]", fixed = TRUE)
  st_use_knit_deps(force = TRUE)
  out2 <- st_wrap(tab, con = NULL)
  expect_match(out2[2], "{table}[H]", fixed = TRUE)
  pmtables:::st_reset_knit_deps()
})
