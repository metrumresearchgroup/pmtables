library(testthat)
library(pmtables)
library(dplyr)
library(stringr)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-panel")

test_that("panel duplicates [PMT-TEST-0158]", {
  data <- data.frame(A = c(1,1,2,2,1,1), B = 3)
  expect_error(
    stable(data, panel = as.panel("A")),
    msg = "or set duplicates_ok to TRUE"
  )
  out <- inspect(data, panel = as.panel("A", duplicates_ok = TRUE))
  expect_equal(ncol(out$data),1L)
})

test_that("can't panel with one column [PMT-TEST-0159]", {
  expect_error(
    stable(data.frame(A = 1), panel = as.panel("A")),
    "must have more than one column"
  )
})

test_that("span split with title [PMT-TEST-0160]", {
  data <- data.frame(a.A = 1, a.B = 2, C = 3, z.Y = 4, z.Z = 5)
  title <- list(a = "First Split", z = "Last Split")
  out <- inspect(data, span_split = colsplit(sep = '.', title = title))
  expect_equal(out$cols_new, c("A", "B", "C", "Y", "Z"))
  utitle <- unique(out$span_data$span[[1]]$title)
  expect_equal(utitle, c("First Split", "", "Last Split"))
})

test_that("panel with sumrow [PMT-TEST-0161]", {
  data <- ptdata()
  data$STUDY[6] <- "Summary"
  data$STUDY[13] <- "Summary"
  out <- inspect(
    data,
    panel = "STUDY",
    sumrows = sumrow(data$STUDY=="Summary",label="All")
  )
  tab <- out$tab
  multi <- grepl("multicolumn",tab)
  expect_equal(sum(multi),2)
  all <- grepl("^All", tab)
  expect_equal(sum(all),2)
})

test_that("panel with drop [PMT-TEST-0162]", {
  data <- ptdata()
  out <- inspect(data, panel = "STUDY", drop  = "N,WT,DOSE")
  n <- ncol(data) - 3 - 1
  expect_equal(n, out$nc)
  expect_equal(n, length(out$cols_new))
  expect <- paste0("\\multicolumn{",n,"}")
  ans <- sum(stringr::str_count(out$tab, stringr::fixed(expect)))
  expect_equal(ans,length(unique(data[["STUDY"]])))
})

test_that("panel invalid regex in panel_skip [PMT-TEST-0163]", {
  x <- "\\textbf{c}"
  data <- data.frame(A = c("a", "b", x), B = "B", C = "C")
  out <- inspect(data, panel  = as.panel("A",skip  = x))
  expect_equal(sum(stringr::str_count(out$tab, "multicolumn")),2)

  out <- inspect(data, panel  = as.panel("A", prefix_skip  = x, prefix = "FOO"))
  expect_match(out$tab, "\\textbf{FOO a}", all = FALSE, fixed = TRUE)
  expect_match(out$tab, "\\textbf{FOO b}", all = FALSE, fixed = TRUE)
  expect_match(out$tab, "\\textbf{c}", all = FALSE, fixed = TRUE)
})

test_that("omit hline from panel [PMT-TEST-0164]", {
  data <- stdata()
  tab1 <- stable(data, panel = as.panel("STUDY"))
  tab2 <- stable(data, panel = as.panel("STUDY", hline = FALSE))
  where <- grep("12-DEMO-002", tab1)
  expect_match(tab1[where], "\\hline", fixed = TRUE)
  expect_false(grepl("\\hline", tab2[where], fixed = TRUE))
})

test_that("nopagebreak for panels in longtable [PMT-TEST-0165]", {
  ans <- stable_long(stdata(), panel = "STUDY")
  inserted <- grep(pmtables:::.internal$marker.panel, ans, fixed = TRUE)
  check <- grep("DEMO", ans, fixed = TRUE)
  expect_identical(inserted, check)
  expect_match(
    ans[inserted],
    "\\\\*",
    fixed = TRUE
  )
  ans <- stable_long(stdata(), panel = as.panel("STUDY", nopagebreak = FALSE))
  inserted <- grep(pmtables:::.internal$marker.panel, ans, fixed = TRUE)
  expect_no_match(
    ans[inserted],
    "\\\\*",
    fixed = TRUE
  )
})

test_that("jut de-indents panel rows [PMT-TEST-0166]", {
  u <- list(WT = "kg")
  ans <- inspect(stdata(), panel = rowpanel("STUDY", jut = 1), units = u)
  code <- ans$output
  tab <- ans$tab
  header <- ans$head_rows
  header <- header[-c(1, length(header))]
  panels <- grepl("DEMO", tab, fixed = TRUE)
  expect_match(
    tab[!panels],
    "\\hskip 1ex",
    fixed = TRUE
  )
  expect_no_match(
    tab[panels],
    "\\hskip 1ex",
    fixed = TRUE
  )
  expect_match(
    header,
    "\\hskip 1ex",
    fixed = TRUE
  )
  indented <- c(header, tab[!panels])
  pick <- code[grep("hskip", code)]
  expect_identical(indented, pick)
})
