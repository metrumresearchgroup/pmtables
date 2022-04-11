library(testthat)
library(pmtables)

context("test-demographics-table")

cont <- "AGE,WT"
cat <- "SEXf,ASIANf"

test_that("pt_demographics - call with span and summary [PMT-TEST-0043]", {
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = cont,
    cols_cat = cat,
    span = c("STUDYf")
  )
  expect_is(out, "pmtable")
  expect_equal(unique(out$data$name), c("AGE", "WT", "SEXf", "ASIANf"))
  expect_equal(out$span$title,"STUDYf")
  ustudy <- unique(as.character(pmt_first$STUDYf))
  expect_equal(
    names(out$data),
    c("name", "Statistic", ustudy, "Summary")
  )
})

test_that("pt_demographics - call with span, no summary [PMT-TEST-0044]", {
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = cont,
    cols_cat = cat,
    span = c("STUDYf"),
    summarize_all = FALSE
  )
  expect_is(out, "pmtable")
  ustudy <- unique(as.character(pmt_first$STUDYf))
  expect_equal(
    names(out$data),
    c("name", "Statistic", ustudy)
  )
})

test_that("pt_demographics - call with summary, no span [PMT-TEST-0045]", {
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = cont,
    cols_cat = cat
  )
  expect_is(out, "pmtable")
  expect_equal(
    names(out$data),
    c("name", "Statistic", "Summary")
  )
})

test_that("demographics data summary - summary function [PMT-TEST-0046]", {

  # With tibble
  new_fun <- function(value = seq(1,5), ...) {
    dplyr::tibble(
      Mean = "1.32",
      Median = "1.45"
    )
  }
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'),
    cols_cat = c('SEXf','ASIANf'),
    fun = new_fun
  )
  expect_true("Median" %in% out$data$Statistic)

  # With data.frame
  new_fun <- function(value = seq(1,5), ...) {
    data.frame(Mean = mean(value), Median = median(value))
  }
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'),
    cols_cat = c('SEXf','ASIANf'),
    fun = new_fun
  )
  expect_true("Mean" %in% out$data$Statistic)
})

test_that("handle numeric values from cont summary function [PMT-TEST-0047]", {
  # Values coerced to character
  new_fun <- function(value = seq(1,5), ...) {
    dplyr::tibble(
      Mean = 1.32,
      Median = "1.45929342"
    )
  }
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'),
    cols_cat = c('SEXf','ASIANf'),
    fun = new_fun
  )
  expect_true("Median" %in% out$data$Statistic)
  cont <- dplyr::slice(out$data, 1:2)
  expect_equal(cont$Summary, c("1.32", "1.45929342"))
})

test_that("demographics data summary - summary function errors [PMT-TEST-0048]", {
  # Wrong structure (list)
  new_fun <- function(value = seq(1,5), ...) {
    list(a = value[1], b = value[2])
  }
  expect_error(
    pt_demographics(
      data = pmt_first,
      cols_cont = c('AGE', 'WT'),
      cols_cat = c('SEXf','ASIANf'),
      fun = new_fun
    ),
    "`fun` must return a data frame. See ?pmtables:::dem_cont_fun.",
    fixed = TRUE
  )

  # Wrong number of rows
  new_fun <- function(value = seq(1,5), ...) {
    data.frame(x = value)
  }
  expect_error(
    pt_demographics(
      data = pmt_first,
      cols_cont = c('AGE', 'WT'),
      cols_cat = c('SEXf','ASIANf'),
      fun = new_fun
    ),
    "`fun` must return a data frame with a single row. See ?pmtables:::dem_cont_fun.",
    fixed = TRUE
  )
})

test_that("demographics data summary - units [PMT-TEST-0049]", {
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'),
    cols_cat = c('SEXf','ASIANf'),
    span = c("Study"="STUDYf"),
    units = ensure_parens(list(WT="kg", AGE="yr")),
    stat_name = "Statistic"
  )
  expect_equal(unique(out$data$name), c("AGE (yr)", "WT (kg)", "SEXf", "ASIANf"))
})

test_that("demographics data summary - column renaming (no units) [PMT-TEST-0050]", {
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('Age'='AGE', 'Weight'='WT'),
    cols_cat = c(Sex='SEXf', Race='ASIANf', `Drug Form`='FORMf'),
    span = c("Study" = "STUDYf"),
    stat_name = "Statistic"
  )
  expect_equal(
    unique(out$data$name),
    c("Age", "Weight", "Sex", "Race", "Drug Form")
  )
})

test_that("demographics data summary - column renaming (with units) [PMT-TEST-0051]", {
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('Age'='AGE', 'Weight'='WT'),
    cols_cat = c(Sex='SEXf',Race='ASIANf', `Drug Form`='FORMf'),
    span = c("Study"="STUDYf"),
    units = ensure_parens(list(WT="kg", AGE="yr", FORMf="type")),
    stat_name = "Statistic"
  )
  expect_equal(
    unique(out$data$name),
    c("Age (yr)", "Weight (kg)", "Sex", "Race", "Drug Form (type)")
  )
})

test_that("demographics data summary - spot check values [PMT-TEST-0052]", {
  cols_cont <- c('AGE', 'WT')
  fun <- pmtables:::dem_cont_fun
  study1 <- dplyr::filter(pmt_first, STUDYf=="12-DEMO-001")
  a <- sig(mean(study1$WT, na.rm = TRUE))
  b <- ensure_parens(sig(sd(study1$WT, na.rm = TRUE)))
  ab1 <- paste(a,b)
  study2 <- dplyr::filter(pmt_first, STUDYf=="12-DEMO-002")
  a <- sig(range(study2$AGE, na.rm = TRUE))
  ab2 <- paste0(a, collapse = " / ")
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'),
    cols_cat = c('SEXf','ASIANf'),
    span = c("STUDYf"),
    stat_name = "Statistic"
  )
  expect_true(out$data$`12-DEMO-001`[4] == ab1)
  expect_true(out$data$`12-DEMO-002`[2] == ab2)
})

test_that("statistic column gets renamed [PMT-TEST-0053]", {
  out <- pt_demographics(pmt_first, cont, cat, stat_name = "Test")
  expect_equal(names(out$data)[2], "Test")
})

test_that("all data column gets renamed [PMT-TEST-0054]", {
  out <- pt_demographics(pmt_first, cont, cat, all_name = "Everything")
  expect_equal(names(out$data)[3], "Everything")
  out <- pt_demographics(pmt_first, cont, cat, span="FORMf", all_name = "ALL")
  expect_equal(names(out$data)[6], "ALL")
})

test_that("paneled or unpaneled output [PMT-TEST-0055]", {
  out <- pt_demographics(pmt_first, cont, cat, paneled = FALSE)
  expect_equal(out$clear_reps, "Covariate")
  expect_equal(out$hline_from, "Covariate")
  expect_true(out$panel$null)
  u <- ensure_parens(list(WT = "weight"))
  out <- pt_demographics(pmt_first, cont, cat, paneled = FALSE, units = u)
  expect_equal(unname(out$data$Covariate[4]), "WT (weight)")
})

test_that("add notes to the output [PMT-TEST-0056]", {
  out <- pt_demographics(pmt_first, cont, cat)
  expect_true("notes" %in% names(out))
  expect_identical(out$notes, pmtables:::pt_demographics_notes())
  out <- pt_demographics(pmt_first, cont, cat, notes = "mynotes")
  expect_identical(out$notes, "mynotes")
})

test_that("set width of Statistic column [PMT-TEST-0057]", {
  out <- pt_demographics(pmt_first, cont, cat, stat_width = 5)
  expect_match(out$align$update$Statistic, "5cm")
})

test_that("table argument is implemented [PMT-TEST-0058]", {
  tab <- list(WT = "Weight (kg)", SCR = "Creat", ASIANf = "Asian")
  out <- pt_demographics(pmt_first, "WT,AGE,SCR", cat, table = tab)
  expect_equal(out$data$name[1], "Weight (kg)")
  expect_equal(out$data$name[4], "AGE")
  expect_equal(out$data$name[7], "Creat")
  expect_equal(out$data$name[12], "Asian")
})

test_that("demographics table has group argument [PMT-TEST-0059]", {
  tab1a <- pt_demographics(
    pmt_first,
    cols_cont = "WT",
    cols_cat = c("SEXf", "ASIANf"),
    span = "FORMf"
  )$data
  tab1b <- pt_cat_long(
    pmt_first,
    cols = c("SEXf", "ASIANf"),
    span = "FORMf"
  )$data

  test1 <- select(filter(tab1a, name != "WT"), -1, -2)
  ref1 <- select(tab1b, -1, -2)
  expect_identical(test1, ref1)

  tab2a <- pt_demographics(
    pmt_first,
    cols_cont = "WT",
    cols_cat = c("SEXf", "ASIANf"),
    span = "FORMf",
    denom = "total"
  )$data
  tab2b <- pt_cat_long(
    pmt_first,
    cols= c("SEXf", "ASIANf"),
    span = "FORMf",
    denom = "total"
  )$data

  test2 <- select(filter(tab2a, name != "WT"), -1, -2)
  ref2 <- select(tab2b, -1, -2)
  expect_identical(test2, ref2)
})
