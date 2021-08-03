library(testthat)
library(pmtables)

context("test-demographics-table")

test_that("demographics data summary - basic call", {
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'),
    cols_cat = c('SEXf','ASIANf'),
    span = c("STUDYf"),
    stat_name = "Statistic"
  )
  expect_true(all(class(out)==c("pmtable", "list")))
  expect_equal(unique(out$data$name), c("AGE", "WT", "SEXf", "ASIANf"))
  expect_equal(out$span$title,"STUDYf")
})

test_that("demographics data summary - summary function", {

  # With tibble
  new_fun <- function(value = seq(1,5)) {
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
  new_fun <- function(value = seq(1,5)) {
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

test_that("handle numeric values from cont summary function", {
  # Values coerced to character
  new_fun <- function(value = seq(1,5)) {
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
  expect_equal(cont$`All data`, c("1.32", "1.45929342"))
})

test_that("demographics data summary - summary function errors", {
  # Wrong structure (list)
  new_fun <- function(value = seq(1,5)) {
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
  new_fun <- function(value = seq(1,5)) {
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

test_that("demographics data summary - units", {
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

test_that("demographics data summary - column renaming (no units)", {
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

test_that("demographics data summary - column renaming (with units)", {
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

test_that("demographics data summary - spot check values", {
  cols_cont <- c('AGE', 'WT')
  fun <- pmtables:::dem_cont_fun
  study1 <- dplyr::filter(pmt_first, STUDYf=="12-DEMO-001")
  a <- sig(mean(study1$WT, na.rm = TRUE))
  b <- ensure_parens(sig(sd(study1$WT, na.rm = TRUE)))
  ab1 <- paste(a,b)
  study2 <- dplyr::filter(pmt_first, STUDYf=="12-DEMO-002")
  a <- sig(range(study2$AGE, na.rm = TRUE))
  ab2 <- paste0(a, collapse = " - ")
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

cont <- "AGE,WT"
cat <- "SEXf,ASIANf"
test_that("statistic column gets renamed", {
  out <- pt_demographics(pmt_first, cont, cat, stat_name = "Test")
  expect_equal(names(out$data)[2], "Test")
})

test_that("all data column gets renamed", {
  out <- pt_demographics(pmt_first, cont, cat, all_name = "Everything")
  expect_equal(names(out$data)[3], "Everything")
  out <- pt_demographics(pmt_first, cont, cat, span="FORMf", all_name = "ALL")
  expect_equal(names(out$data)[6], "ALL")
})

test_that("paneled or unpaneled output", {
  out <- pt_demographics(pmt_first, cont, cat, paneled = FALSE)
  expect_equal(out$clear_reps, "Covariate")
  expect_equal(out$hline_from, "Covariate")
  expect_true(out$panel$null)
  u <- ensure_parens(list(WT = "weight"))
  out <- pt_demographics(pmt_first, cont, cat, paneled = FALSE, units = u)
  expect_equal(unname(out$data$Covariate[4]), "WT (weight)")
})

test_that("add notes to the output", {
  out <- pt_demographics(pmt_first, cont, cat)
  expect_true("notes" %in% names(out))
  expect_identical(out$notes, pmtables:::pt_demographics_notes())
  out <- pt_demographics(pmt_first, cont, cat, notes = "mynotes")
  expect_identical(out$notes, "mynotes")
})

test_that("set width of Statistic column", {
  out <- pt_demographics(pmt_first, cont, cat, stat_width = 5)
  expect_match(out$align$update$Statistic, "5cm")
})
