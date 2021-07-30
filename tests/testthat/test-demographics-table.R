library(testthat)
library(pmtables)

context("test-demographics-table")

test_that("demographics data summary - simple", {

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
    tibble(
      `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
      `median` = paste0(sig(median(value, na.rm = TRUE))),
      `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
    )
  }
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'), cols_cat = c('SEXf','ASIANf'),
    span = c("Study"="STUDYf"), fun = new_fun
  )
  expect_true("median" %in% out$data$Statistic)

  # With data.frame
  new_fun <- function(value = seq(1,5)) {
    data.frame(
      `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
      `median` = paste0(sig(median(value, na.rm = TRUE))),
      `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
    )
  }
  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'), cols_cat = c('SEXf','ASIANf'),
    span = c("Study"="STUDYf"), fun = new_fun
  )
  expect_true("median" %in% out$data$Statistic)
})

test_that("demographics data summary - summary function errors", {
  # Wrong structure (list)
  new_fun <- function(value = seq(1,5)) {
    list(
      `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
      `median` = paste0(sig(median(value, na.rm = TRUE))),
      `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
    )
  }
  expect_error(
    pt_demographics(data = pmt_first, cols_cont = c('AGE', 'WT'),
                    cols_cat = c('SEXf','ASIANf'),
                    span = c("Study"="STUDYf"), fun = new_fun),
    "- Your function does not return values in the correct format. Output must be a tibble or data frame. \nPlease see pmtables::dem_cont_fun() for more information.",
    fixed = TRUE
  )

  # Wrong number of rows
  new_fun <- function(value = seq(1,5)) {
    tibble(
      `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
      `vals` = paste0(sig(value)),
      `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
    )
  }
  expect_error(
    pt_demographics(data = pmt_first, cols_cont = c('AGE', 'WT'),
                    cols_cat = c('SEXf','ASIANf'),
                    span = c("Study"="STUDYf"), fun = new_fun),
    "- Your function does not summarize data to a single row. \nPlease see pmtables::dem_cont_fun() for more information.",
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
    cols_cat = c(Sex='SEXf',Race='ASIANf', `Drug Form`='FORMf'),
    span = c("Study"="STUDYf"),
    stat_name = "Statistic"
  )
  expect_equal(unique(out$data$name), c("Age", "Weight", "Sex", "Race", "Drug Form"))
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
  expect_equal(unique(out$data$name), c("Age (yr)", "Weight (kg)", "Sex", "Race", "Drug Form (type)"))
})

test_that("demographics data summary - spot check values", {
  data <- pmt_first
  cols_cont <- c('AGE', 'WT')
  span <- "STUDYf"
  sum_func <- pmtables:::dem_cont_fun
  cont_table <- pivot_longer(data, cols = all_of(unname(cols_cont)))
  cont_table <- mutate(cont_table, name = fct_inorder(name))
  cont_table <- group_by(cont_table, name, !!sym(span))
  cont_table <- summarise(cont_table, sum_func(value), .groups = "drop")

  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('AGE', 'WT'),
    cols_cat = c('SEXf','ASIANf'),
    span = c("STUDYf"),
    stat_name = "Statistic"
  )

  expect_true(out$data$`12-DEMO-001`[1] == cont_table$`mean (sd)`[1])
  mm <- filter(out$data, Statistic=="min-max")
  expect_true(mm$`12-DEMO-001`[1] == cont_table$`min-max`[1])
  expect_true(mm$`12-DEMO-002`[2] == cont_table$`min-max`[6])
})
