context("test-demographics-table")

test_that("demographics data summary - simple", {

  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('Age'='AGE', 'Weight'='WT'),
    cols_cat = c(Sex='SEXf',Race='ASIANf'),
    span = c("Study"="STUDYf"),
    units = list(WT="kg"),
    stat_name = "Statistic"
  )
  out %>% stable() %>% st_preview()

  expect_true(all(class(out)==c("pmtable", "list")))
  expect_equal(unique(out$data$name), c("Age", "Weight (kg)", "Sex", "Race"))


})

test_that("demographics data summary - summary function", {

  new_fun <- function(value = seq(1,5)) {
    tibble(
      `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
      `median` = paste0(sig(median(value, na.rm = TRUE))),
      `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
    )
  }

  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('Age'='AGE', 'Weight'='WT'),
    cols_cat = c(Sex='SEXf',Race='ASIANf'),
    span = c("Study"="STUDYf"),
    sum_func = new_fun,
    units = list(WT="kg"),
    stat_name = "Statistic"
  )

  expect_true("median" %in% out$data$Statistic)

})
