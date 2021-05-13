context("test-demographics-table")

test_that("demographics data summary - simple", {

  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('Age'='AGE', 'Weight'='WT'),
    cols_cat = c(Sex='SEXf',Race='ASIANf'),
    span = c("Study"="STUDYf"),
    units = list(WT="kg", AGE="yrs"),
    stat_name = "Statistic"
  )
  #out %>% stable() %>% st_preview()

  expect_true(all(class(out)==c("pmtable", "list")))

  expect_equal(unique(out$data$name), c("Age (yrs)", "Weight (kg)", "Sex", "Race"))


})
