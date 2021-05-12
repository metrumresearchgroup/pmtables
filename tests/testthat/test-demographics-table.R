context("test-demographics-table")

test_that("demographics data summary - simple", {

  out <- pt_demographics(
    data = pmt_first,
    cols_cont = c('Age (yrs)'='AGE', 'Weight (kg)'='WT'),
    cols_cat = c(Sex='SEXf',Race='ASIANf'),
    span = c("Study"="STUDYf"),
    stat_name = "Statistic"
  )

  expect_true(all(class(out)==c("pmtable", "list")))

  expect_equal(unique(out$data$name), c("Age (yrs)", "Weight (kg)", "Sex", "Race"))
  #out %>% stable() %>% st_preview()

})
