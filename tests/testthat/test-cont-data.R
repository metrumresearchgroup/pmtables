
context("test-cont-data")



test_that("continuous data summary long - simple", {
  data <- pmt.first
  cols <- pmtables:::new_names("WT,ALB,SCR")
  ans <- cont_table_data(data, cols = cols)
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"outer")
  expect_identical(names(ans)[2],"name")
  expect_equal(nrow(ans),3)
})

test_that("continuous data summary long - by", {
  data <- pmt.first
  cols <- pmtables:::new_names("WT,ALB,SCR")
  by <- pmtables:::new_names("STUDYf")
  ans <- cont_table_data(data, cols = cols, by = by)
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"outer")
  expect_identical(names(ans)[2],"name")
  comb <- expand.grid(unique(data$STUDY),cols)
  expect_equal(nrow(ans),nrow(comb))
  expect_true(all(ans[[2]] %in% cols))
})

test_that("continuous data summary wide - simple", {
  data <- pmt.first
  cols <- pmtables:::new_names("WT,ALB,SCR")
  ans <- cont_table_data(
    data, cols = cols, wide = TRUE,
    fun = pmtables:::str_sum_2
  )
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"outer")
  expect_equal(nrow(ans),1)
})

test_that("continuous data summary wide - by", {
  data <- pmt.first
  cols <- pmtables:::new_names("WT,ALB,SCR")
  ans <- cont_table_data(
    data, cols = cols, wide = TRUE,
    fun = pmtables:::str_sum_2,
    by = pmtables:::new_names("RFf")
  )
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"outer")
  expect_equal(nrow(ans),length(unique(data[["RFf"]])))
})

