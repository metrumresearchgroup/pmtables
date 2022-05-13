
context("test-cont-data")

test_that("continuous data summary long - simple [PMT-TEST-0013]", {
  data <- pmt_first
  cols <- pmtables:::new_names("WT,ALB,SCR")
  ans <- cont_table_data(data, cols = cols)
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"outer")
  expect_identical(names(ans)[2],"name")
  expect_equal(nrow(ans),3)
})

test_that("continuous data summary long - by [PMT-TEST-0014]", {
  data <- pmt_first
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

test_that("continuous data summary wide - simple [PMT-TEST-0015]", {
  data <- pmt_first
  cols <- pmtables:::new_names("WT,ALB,SCR")
  ans <- cont_table_data(
    data, cols = cols, wide = TRUE,
    fun = pmtables:::cont_wide_fun
  )
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"outer")
  expect_equal(nrow(ans),1)
})

test_that("continuous data summary wide - by [PMT-TEST-0016]", {
  data <- pmt_first
  cols <- pmtables:::new_names("WT,ALB,SCR")
  ans <- cont_table_data(
    data, cols = cols, wide = TRUE,
    fun = pmtables:::cont_wide_fun,
    by = pmtables:::new_names("RFf")
  )
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"outer")
  expect_equal(nrow(ans),length(unique(data[["RFf"]])))
})

