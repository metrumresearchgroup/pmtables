
context("test-inventory-data")

test_that("inventory data summary [PMT-TEST-0119]", {
  data <- pmt_first
  ans <- data_inventory_data(data, by = ".total")
  expect_equal(nrow(ans),1)
  expect_equal(names(ans)[1],".total")
  expect_equal(ncol(ans),9)
  expect_equal(
    names(ans)[2:9],
    c("SUBJ", "NMISS","NOBS", "NBQL", "POBS", "PBQL", "OOBS", "OBQL")
  )
  ans2 <- pmtables:::data_inventory_chunk(data, by = ".total")
  expect_identical(ans,ans2)
})

test_that("stacked inventory data summary [PMT-TEST-0120]", {
  data <- pmt_first
  ans <- pmtables:::data_inventory_data_split(data, by = "STUDYf")
  nstudy <- length(unique(data[["STUDY"]]))
  expect_equal(nrow(ans), 2*nstudy)
  expect_equal(names(ans)[1],"STUDYf")
  expect_equal(ncol(ans),9)
  expect_equal(
    names(ans)[2:9],
    c("SUBJ", "NMISS","NOBS", "NBQL", "POBS", "PBQL", "OOBS", "OBQL")
  )
})

test_that("missing columns [PMT-TEST-0121]", {
  data <- data.frame(DV = 1, ID = 2, BQL = 3, by = 'a')
  expect_silent(data_inventory_data(data, by = "by"))
  bad <- select(data, -DV)
  expect_error(
    data_inventory_data(bad, by = "by")
  )
  bad <- select(data, -ID)
  expect_error(
    data_inventory_data(bad, by = "by")
  )
  bad <- select(data, -BQL)
  expect_error(
    data_inventory_data(bad, by = "by")
  )
})

