
context("test-inventory-data")

data <- pmtables:::data("id")

test_that("inventory data summary", {
  ans <- data_inventory_data(data, outer = ".total")
  expect_equal(nrow(ans),1)
  expect_equal(names(ans)[1],".total")
  expect_equal(ncol(ans),9)
  expect_equal(
    names(ans)[2:9],
    c("SUBJ", "NMISS","NOBS", "NBQL", "POBS", "PBQL", "OOBS", "OBQL")
  )
  ans2 <- pmtables:::data_inventory_chunk(data, outer = ".total")
  expect_identical(ans,ans2)
})



