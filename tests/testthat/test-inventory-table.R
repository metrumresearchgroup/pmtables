
context("test-inventory-table")

data <- pmtables:::data("id")

test_that("inventory table", {
  ans <- pt_data_inventory(data,  by = "STUDYf")
  expect_is(ans,"pmtable")
  tab <- ans[["data"]]
  nstudy <- length(unique(data[["STUDYf"]]))
  expect_equal(nrow(tab),nstudy + 1)
  expect_equal(names(tab)[1], "STUDYf")
  expect_equal(names(tab)[2], "Number.SUBJ")
  expect_equal(names(tab)[3], "Number.MISS")
  expect_equal(names(tab)[4], "Number.OBS")
  expect_equal(names(tab)[5], "Number.BQL")
  expect_equal(names(tab)[6], "Percent.OBS")
  expect_equal(names(tab)[7], "Percent.BQL")
})

test_that("inventory table - stacked", {
  data <- pmtables:::data("obs")
  ans <- pt_data_inventory(data,  panel = "SEQf", by = "STUDYf",stacked = TRUE)
  expect_is(ans,"pmtable")
  tab <- ans[["data"]]
  smry <- dplyr::count(data,SEQf,STUDYf)
  nseq <- length(unique(smry[["SEQf"]]))
  expect_equal(nrow(tab), nrow(smry) + nseq)
  expect_equal(names(tab)[1], "STUDYf")
  expect_equal(names(tab)[2], "SEQf")
  expect_equal(names(tab)[3], "Number.SUBJ")
  expect_equal(names(tab)[4], "Number.MISS")
  expect_equal(names(tab)[5], "Number.OBS")
  expect_equal(names(tab)[6], "Number.BQL")
  expect_equal(names(tab)[7], "Percent.OBS")
  expect_equal(names(tab)[8], "Percent.BQL")
})
