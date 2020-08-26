library(testthat)

context("test-inventory-table")

test_that("inventory table", {
  data <- pmt_first
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

test_that("inventory table grouped paneled", {
  data <- pmt_first
  ans <- pt_data_inventory(data,  by = "STUDYf", panel = "FORMf")
  expect_is(ans,"pmtable")
  expect_identical(unname(ans$panel$col), "FORMf")
  tab <- ans[["data"]]
  nstudy <- length(unique(paste0(data[["STUDYf"]],data[["FORMf"]])))
  expect_equal(nrow(tab),nstudy + 1)
  expect_equal(names(tab)[1],  "STUDYf")
  expect_equal(names(tab)[2],  "FORMf")
  expect_equal(names(tab)[3],  "Number.SUBJ")
  expect_equal(names(tab)[4],  "Number.MISS")
  expect_equal(names(tab)[5],  "Number.OBS")
  expect_equal(names(tab)[6],  "Number.BQL")
  expect_equal(names(tab)[7],  "Group percent.OBS")
  expect_equal(names(tab)[8],  "Group percent.BQL")
  expect_equal(names(tab)[9],  "Overall percent.OBS")
  expect_equal(names(tab)[10], "Overall percent.BQL")
})


test_that("inventory table - stacked", {
  data <- pmt_obs
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

test_that("inventory table - different BQL cols", {
  data1 <- pmt_pk
  data2 <- rename(data1, BLQ = BQL)
  ans1 <- pt_data_inventory(data1)
  ans2 <- pt_data_inventory(data2)
  expect_identical(ans1,ans2)
})

test_that("inventory table - no bq col", {
  data <- select(pmt_pk, -BQL)
  ans <- pt_data_inventory(data)
  expect_false(any(grepl("BQL", names(ans$data), fixed = TRUE)))
})

test_that("notes - inventory", {
  ans <- pt_data_inventory(pmt_pk)$notes
  expect_is(ans, "character")
  expect_length(ans,4)
  expect_match(ans[1], "SUBJ: subjects", fixed = TRUE)
  expect_match(ans[2], "below", fixed = TRUE)
  expect_match(ans[3], "MISS: missing", fixed = TRUE)
  expect_match(ans[4], "OBS: observations", fixed = TRUE)
})
