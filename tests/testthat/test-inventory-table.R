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

test_that("drop MISS column", {
  ans1 <- pt_data_inventory(pmt_pk)$data
  expect_equal(names(ans1)[2], "Number.MISS")
  ans2 <- pt_data_inventory(pmt_pk, drop_miss = TRUE)$data
  expect_equal(names(ans2)[2], "Number.OBS")
})

test_that("inventory table - denominator", {
  non_miss <- filter(pmt_pk, !(is.na(DV) & BQL==0))
  ans <- pt_data_inventory(pmt_pk)$data
  miss <- ans$Number.MISS
  obs <- ans$Number.OBS
  bql <- ans$Number.BQL
  expect_equal(miss + obs + bql, nrow(pmt_pk))
  expect_equal(nrow(non_miss), obs + bql)
  den <- obs + bql
  pobs <- ans$Percent.OBS
  pbq <- ans$Percent.BQL
  expect_equal(pmtables:::digit1(100*obs/den), pobs)
  expect_equal(pmtables:::digit1(100*bql/den), pbq)
})

test_that("inventory table - bql", {
  x <- c(rep(0,10), rep(1,2), rep(3,5))
  expect_equal(pmtables:::n_bql(x), 7)
  expect_equal(sum(pmtables:::is_bql(x)), 7)
})

test_that("inventory table - obs", {
  dv <-  c(1,2,3,4,5,6,7,8)
  bql <- c(0,0,1,0,0,2,0,0)
  ans <- pmtables:::n_obs(dv, bql)
  expect_equal(ans, 6)
})

test_that("inventory table - missing / non-missing", {
  x <- NA_real_
  dv <-  c(0,1,x,3,4,5,x,7,8,x)
  bql <- c(0,0,0,1,0,0,2,0,0,0)
  ans <- pmtables:::n_missing(dv, bql)
  expect_equal(ans, 2)
  ans <- pmtables:::n_non_missing(dv, bql)
  expect_equal(ans, 8)
})
