library(testthat)
library(pmtables)

context("test-inventory-table")

test_that("inventory table [PMT-TEST-0122]", {
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

test_that("inventory table grouped paneled [PMT-TEST-0123]", {
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


test_that("inventory table - stacked [PMT-TEST-0124]", {
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

test_that("inventory table - different BQL cols [PMT-TEST-0125]", {
  data1 <- pmt_pk
  data2 <- rename(data1, BLQ = BQL)
  ans1 <- pt_data_inventory(data1)
  ans2 <- pt_data_inventory(data2)
  expect_false(identical(ans1,ans2))
})

test_that("inventory table - no bq col [PMT-TEST-0126]", {
  data <- select(pmt_pk, -BQL)
  ans <- pt_data_inventory(data)
  expect_false(any(grepl("BQL", names(ans$data), fixed = TRUE)))
})

test_that("notes - inventory [PMT-TEST-0127]", {
  ans <- pt_data_inventory(pmt_pk)$notes
  expect_is(ans, "character")
  expect_length(ans,4)
  expect_match(ans[1], "SUBJ: subjects", fixed = TRUE)
  expect_match(ans[2], "below", fixed = TRUE)
  expect_match(ans[3], "MISS: missing", fixed = TRUE)
  expect_match(ans[4], "OBS: observations", fixed = TRUE)
})

test_that("drop MISS column [PMT-TEST-0128]", {
  ans1 <- pt_data_inventory(pmt_pk)$data
  expect_equal(names(ans1)[2], "Number.MISS")
  ans2 <- pt_data_inventory(pmt_pk, drop_miss = TRUE)$data
  expect_equal(names(ans2)[2], "Number.OBS")
})

test_that("inventory table - denominator [PMT-TEST-0129]", {
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

test_that("inventory table - bql [PMT-TEST-0130]", {
  x <- c(rep(0,10), rep(1,2), rep(3,5))
  expect_equal(pmtables:::n_bql(x), 7)
  expect_equal(sum(pmtables:::is_bql(x)), 7)
})

test_that("inventory table - obs [PMT-TEST-0131]", {
  dv <-  c(1,2,3,4,5,6,7,8)
  bql <- c(0,0,1,0,0,2,0,0)
  ans <- pmtables:::n_obs(dv, bql)
  expect_equal(ans, 6)
})

test_that("inventory table - missing / non-missing [PMT-TEST-0132]", {
  x <- NA_real_
  dv <-  c(0,1,x,3,4,5,x,7,8,x)
  bql <- c(0,0,0,1,0,0,2,0,0,0)
  ans <- pmtables:::n_missing(dv, bql)
  expect_equal(ans, 2)
  ans <- pmtables:::n_non_missing(dv, bql)
  expect_equal(ans, 8)
})

test_that("handle BQL and BLQ inventory table [PMT-TEST-0133]", {

  data1 <- pmt_first
  data2 <- dplyr::rename(data1, BLQ = BQL)
  data3 <- dplyr::mutate(data2, BLQ = NULL, BQL = NULL)

  tab1 <- pt_data_inventory(data1, panel = "STUDYf")
  tab2 <- pt_data_inventory(data2, panel = "STUDYf")
  tab3 <- pt_data_inventory(data3, panel = "STUDYf")

  expect_equal(names(tab1$data)[5], "Number.BQL")
  expect_equal(names(tab2$data)[5], "Number.BLQ")
  expect_false(any(grepl("BLQ|BQL", names(tab3$data))))

  expect_length(tab1$notes, 4)
  expect_length(tab2$notes, 4)
  expect_length(tab3$notes, 3)

  expect_equal(tab1$notes[2], "BQL: below quantification limit")
  expect_equal(tab2$notes[2], "BLQ: below limit of quantification")
  expect_equal(tab1$notes[3], "MISS: missing observations (non-BQL)" )
  expect_equal(tab2$notes[3], "MISS: missing observations (non-BLQ)")
  expect_equal(tab3$notes[2], "MISS: missing observations")
})

test_that("Optional All Data summary - inventory table [PMT-INVEN-0001]", {
  a <- pt_data_inventory(
    pmt_obs,
    by = "STUDYf"
  )$data
  b <- pt_data_inventory(
    pmt_obs,
    by = "STUDYf",
    summarize_all = FALSE
  )$data
  expect_identical(a[1:4,], b[1:4,])
  expect_true(nrow(a) == 1 + nrow(b))
})

test_that("Change All Data name - inventory table [PMT-INVEN-0002]", {
  a <- pt_data_inventory(
    pmt_obs,
    by = "STUDYf"
  )$data
  b <- pt_data_inventory(
    pmt_obs,
    by = "STUDYf",
    all_name="ALL"
  )$data
  expect_match(a$STUDYf[5], "All")
  expect_match(b$STUDYf[5], "ALL")
})

test_that("Change stacked group name - inventory table [PMT-INVEN-0003]", {
  a <- pt_data_inventory(
    pmt_obs,
    by = "STUDYf",
    stacked = TRUE,
    panel = "SEQf",
    all_name_stacked = "STACKED"
  )$data
  expect_match(a$STUDYf[5], "STACKED")
  expect_match(a$STUDYf[11], "STACKED")
})

test_that("Long inventory table", {

  .table <- list(RFf = "Renal function", SEXf = "Sex", FORMf = "Formulation")
  .cols <- c("RFf", "FORMf", "SEXf")

  # Basic call plus table argument
  tab <- pt_inventory_long(
    data = pmt_first,
    cols = .cols,
    table = .table
  )

  expect_is(tab, "pmtable")
  expect_is(tab$data, "data.frame")

  out <- tab$data

  var <- sort(c(unlist(.table, use.names = FALSE), "NULL"))
  expect_equal(sort(unique(out$var)), var)

  expect_equal(
    names(out),
    c(
      "var", "level", "Number.SUBJ", "Number.MISS", "Number.OBS",
      "Number.BQL", "Percent.OBS", "Percent.BQL"
    )
  )

  expect_in(c("normal", "mild", "moderate", "severe"), out$level)
  expect_in(c("tablet", "capsule", "troche"), out$level)
  expect_in(c("male", "female"), out$level)

  expect_equal(tab$notes, pmtables:::pt_data_inventory_notes())

  # Set width of level column
  tab <- pt_inventory_long(
    data = pmt_first,
    cols = .cols,
    level_width = 0.52
  )

  expect_match(tab$align$update$level, "raggedright", all = FALSE)
  expect_match(tab$align$update$level, "0.52cm", all = FALSE)

  # Set rename all data
  tab <- pt_inventory_long(
    data = pmt_first,
    cols = .cols,
    all_name = "Check ALL"
  )
  nn <- nrow(tab$data)

  expect_match(tab$data$level[nn], "Check ALL", all = FALSE)

  # Don't summarize all
  tab2 <- pt_inventory_long(
    data = pmt_first,
    cols = .cols,
    summarize_all = FALSE
  )
  expect_equal(nrow(tab2$data), nn-1)
  expect_no_match(tab$data$level, "All data", all = FALSE)
})

test_that("Long inventory table - errors", {

  .table <- list(RFf = "Renal function", SEXf = "Sex", FORMf = "Formulation")
  .cols <- c("RFf", "FORMf", "SEXf")

  expect_error(
    pt_inventory_long(
      data = pmt_first,
      cols = NULL
    ),
    "cols is not a character vector"
  )
  expect_error(
    pt_inventory_long(
      data = data.frame(),
      cols = .cols
    ),
    regexp = "nrow(data) not greater than 1",
    fixed = TRUE
  )
  # Passed through
  expect_error(
    pt_inventory_long(
      data = pmt_first,
      cols = .cols,
      id_col = "ID COL"
    ),
    regexp = "there was a problem finding required columns"
  )
  # Passed through
  expect_error(
    pt_inventory_long(
      data = pmt_first,
      cols = .cols,
      dv_col = "DV COL"
    ),
    regexp = "there was a problem finding required",
    fixed = TRUE
  )
  # Passed through
  expect_error(
    pt_inventory_long(
      data = pmt_first,
      cols = .cols,
      bq_col = "BQL COL"
    ),
    regexp = "there was a problem finding required",
    fixed = TRUE
  )
})
