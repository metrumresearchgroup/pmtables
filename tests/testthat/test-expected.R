
library(testthat)
library(pmtables)
library(dplyr)

context("test-expected")

read_tex <- function(file) {
  readLines(file.path("validate", file))
}

test_that("basic-table [PMT-TEST-0085]", {
  expect <- read_tex("basic-table.tex")
  ans <- stable(pmt_summarized)
  expect_identical(expect, as.character(ans))
})

test_that("basic-table-bold [PMT-TEST-0086]", {
  expect <- read_tex("basic-table-bold.tex")
  ans <- stable(pmt_summarized, cols_bold = TRUE)
  expect_identical(expect, as.character(ans))
})

test_that("file-names [PMT-TEST-0087]", {
  expect <- read_tex("file-names.tex")
  ans <- stable(
    pmt_summarized,
    r_file = "validate.Rmd",
    output_file = "file-names.tex"
  )
  expect_identical(expect, as.character(ans))
})

test_that("notes-tpt [PMT-TEST-0088]", {
  expect <- read_tex("notes-tpt.tex")
  ans <- stable(
    pmt_summarized,
    notes = c("WT: weight", "ALB: albumin")
  )
  expect_identical(expect, as.character(ans))
})

test_that("notes-mini [PMT-TEST-0089]", {
  expect <- read_tex("notes-mini.tex")
  conf <- noteconf(type = "minipage")
  ans <- stable(
    pmt_summarized,
    notes = c("WT: weight", "ALB: albumin"),
    note_config = conf
  )
  expect_identical(expect, as.character(ans))
})

test_that("panel-basic [PMT-TEST-0090]", {
  expect <- read_tex("panel-basic.tex")
  ans <- stable(
    pmt_summarized,
    panel = "STUDY"
  )
  expect_identical(expect, as.character(ans))
})

test_that("panel-prefix [PMT-TEST-0091]", {
  expect <- read_tex("panel-prefix.tex")
  ans <- stable(
    pmt_summarized,
    panel = as.panel("STUDY", prefix = "Study: ")
  )
  expect_identical(expect, as.character(ans))
})

test_that("clear-reps [PMT-TEST-0092]", {
  expect <- read_tex("clear-reps.tex")
  ans <- stable(
    pmt_summarized,
    clear_reps = "STUDY"
  )
  expect_identical(expect, as.character(ans))
})

test_that("clear-grouped-reps [PMT-TEST-0093]", {
  expect <- read_tex("clear-grouped-reps.tex")
  ans <- stable(
    pmt_summarized,
    clear_grouped_reps = c("STUDY", "DOSE")
  )
  expect_identical(expect, as.character(ans))
})

test_that("hline-at [PMT-TEST-0094]", {
  expect <- read_tex("hline-at.tex")
  ans <- stable(
    pmt_summarized,
    hline_at = c(2,4,6)
  )
  expect_identical(expect, as.character(ans))
})

test_that("hline-from [PMT-TEST-0095]", {
  expect <- read_tex("hline-from.tex")
  ans <- stable(
    pmt_summarized,
    hline_from = "STUDY"
  )
  expect_identical(expect, as.character(ans))
})

test_that("hline-from-clear [PMT-TEST-0096]", {
  expect <- read_tex("hline-from-clear.tex")
  ans <- stable(
    pmt_summarized,
    hline_from = "STUDY",
    clear_reps = "STUDY"
  )
  expect_identical(expect, as.character(ans))
})

test_that("align [PMT-TEST-0097]", {
  expect <- read_tex("align.tex")
  ans <- stable(
    pmt_summarized,
    align = cols_center(.outer = 'lr', DOSE = 'r')
  )
  expect_identical(expect, as.character(ans))
})

test_that("cols-rename [PMT-TEST-0098]", {
  expect <- read_tex("col-rename.tex")
  ans <- stable(
    pmt_summarized,
    cols_rename  =c (Weight = "WT", Dose = "DOSE")
  )
  expect_identical(expect, as.character(ans))
})

test_that("cols-blank [PMT-TEST-0099]", {
  expect <- read_tex("col-blank.tex")
  ans <- stable(
    pmt_summarized,
    cols_blank = "WT,ALB"
  )
  expect_identical(expect, as.character(ans))
})

test_that("col-multi-line [PMT-TEST-0100]", {
  expect <- read_tex("col-multi-line.tex")
  ans <- stable(
    pmt_summarized,
    cols_rename = c("Protocol...Number" = "STUDY", Weight = "WT")
  )
  expect_identical(expect, as.character(ans))
})

test_that("col-multi-line-units [PMT-TEST-0101]", {
  expect <- read_tex("col-multi-line-units.tex")
  ans <- stable(
    pmt_summarized,
    cols_rename = c("Protocol...Number" = "STUDY", Weight = "WT"),
    units = list(WT = "(kg)", CRCL = "(ml/min)", AGE = "(years)",
                 ALB = "(g/dL)", SCR = "(mg/dL)")
  )
  expect_identical(expect, as.character(ans))
})

test_that("span [PMT-TEST-0102]", {
  expect <- read_tex("span.tex")
  ans <- stable(
    pmt_summarized,
    span = colgroup("Final model", CRCL:AGE)
  )
  expect_identical(expect, as.character(ans))
})

test_that("span-levels [PMT-TEST-0103]", {
  expect <- read_tex("span-levels.tex")
  ans <- stable(
    pmt_summarized,
    span = list(
      colgroup("Final model", CRCL:AGE),
      colgroup("All covariates", WT:SCR, level = 2)
    )
  )
  expect_identical(expect, as.character(ans))
})

test_that("row-space [PMT-TEST-0104]", {
  expect <- read_tex("row-space.tex")
  ans <- stable(
    pmt_summarized,
    sizes = tab_size(row = 0.8)
  )
  expect_identical(expect, as.character(ans))
})

test_that("col-space [PMT-TEST-0105]", {
  expect <- read_tex("col-space.tex")
  ans <- stable(
    pmt_summarized,
    sizes = tab_size(col = 15)
  )
  expect_identical(expect, as.character(ans))
})

test_that("header-space [PMT-TEST-0106]", {
  expect <- read_tex("header-space.tex")
  ans <- stable(
    pmt_summarized,
    cols_rename = c("Clinical ... Study ... Number" = "STUDY"),
    sizes = tab_size(row = 2.4, header_row = -1.4)
  )
  expect_identical(expect, as.character(ans))
})

test_that("continuous-long-panel [PMT-TEST-0107]", {
  expect <- read_tex("continuous-long-panel.tex")
  ans <- pt_cont_long(
    pmt_first, cols = "WT,CRCL,ALB", panel = "STUDYf",
    units = list(WT = "(kg)", CRCL = "(ml/min)", ALB = "(g/dL)")
  ) %>% as_stable()
  expect_identical(expect, as.character(ans))
})

test_that("continuous-wide-by [PMT-TEST-0108]", {
  expect <- read_tex("continuous-wide-by.tex")
  ans <- pt_cont_wide(
    pmt_first,
    cols = "WT,CRCL,ALB", by = "STUDYf",
    units = list(WT = "(kg)", CRCL = "(ml/min)", ALB = "(g/dL)")
  ) %>% as_stable()

  expect_identical(expect, as.character(ans))
})

test_that("cat-long-span [PMT-TEST-0109]", {
  expect <- read_tex("cat-long-span.tex")
  ans <- pt_cat_long(
    pmt_first,
    cols = "SEXf,RFf,FORMf", span = "STUDYf"
  ) %>% as_stable()

  expect_identical(expect, as.character(ans))
})

test_that("cat-wide-by-panel [PMT-TEST-0110]", {
  expect <- read_tex("cat-wide-by-panel.tex")
  ans <- pt_cat_wide(
    pmt_first,
    cols = "SEXf,RFf", by = "FORMf", panel = "STUDYf"
  ) %>% as_stable()

  expect_identical(expect, as.character(ans))
})

test_that("inventory-by [PMT-TEST-0111]", {
  expect <- read_tex("inventory-by.tex")
  ans <- pt_data_inventory(
    pmt_pk,
    by = "STUDYf"
  ) %>% as_stable()

  expect_identical(expect, as.character(ans))
})

test_that("inventory-panel-by [PMT-TEST-0112]", {
  expect <- read_tex("inventory-panel-by.tex")
  ans <- pt_data_inventory(
    pmt_pk,
    by = "STUDYf",
    panel = "FORMf"
  ) %>% as_stable()
  expect_identical(expect, as.character(ans))
})

test_that("inventory-stacked [PMT-TEST-0113]", {
  expect <- read_tex("inventory-stacked.tex")
  ans <- pt_data_inventory(
    pmt_obs,
    by = "STUDYf",
    panel = "SEQf",
    stacked = TRUE
  ) %>% as_stable()
  expect_identical(expect, as.character(ans))
})




