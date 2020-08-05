
library(testthat)
library(pmtables)
library(dplyr)

context("test-expected")

read_tex <- function(file) {
  readLines(file.path("validate", file))
}

test_that("basic-table", {
  expect <- read_tex("basic-table.tex")
  ans <- stable(pmt_summarized)
  expect_identical(expect, as.character(ans))
})

test_that("basic-table-bold", {
  expect <- read_tex("basic-table-bold.tex")
  ans <- stable(pmt_summarized, col_bold = TRUE)
  expect_identical(expect, as.character(ans))
})

test_that("file-names", {
  expect <- read_tex("file-names.tex")
  ans <- stable(
    pmt_summarized,
    r_file = "validate.Rmd",
    output_file = "file-names.tex"
  )
  expect_identical(expect, as.character(ans))
})

test_that("notes-tpt", {
  expect <- read_tex("notes-tpt.tex")
  ans <- stable(
    pmt_summarized,
    notes = c("WT: weight", "ALB: albumin")
  )
  expect_identical(expect, as.character(ans))
})

test_that("notes-mini", {
  expect <- read_tex("notes-mini.tex")
  conf <- noteconf(type = "minipage")
  ans <- stable(
    pmt_summarized,
    notes = c("WT: weight", "ALB: albumin"),
    note_config = conf
  )
  expect_identical(expect, as.character(ans))
})

test_that("panel-basic", {
  expect <- read_tex("panel-basic.tex")
  ans <- stable(
    pmt_summarized,
    panel = "STUDY"
  )
  expect_identical(expect, as.character(ans))
})

test_that("panel-prefix", {
  expect <- read_tex("panel-prefix.tex")
  ans <- stable(
    pmt_summarized,
    panel = as.panel("STUDY", prefix = "Study: ")
  )
  expect_identical(expect, as.character(ans))
})

test_that("clear-reps", {
  expect <- read_tex("clear-reps.tex")
  ans <- stable(
    pmt_summarized,
    clear_reps = "STUDY"
  )
  expect_identical(expect, as.character(ans))
})

test_that("clear-grouped-reps", {
  expect <- read_tex("clear-grouped-reps.tex")
  ans <- stable(
    pmt_summarized,
    clear_grouped_reps = c("STUDY", "DOSE")
  )
  expect_identical(expect, as.character(ans))
})

test_that("hline-at", {
  expect <- read_tex("hline-at.tex")
  ans <- stable(
    pmt_summarized,
    hline_at = c(2,4,6)
  )
  expect_identical(expect, as.character(ans))
})

test_that("hline-from", {
  expect <- read_tex("hline-from.tex")
  ans <- stable(
    pmt_summarized,
    hline_from = "STUDY"
  )
  expect_identical(expect, as.character(ans))
})

test_that("hline-from-clear", {
  expect <- read_tex("hline-from-clear.tex")
  ans <- stable(
    pmt_summarized,
    hline_from = "STUDY",
    clear_reps = "STUDY"
  )
  expect_identical(expect, as.character(ans))
})

test_that("align", {
  expect <- read_tex("align.tex")
  ans <- stable(
    pmt_summarized,
    align = cols_center(.outer = 'lr', DOSE = 'r')
  )
  expect_identical(expect, as.character(ans))
})

test_that("cols-rename", {
  expect <- read_tex("col-rename.tex")
  ans <- stable(
    pmt_summarized,
    col_rename  =c (Weight = "WT", Dose = "DOSE")
  )
  expect_identical(expect, as.character(ans))
})

test_that("cols-blank", {
  expect <- read_tex("col-blank.tex")
  ans <- stable(
    pmt_summarized,
    col_blank = "WT,ALB"
  )
  expect_identical(expect, as.character(ans))
})

test_that("col-multi-line", {
  expect <- read_tex("col-multi-line.tex")
  ans <- stable(
    pmt_summarized,
    col_rename = c("Protocol...Number" = "STUDY", Weight = "WT")
  )
  expect_identical(expect, as.character(ans))
})

test_that("col-multi-line-units", {
  expect <- read_tex("col-multi-line-units.tex")
  ans <- stable(
    pmt_summarized,
    col_rename = c("Protocol...Number" = "STUDY", Weight = "WT"),
    units = list(WT = "(kg)", CRCL = "(ml/min)", AGE = "(years)",
                 ALB = "(g/dL)", SCR = "(mg/dL)")
  )
  expect_identical(expect, as.character(ans))
})

test_that("span", {
  expect <- read_tex("span.tex")
  ans <- stable(
    pmt_summarized,
    span = colgroup("Final model", CRCL:AGE)
  )
  expect_identical(expect, as.character(ans))
})

test_that("span-levels", {
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

test_that("continuous-long-panel", {
  expect <- read_tex("continuous-long-panel.tex")
  ans <- pt_cont_long(
    pmt_first, cols = "WT,CRCL,ALB", panel = "STUDYf",
    units = list(WT = "(kg)", CRCL = "(ml/min)", ALB = "(g/dL)")
  ) %>% as_stable()
  expect_identical(expect, as.character(ans))
})

test_that("continuous-wide-by", {
  expect <- read_tex("continuous-wide-by.tex")
  ans <- pt_cont_wide(
  pmt_first,
  cols = "WT,CRCL,ALB", by = "STUDYf",
  units = list(WT = "(kg)", CRCL = "(ml/min)", ALB = "(g/dL)")
) %>% as_stable()

  expect_identical(expect, as.character(ans))
})

test_that("cat-long-by", {
  expect <- read_tex("cat-long-by.tex")
  ans <- pt_cat_long(
  pmt_first,
  cols = "SEXf,RFf,FORMf", by = "STUDYf"
) %>% as_stable()

  expect_identical(expect, as.character(ans))
})

test_that("cat-wide-by-panel", {
  expect <- read_tex("cat-wide-by-panel.tex")
  ans <- pt_cat_wide(
  pmt_first,
  cols = "SEXf,RFf", by = "FORMf", panel = "STUDYf"
) %>% as_stable()

  expect_identical(expect, as.character(ans))
})

test_that("inventory-by", {
  expect <- read_tex("inventory-by.tex")
  ans <- pt_data_inventory(
  pmt_pk,
  by = "STUDYf"
) %>% as_stable()

  expect_identical(expect, as.character(ans))
})

test_that("inventory-panel-by", {
  expect <- read_tex("inventory-panel-by.tex")
  ans <- pt_data_inventory(
  pmt_pk,
  by = "STUDYf",
  panel = "FORMf"
) %>% as_stable()
  expect_identical(expect, as.character(ans))
})


test_that("inventory-stacked", {
  expect <- read_tex("inventory-stacked.tex")
  ans <- pt_data_inventory(
  pmt_obs,
  by = "STUDYf",
  panel = "SEQf",
  stacked = TRUE
) %>% as_stable()
  expect_identical(expect, as.character(ans))
})




