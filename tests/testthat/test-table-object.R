
library(testthat)
library(pmtables)
library(dplyr)

context("test-table-object.R")

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

inspect2 <- function(x) {
  x %>% st_make(inspect=TRUE) %>% get_stable_data()
}

test_that("stobject equivalent hline [PMT-TEST-0215]", {
  mt <- mtcars[1:20,]
  x <- inspect(mt, hline_at = c(1,5,8))
  y <- st_new(mt) %>% st_hline(at = c(1,5,8)) %>%
    st_make(inspect = TRUE) %>% get_stable_data()
  expect_identical(x$tab, y$tab)
  mt <- arrange(mt,cyl)
  x <- inspect(mt, hline_from = "cyl")
  y <- st_new(mt) %>% st_hline(from = "cyl") %>%
    st_make(inspect = TRUE) %>%
    get_stable_data()
  expect_identical(x$tab, y$tab)
})

test_that("hine re [PMT-TEST-0216]", {
  data <- data.frame(
    Study = c("A", "B", "C", "All Studies"),
    Result = c(1,2,3,123),
    Letter = letters[1:4]
  )
  x <-
    st_new(data) %>%
    st_hline(pattern = "All Studies") %>%
    inspect2()
  where <- grep("\\hline", x$tab, fixed = TRUE)
  expect_identical(where, nrow(data)-1L)
})

test_that("stobject equivalent cols_bold [PMT-TEST-0217]", {
  mt <- mtcars[1:20,]
  x <- inspect(mt, cols_bold = TRUE)
  y <- st_new(mt, cols_bold = TRUE) %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_identical(x$cols_tex, y$cols_tex)
})

test_that("stobject equivalent panel [PMT-TEST-0218]", {
  mt <- arrange(mtcars[1:20,], cyl)
  x <- inspect(mt, panel = "cyl")
  y <- st_new(mt) %>%
    st_panel("cyl") %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_identical(x$cols_tex, y$cols_tex)
})

test_that("stobject equivalent sumrow [PMT-TEST-0219]", {
  file <- system.file("datasets", "with-total.RDS", package = "pmtables")
  data <- readRDS(file)
  sumr <- sumrow(rows = data$STUDY=="all", bold = TRUE, label = "All data")
  out1 <- inspect(data, sumrows = sumr)
  out2 <- st_new(data) %>%
    st_sumrow(rows = data$STUDY=="all", bold = TRUE, label = "All data") %>%
    inspect2()
  expect_identical(out1,out2)
})

test_that("stobject equivalent span [PMT-TEST-0220]", {
  data <- tibble(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6)
  sp <- list(
    colgroup("first", a:c),
    colgroup("second", e:f, level = 2)
  )
  x <- inspect(data, span = sp)
  y <- st_new(data) %>% st_span("first", a:c)
  y <- y %>% st_span("second", e:f, level = 2)
  y <- st_make(y, inspect = TRUE) %>% get_stable_data()
  expect_identical(x,y)
})

test_that("stobject equivalent files [PMT-TEST-0221]", {
  mt <- arrange(mtcars[1:20,], cyl)
  x <- inspect(mt, r_file = "a.R", output_file = "b.tex")
  y <- st_new(mt) %>%
    st_files(r = "a.R", output = "t.tex") %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_identical(x$cols_tex, y$cols_tex)

})

test_that("stobject equivalent drop [PMT-TEST-0222]", {
  mt <- mtcars[1:3,]
  x <- inspect(mt, drop = "cyl")
  y <- st_new(mt) %>% st_drop(cyl) %>%
    inspect2()
  expect_identical(x$output, y$output)
})

test_that("stobject equivalent align [PMT-TEST-0223]", {
  mt <- mtcars[1:3,]
  ali <- cols_center(cyl = 'r', hp = 'l', qsec = col_ragged(3))
  x <- inspect(mt, align = ali)
  y <-
    st_new(mt) %>%
    st_center(cyl = 'r', hp = 'l', qsec = col_ragged(3)) %>%
    inspect2()
  expect_identical(x$output, y$output)
})

test_that("stobject equivalent notes [PMT-TEST-0224]", {
  mt <- mtcars[1:3,]
  notes <- letters[1:3]
  x <- inspect(
    mt, notes = notes,
    r_file = "foo.R", output_file = "foo.tex"
  )
  y <-
    st_new(mt) %>%
    st_notes(notes) %>%
    st_files(r = "foo.R", output= "foo.tex") %>%
    inspect2()
  expect_identical(x$output, y$output)

  notec <- noteconf(type = "minipage")
  a <- inspect(
    mt, notes = notes,
    r_file = "foo.R", output_file = "foo.tex",
    note_config = notec
  )
  b <-
    st_new(mt) %>%
    st_notes(notes) %>%
    st_noteconf(type = "minipage") %>%
    st_files(r = "foo.R", output= "foo.tex") %>%
    inspect2()
  expect_identical(a$output, b$output)
})

test_that("stobject equivalent rename [PMT-TEST-0225]", {
  mt <- mtcars[1:3,]
  notes <- letters[1:3]
  x <- inspect(
    mt,
    cols_rename = c(cylinder = "cyl", "miles per gallon" = "mpg")
  )
  y <-
    st_new(mt) %>%
    st_rename(cylinder = cyl, `miles per gallon` = mpg) %>%
    inspect2()
  expect_identical(x$output, y$output)
})

test_that("rename by named list [PMT-TEST-0226]", {
  mt <- mtcars[1:3,]

  y1 <- st_new(mt) %>%
    st_rename(cylinder = cyl) %>%
    inspect2()
  tst1 <- y1$cols_new
  expect_identical(tst1[4], "hp")
  expect_identical(tst1[2], "cylinder")

  y2 <- st_new(mt) %>%
    st_rename(.list = list(hp = "HorsePower")) %>%
    inspect2()

  tst2 <- y2$cols_new

  expect_identical(tst2[4], "HorsePower")
  expect_identical(tst2[2], "cyl")

  expect_warning(
    st_rename(st_new(mt),.list = list(HorsePower = "hp")),
    "rename data was passed as `.list`, but zero columns were matched"
  )

})

test_that("stobject equivalent blank [PMT-TEST-0227]", {
  mt <- mtcars[1:3,]
  notes <- letters[1:3]
  x <- inspect(
    mt,
    cols_blank = "qsec,drat,carb,foo"
  )
  y <-
    st_new(mt) %>%
    st_blank(qsec, drat, carb, foo) %>%
    inspect2()
  expect_identical(x$output, y$output)
})

test_that("stobject equivalent clear_reps [PMT-TEST-0228]", {
  data <- ptdata()
  x <- inspect(data, clear_reps = "FORM")
  y <- st_new(data) %>% st_clear_reps(FORM) %>% inspect2()
  expect_identical(x$output, y$output)
})

test_that("stobject equivalent clear_grouped_reps [PMT-TEST-0229]", {
  data <- ptdata()
  x <- inspect(data, clear_grouped_reps = "STUDY,DOSE")
  y <- st_new(data) %>% st_clear_grouped(STUDY,DOSE) %>% inspect2()
  expect_identical(x$output, y$output)
})

test_that("stobject equivalent longtable [PMT-TEST-0230]", {
  data <- ptdata()
  x <- stable_long(data)
  y <- st_new(data) %>% st_make(long=TRUE)
  expect_identical(x, y)
})

test_that("tab edit [PMT-TEST-0231]", {
  data <- tibble(a = c(1,2,3), b = c(4,2,6))
  ans <- tab_edit(data, "2", "222")
  expect_equal(ans$a, c("1", "222", "3"))
  expect_equal(ans$b, c("4", "222", "6"))
  ans <- tab_edit(data, "2", "222", cols = 'b')
  expect_equal(ans$a, c(1, 2, 3))
  expect_equal(ans$b, c("4", "222", "6"))
})

test_that("st_units [PMT-TEST-0232]", {
  data <- ptdata()
  x <- st_new(data) %>% st_units(WT = "kg", ALB = "foo")
  expect_identical(x$units, list(WT = "(kg)", ALB = "(foo)"))
  unit_list <- list(ALB = "foo")
  x <- st_new(data) %>% st_units(WT = "kg", unit_list)
  expect_identical(x$units, list(WT = "(kg)", ALB = "(foo)"))
})

test_that("st_bold and st_it [PMT-TEST-0233]", {
  data <- data.frame(A = 1, B =2 , C = 3)
  x <- st_new(data) %>% st_bold("B")
  expect_equal(x$data$B, "\\textbf{2}")
  x <- st_new(data) %>% st_it("A")
  expect_equal(x$data$A, "\\textit{1}")
})

test_that("st_args overwrites any arg [PMT-TEST-0234]", {
  x <- st_new(stdata())
  x <- st_notes(x, "the original note")
  y <- get_stable_data(stable(x, inspect = TRUE))
  expect_identical(y$notes, "the original note")
  x <- st_args(x, notes = "a new note")
  z <- get_stable_data(stable(x, inspect = TRUE))
  expect_identical(z$notes, "a new note")
})

test_that("arguments to st_args must be named [PMT-TEST-0235]", {
  x <- st_new(stdata())
  expect_error(
    st_args(x, "the original note"),
    regexp = "must be named"
  )
})
