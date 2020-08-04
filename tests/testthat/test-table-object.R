
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

test_that("stobject equivalent hline", {
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

test_that("stobject equivalent hline", {
  mt <- mtcars[1:20,]
  x <- inspect(mt, col_bold = TRUE)
  y <- st_new(mt, col_bold = TRUE) %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_identical(x$cols_tex, y$cols_tex)
})

test_that("stobject equivalent panel", {
  mt <- arrange(mtcars[1:20,], cyl)
  x <- inspect(mt, panel = "cyl")
  y <- st_new(mt) %>%
    st_panel("cyl") %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_identical(x$cols_tex, y$cols_tex)
})

test_that("stobject equivanelt sumrow", {
  file <- system.file("datasets", "with-total.RDS", package = "pmtables")
  data <- readRDS(file)
  sumr <- sumrow(rows = data$STUDY=="all", bold = TRUE, label = "All data")
  out1 <- inspect(data, sumrows = sumr)
  out2 <- st_new(data) %>%
    st_sumrow(rows = data$STUDY=="all", bold = TRUE, label = "All data") %>%
    inspect2()
  expect_identical(out1,out2)
})

test_that("stobject equivalent span", {
  data <- tibble(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6)
  sp <- list(
    colgroup("first", a:c),
    colgroup("second", e:f, level = 2)
  )
  x <- inspect(data, span = sp)
  y <- st_new(data) %>% st_span("first", a:c)
  y <- y %>% st_span("second", e:f)
  y <- st_make(y, inspect = TRUE) %>% get_stable_data()
})

test_that("stobject equivalent files", {
  mt <- arrange(mtcars[1:20,], cyl)
  x <- inspect(mt, r_file = "a.R", output_file = "b.tex")
  y <- st_new(mt) %>%
    st_files(r = "a.R", output = "t.tex") %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_identical(x$cols_tex, y$cols_tex)

})

test_that("stobject equivalent drop", {
  mt <- mtcars[1:3,]
  x <- inspect(mt, drop = "cyl")
  y <- st_new(mt) %>% st_drop(cyl) %>%
    inspect2()
  expect_identical(x$output, y$output)
})

test_that("stobject equivalent align", {
  mt <- mtcars[1:3,]
  ali <- cols_center(cyl = 'r', hp = 'l', qsec = col_ragged(3))
  x <- inspect(mt, align = ali)
  y <-
    st_new(mt) %>%
    st_center(cyl = 'r', hp = 'l', qsec = col_ragged(3)) %>%
    inspect2()
  expect_identical(x$output, y$output)
})

test_that("stobject equivalent notes", {
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

test_that("stobject equivalent rename", {
  mt <- mtcars[1:3,]
  notes <- letters[1:3]
  x <- inspect(
    mt,
    col_rename = c(cylinder = "cyl", "miles per gallon" = "mpg")
  )
  y <-
    st_new(mt) %>%
    st_rename(cylinder = cyl, `miles per gallon` = mpg) %>%
    inspect2()
  expect_identical(x$output, y$output)
})

test_that("stobject equivalent blank", {
  mt <- mtcars[1:3,]
  notes <- letters[1:3]
  x <- inspect(
    mt,
    col_blank = "qsec,drat,carb,foo"
  )
  y <-
    st_new(mt) %>%
    st_blank(qsec, drat, carb, foo) %>%
    inspect2()
  expect_identical(x$output, y$output)
})

test_that("tab edit", {
  data <- tibble(a = c(1,2,3), b = c(4,2,6))
  ans <- tab_edit(data, "2", "222")
  expect_equal(ans$a, c("1", "222", "3"))
  expect_equal(ans$b, c("4", "222", "6"))
  ans <- tab_edit(data, "2", "222", cols = 'b')
  expect_equal(ans$a, c(1, 2, 3))
  expect_equal(ans$b, c("4", "222", "6"))
})