
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

glofile <- system.file("glo", "glossary.tex", package = "pmtables")

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

test_that("call st_align functions multiple times in a pipeline", {
  a <- st_new(stdata())
  a <- st_align(a, WT = "r", .c = "FORM",
                .coltype = "m", .outer = "lr")
  expect_equal(a$align$update$WT, "r")
  expect_equal(a$align$update$FORM, "c")
  expect_null(a$align$update$AGE)
  expect_equal(a$align$coltype, "m")

  a <- st_right(a, AGE = "c", FORM = "l", .coltype = "p")

  expect_is(a, "stobject")
  expect_is(a$align, "aligncol")
  expect_equal(a$align$update$WT, "r")
  expect_equal(a$align$update$FORM, "l")
  expect_equal(a$align$update$AGE, "c")
  expect_equal(a$align$coltype, "p")
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

test_that("pipe pt_cat_wide to stobject", {
  x <- pt_cat_wide(
    pmt_first,
    cols = c("SEXf", "ASIANf")
  )
  a <- st_new(x) %>% st_notes("a,b,c") %>% stable()
  b <- stable(x, notes = c(x$notes, "a,b,c"))
  expect_identical(a, b)
})

test_that("pipe pt_cat_long to stobject", {
  x <- pt_cat_long(
    pmt_first,
    cols = c("SEXf", "ASIANf")
  )
  a <- st_new(x) %>% st_notes("a,b,c") %>% stable()
  b <- stable(x, notes = c(x$notes, "a,b,c"))
  expect_identical(a, b)
})

test_that("pipe pt_cont_wide to stobject", {
  x <- pt_cont_long(
    pmt_first,
    cols = c("AGE", "WT")
  )
  a <- st_new(x) %>% st_notes("a,b,c") %>% stable()
  b <- stable(x, notes = c(x$notes, "a,b,c"))
  expect_identical(a, b)
})

test_that("pipe pt_cont_long to stobject", {
  x <- pt_cont_wide(
    pmt_first,
    cols = c("AGE", "WT")
  )
  a <- st_new(x) %>% st_notes("a,b,c") %>% stable()
  b <- stable(x, notes = c(x$notes, "a,b,c"))
  expect_identical(a, b)
})

test_that("pipe pt_demographics to stobject", {
  x <- pt_demographics(
    pmt_first,
    cols_cont = c("AGE", "WT"),
    cols_cat = c("SEXf", "ASIANf")
  )
  a <- st_new(x) %>% st_notes("a,b,c") %>% stable()
  b <- stable(x, notes = c(x$notes, "a,b,c"))
  expect_identical(a, b)
})

test_that("call st_col_split() on pmtable", {
  expect_error(
    pt_cat_wide(pmt_first, cols = c("SEXf")) %>%
      st_new() %>% st_span_split(),
    regexp = "the st_span_split() function cannot be used",
    fixed = TRUE
  )
})

test_that("call st_panel() on pmtable", {
  expect_error(
    pt_cat_wide(pmt_first, cols = c("SEXf")) %>%
      st_new() %>% st_panel(),
    regexp = "the st_panel() function cannot be used",
    fixed = TRUE
  )
})

test_that("call st_units() on pmtable", {
  expect_error(
    pt_cat_wide(pmt_first, cols = c("SEXf")) %>%
      st_new() %>% st_units(),
    regexp = "the st_units() function cannot be used",
    fixed = TRUE
  )
})

test_that("remove notes from a st object", {
  x <- pt_data_inventory(pmt_obs)
  expect_true(is.character(x$notes))
  expect_true(length(x$notes) > 0)
  x <- st_new(x)
  x <- st_notes_rm(x)
  expect_null(x$notes)
})

test_that("append a note in a st object", {
  x <- pt_data_inventory(pmt_obs)
  x <- st_new(x)
  nn <- x$notes
  x <- st_notes_app(x, "more notes")
  mm <- x$notes
  l <- length(nn)
  ans <- paste0(nn[l], "; more notes")
  expect_equal(ans, mm[l])
})

test_that("collapse notes to a single string in st object", {
  x <- pt_data_inventory(pmt_obs)
  nn <- x$notes
  x <- st_new(x)
  x <- st_notes_str(x)
  mm <- x$notes
  ans <- paste0(nn, collapse = "; ")
  expect_identical(ans, mm)
})

test_that("detach the notes in a st object", {
  x <- st_new(pt_data_inventory(pmt_obs))
  x <- st_notes_detach(x, 0.95)
  conf <- x$note_config
  expect_equal(conf$width, 0.95)
  expect_equal(conf$type, "minipage")
})

test_that("substitute lines in table notes", {
  x <- pt_data_inventory(pmt_obs)
  not <- x$notes
  x <- st_new(x)

  a <- st_notes_sub(x, "^MISS", "missing stuff")
  expect_equal(a$notes[3], "missing stuff")

  b <- st_notes_sub(x, 2, "below ql")
  expect_equal(b$notes[2], "below ql")

  expect_warning(
    st_notes_sub(x, "kyle", "abc"),
    regexp = "did not find any matching notes"
  )

  expect_error(
    st_notes_sub(x, TRUE, "abc"),
    regexp = "must be either character or numeric"
  )

  expect_error(
    st_notes_sub(x, 100, "abc"),
    regexp = "not less than or equal to"
  )

  xx <- st_notes_rm(x)
  expect_warning(
    st_notes_sub(xx, "kyle", "abc"),
    regexp = "did not find any notes"
  )
})

test_that("get notes from glossary file with st_notes_glo", {
  glossary <- read_glossary(glofile)
  data <- stdata()

  x <- st_new(data)
  y <- st_notes_glo(x, glossary, WT, CLF, V2F)$notes
  expect_length(y, 1)
  expect_match(y, "subject weight", fixed = TRUE)
  expect_match(y, "CL/F", fixed = TRUE)

  x <- st_new(data)
  y <- st_notes_glo(x, glossary, WT, CLF, collapse = NULL)$notes
  expect_length(y, 2)

  x <- st_new(data)
  y <- st_notes_glo(x, glossary, WT, QD, sep = "@  ")$notes
  expect_length(y, 1)
  expect_match(y, "QD@  ")

  x <- st_new(data)
  y <- st_notes_glo(x, glossary, WT, CLF, labels = "SCR,V2F", collapse = NULL)$notes
  expect_length(y, 4)
  expect_match(y[[3]], "SCR: ")

  x <- st_new(data)
  y <- st_notes_glo(x, glossary, WT, CLF, width = 1)$note_config
  expect_identical(y$type, "minipage")
  expect_identical(y$width, 1)
})

test_that("st_filter filters data in pmtable object [PMT-STFUN-0001]", {
  data <- stdata()
  x <- st_new(data)
  y <- st_filter(x, FORM != "troche")
  expect_true(all(y$data$FORM %in% c("tablet", "capsule")))
  expect_true("troche" %in% data$FORM)
})

test_that("sumrows is valid field for st_new()", {
  ans <- st_new(
    pt_cont_long(
      pmt_first,
      cols = c("WT", "CRCL"),
      by  = "STUDYf"
    )
  )
  expect_is(ans, "stobject")
})

test_that("clone stable object", {
  x <- st_new(stdata())
  x <- st_panel(x, "FORM")
  x <- st_files(x, r = "test-table-object.R", output = "output.tex")
  y <- st_clone(x)
  expect_identical(ls(x), ls(y))
  for(i in ls(x)) {
    expect_identical(get(i, envir = x), get(i,envir = y))
  }
  expect_identical(attributes(x), attributes(y))
  # with cloning
  y$a <- 1
  expect_null(x$a)
  # without cloning
  z <- x
  z$b <- 2
  expect_equal(x$b, 2)
  # error to try to clone another object
  a <- new.env(parent = emptyenv())
  a$foo <- 1
  expect_error(st_clone(a))
})
