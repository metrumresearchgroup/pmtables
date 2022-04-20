library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-span")

test_that("span split [PMT-TEST-0186]", {
  data <- data.frame(a.A = 1, a.B = 2, C = 3, z.Y = 4, z.Z = 5)
  out <- inspect(data, span_split = colsplit(sep = '.'))
  expect_equal(out$cols_new, c("A", "B", "C", "Y", "Z"))
  expect_equal(out$span_data$span[[1]]$title, c("a", "a", "", "z", "z"))
})

test_that("span split with reversed title / col [PMT-TEST-0187]", {
  data1 <- data.frame(a.A = 1, a.B = 2, C = 3, z.Y = 4, z.Z = 5)
  data2 <- data.frame(A.a = 1, B.a = 2, C = 3, Y.z = 4, Z.z = 5)
  out1 <- inspect(data1, span_split = colsplit(sep = '.'))
  out2 <- inspect(data2, span_split = colsplit(sep = '.', title_side = ".r"))
  expect_identical(out1$output, out2$output)
  expect_equal(out2$cols_new, c("A", "B", "C", "Y", "Z"))
  expect_equal(out2$span_data$span[[1]]$title, c("a", "a", "", "z", "z"))
})

test_that("span split with title [PMT-TEST-0160]", {
  data <- data.frame(a.A = 1, a.B = 2, C = 3, z.Y = 4, z.Z = 5)
  title <- list(a = "First Split", z = "Last Split")
  out <- inspect(data, span_split = colsplit(sep = '.', title = title))
  expect_equal(out$cols_new, c("A", "B", "C", "Y", "Z"))
  utitle <- unique(out$span_data$span[[1]]$title)
  expect_equal(utitle, c("First Split", "", "Last Split"))
})

test_that("span from user [PMT-TEST-0189]", {
  out <- inspect(ptdata(), span = colgroup("from us_er",  FORM:WT))
  span <- out$span_data$span[[1]]
  expect_equal(nrow(span), ncol(ptdata()))
  expect_true(all(span$title[3:5] == "from us_er"))
})

test_that("span with breaks in title [PMT-TEST-0190]", {
  data <- ptdata()
  span <- colgroup("line 1 ~~~ line 2", WT:ALB)
  out <- inspect(data, span = span, span_title_break = "~~~")
  ans <- out$span_data$tex
  expect_length(ans, 3)
  sp <- strsplit(ans, " *& *")
  expect_match(sp[[1]][1], "multicolumn{4}{c}{}", fixed = TRUE)
  expect_match(sp[[2]][1], "multicolumn{4}{c}{}", fixed = TRUE)
  expect_match(sp[[1]][2], "multicolumn{4}{c}{line 1}", fixed = TRUE)
  expect_match(sp[[2]][2], "multicolumn{4}{c}{line 2}", fixed = TRUE)
  expect_match(sp[[3]], "cmidrule(lr)", fixed = TRUE)
})

test_that("names are not clobbered with span plus span_split [PMT-TEST-0191]", {
  data <- tibble(A = 2, B_A = 2, B_B = 3, B_C = 4)
  ans <- inspect(
    data, span_split = colsplit(sep = "_"),
    span = colgroup("foo", B_B:B_C, level = 2)
  )
  expect_equal(ans$span_data$cols, c("A", "A", "B", "C"))
  expect_equal(ans$span_data$span[[1]]$title, c("", "B", "B", "B"))
  expect_equal(ans$span_data$span[[2]]$title, c("", "", "foo", "foo"))
})

test_that("add span_split via st_span [PMT-TEST-0192]", {
  data <- tibble(A = 2, B_A = 2, B_B = 3, B_C = 4)
  x <- st_new(data) %>% st_span(sep = "_", split = TRUE) %>% inspect()
  expect_equal(x$span_data$cols, c("A", "A", "B", "C"))
  expect_equal(x$span_data$span[[1]]$title, c("", "B", "B", "B"))
  expect_error(
    st_new(data) %>% st_span_split(split = FALSE),
    regexp = "the `split` argument is FALSE"
  )
  x <- st_new(data) %>% st_span_split(sep = "_")
  expect_warning(
    st_span_split(x, sep = "."),
    regexp = "`span_split` is already set and will be replaced"
  )
})

test_that("align spanner - standard [PMT-TEST-0193]", {
  data <- stdata()
  ans1 <- stable(stdata(), span = colgroup("FOO", AGE:SCR))
  ans2 <- stable(stdata(), span = colgroup("FOO", AGE:SCR, align = 'l'))
  check <- which(grepl("FOO", ans1))
  expect_match(
    ans1[[check]],
    "multicolumn{3}{c}{FOO}",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    ans2[[check]],
    "multicolumn{3}{l}{FOO}",
    fixed = TRUE,
    all = FALSE
  )
  expect_error(
    stable(stdata(), span = colgroup("FOO", AGE:SCR, align = 'x')),
    regexp='should be one of '
  )
})

test_that("align spanner - multiple [PMT-TEST-0194]", {
  sp <- list(
    colgroup("LEFT", DOSE:FORM, align = 'l'),
    colgroup("CENTER", WT:CRCL),
    colgroup("RIGHT", AGE:SCR, align = 'r')
  )
  ans <- stable(stdata(), span = sp)
  spans <- ans[grepl("multicolumn", ans)]
  expect_match(
    spans,
    "multicolumn{2}{l}{LEFT}",
    fixed = TRUE
  )
  expect_match(
    spans,
    "multicolumn{2}{c}{CENTER}",
    fixed = TRUE
  )
  expect_match(
    spans,
    "multicolumn{3}{r}{RIGHT}",
    fixed = TRUE
  )
})

test_that("align spanner - via colsplit [PMT-TEST-0195]", {
  data <- rename(stdata(), AAA.CRCL= CRCL, AAA.WT = WT)
  ans1 <- stable(data, span_split = colsplit(sep = ".", align = 'r'))
  check <- which(grepl("AAA", ans1))
  expect_match(
    ans1[[check]],
    "multicolumn{2}{r}{AAA}",
    fixed = TRUE,
    all = FALSE
  )
})

