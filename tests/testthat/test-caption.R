library(testthat)
library(dplyr)

context("test-caption")

test_that("create caption", {
  cap <- as.caption("[Short] Main", write = TRUE)
  expect_is(cap, "st_caption")
  expect_true(pmtables:::is.st_caption(cap))
  att <- attributes(cap)
  expect_named(att)
  expect_identical(names(att), c("short", "write", "class"))
  expect_true(att$write)
  expect_equal(att$short, "Short")
  expect_equal(as.character(cap), "Short Main")
})

test_that("caption parse", {
  cap <- "[Short title]. Main caption text"
  ans <- pmtables:::parse_caption(cap)
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_identical(ans$short, "Short title")
  expect_identical(ans$main, "Short title. Main caption text")

  # Punctuation stays with the short title
  cap <- "[Short title.] Main caption text"
  ans <- pmtables:::parse_caption(cap)
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_identical(ans$short, "Short title.")
  expect_identical(ans$main, "Short title. Main caption text")

  # Don't repeat short
  cap <- "[Short title] Main caption text"
  ans <- pmtables:::parse_caption(cap, short_repeat = FALSE)
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_identical(ans$short, "Short title")
  expect_identical(ans$main, "Main caption text")

  # Custom separator
  cap <- "[Short title] Main caption text"
  ans <- pmtables:::parse_caption(cap, short_sep = ";")
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_identical(ans$short, "Short title")
  expect_identical(ans$main, "Short title; Main caption text")

  # Handle multiple brackets
  cap <- "[Short Title]. Main caption text with [this] in brackets."
  ans <- pmtables:::parse_caption(cap)
  expect_identical(ans$short, "Short Title")
  expect_identical(ans$main, "Short Title. Main caption text with [this] in brackets.")
})

test_that("caption passed into stable()", {
  cap <- "Table caption \\label{tab:one}"
  text <- stable(stdata(), caption = cap)
  expect_true(pmtables:::has.st_caption(text))
  text <- pmtables:::cap_main(text)
  expect_identical(cap, as.character(text))
})

test_that("caption passed into stable_long()", {
  cap <- "Table caption \\label{tab:one}"
  text <- stable_long(stdata(), caption = cap)
  expect_true(pmtables:::has.st_caption(text))
  text <- pmtables:::cap_main(text)
  expect_identical(cap, as.character(text))
  expect_match(text, cap, fixed = TRUE, all = FALSE)
})

test_that("caption piped into stable()", {
  cap <- as.caption("Table caption")
  tab1 <- st_new(stdata())
  tab1 <- st_caption(tab1, cap)
  text1 <- stable(tab1)
  text2 <- stable(stdata(), caption = cap)
  expect_identical(text1, text2)
})

test_that("caption piped into stable_long()", {
  cap <- as.caption("Table caption")
  tab1 <- st_new(stdata())
  tab1 <- st_caption(tab1, cap)
  text1 <- stable_long(tab1)
  text2 <- stable_long(stdata(), caption = cap)
  expect_identical(text1, text2)
})

test_that("short parsed from caption text", {
  cap <- "[Short title]. Long title"
  tab <- st_new(stdata())
  tab <- st_caption(tab, cap)
  text <- stable(tab)
  expect_true(pmtables:::has.st_caption(text))
  result <- pmtables:::cap_main(text)
  expect_is(result, "character")
  expect_equivalent(as.character(result), "Short title. Long title")
  expect_equivalent(pmtables:::cap_short(text), "Short title")
})

test_that("short parsed from caption text in long table", {
  cap <- "[Short title]. Table caption."
  tab <- st_new(stdata())
  tab <- st_caption(tab, cap)
  text <- stable_long(tab)
  expect_match(
    text,
    "[Short title]{Short title. Table caption.}",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("short passed in separately", {
  cap <- "Table caption."
  tab <- st_new(stdata())
  tab <- st_caption(tab, cap, short = "Foo. ")
  text <- stable_long(tab)
  expect_match(
    text,
    "[Foo.]{Foo. Table caption.}",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("caption on stable gets passed into preview", {
  cap <- "[short] Table caption."
  tab <- st_new(stdata())
  tab <- st_caption(tab, cap, write = TRUE)
  text <- stable(tab)
  st2report(text, dry_run = TRUE, stem = "test1")
  tables <- readLines(file.path(tempdir(), "test1-tables.tex"))
  expect_match(
    tables,
    "caption[short]{short Table caption.}",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("caption on stable_long gets passed into preview", {
  cap <- "[long] Table caption."
  tab <- st_new(stdata())
  tab <- st_caption(tab, cap)
  text <- stable_long(tab)
  st2report(text, dry_run = TRUE, stem = "test2")
  tables <- readLines(file.path(tempdir(), "test2-tables.tex"))
  expect_match(
    tables,
    "caption[long]{long Table caption.}",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("confirm original preview behavior - no info", {
  a <- stable(stdata())
  b <- stable(stdata())
  foo <- st2report(list(a,b), stem = "st2r-previous-a", dry_run = TRUE)
  ans <- readLines(file.path(tempdir(), "st2r-previous-a-tables.tex"))
  ans <- ans[grep("caption", ans)]
  expect_length(ans, 2)
  expect_match(ans[1], "[pmtables output preview - 1]", fixed = TRUE)
  expect_match(ans[2], "[pmtables output preview - 2]", fixed = TRUE)
  expect_match(ans, "{Lorem ipsum", fixed = TRUE)
})

test_that("confirm original preview behavior - names on list become short", {
  # Every caption is Lorum ipsum
  a <- stable(stdata())
  b <- stable(stdata())
  foo <- st2report(list(foo=a,bar=b), stem = "st2r-previous-a", dry_run = TRUE)
  ans <- readLines(file.path(tempdir(), "st2r-previous-a-tables.tex"))
  ans <- ans[grep("caption", ans)]
  expect_length(ans, 2)
  expect_match(ans[1], "[foo]", fixed = TRUE)
  expect_match(ans[2], "[bar]", fixed = TRUE)
  expect_match(ans, "{Lorem ipsum", fixed = TRUE)
})

test_that("caption appears in st_asis output", {
  cap <- "[short] Table caption."
  tab <- st_new(stdata())
  tab <- st_caption(tab, cap)
  text <- stable(tab)
  out <- st_asis(text)
  expect_match(
    out,
    "caption[short]{short Table caption.}",
    fixed = TRUE,
    all = FALSE
  )
})
