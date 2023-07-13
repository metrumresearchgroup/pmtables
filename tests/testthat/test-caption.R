library(testthat)
library(dplyr)

context("test-caption")

test_that("caption passed into stable()", {
  cap <- "Table caption \\label{tab:one}"
  text <- stable(stdata(), caption = cap)
  text <- attributes(text)$caption
  expect_identical(cap, text)
})

test_that("caption passed into stable_long()", {
  cap <- "Table caption \\label{tab:one}"
  text <- stable_long(stdata(), caption = cap)
  text <- attributes(text)$caption
  expect_identical(cap, text)
  expect_match(text, cap, fixed = TRUE, all = FALSE)
})

test_that("caption piped into stable()", {
  cap <- "Table caption"
  tab1 <- st_new(stdata())
  tab1 <- st_caption(tab1, cap)
  text1 <- stable(tab1)
  text2 <- stable(stdata(), caption = cap)
  expect_identical(text1, text2)
})

test_that("caption piped into stable_long()", {
  cap <- "Table caption"
  tab1 <- st_new(stdata())
  tab1 <- st_caption(tab1, cap)
  text1 <- stable_long(tab1)
  text2 <- stable_long(stdata(), caption = cap)
  expect_identical(text1, text2)
})

test_that("caption parse", {
  cap <- "[Short title]. Main caption text"
  ans <- pmtables:::parse_caption(cap)
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_identical(ans$short, "Short title")
  expect_identical(ans$text, "Short title. Main caption text")

  # Punctuation stays with the short title
  cap <- "[Short title.] Main caption text"
  ans <- pmtables:::parse_caption(cap)
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_identical(ans$short, "Short title.")
  expect_identical(ans$text, "Short title. Main caption text")

  # Don't repeat short
  cap <- "[Short title] Main caption text"
  ans <- pmtables:::parse_caption(cap, short_repeat = FALSE)
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_identical(ans$short, "Short title")
  expect_identical(ans$text, "Main caption text")

  # Custom separator
  cap <- "[Short title] Main caption text"
  ans <- pmtables:::parse_caption(cap, short_sep = ";")
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_identical(ans$short, "Short title")
  expect_identical(ans$text, "Short title; Main caption text")
})

test_that("short parsed from caption text", {
  cap <- "[Short title]. Long title"
  tab <- st_new(stdata())
  tab <- st_caption(tab, cap)
  text <- stable(tab)

  result <- attributes(text)$caption
  expect_is(result, "character")
  expect_equivalent(result, "Short title. Long title")
  expect_identical(attributes(result)$short, "Short title")

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
  tab <- st_caption(tab, cap)
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
