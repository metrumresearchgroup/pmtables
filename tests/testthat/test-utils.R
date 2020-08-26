
context("test-utils")

test_that("Update_List", {
  a <- list(a = 1, b = 2, c = 3)
  b <- list(c = 4, a = 3, z = 500)
  x <- pmtables:::Update_List(a,b)
  expect_is(x,"list")
  expect_equal(length(x),3)
  expect_identical(names(x),names(a))
  expect_equal(x$a,b$a)
  expect_equal(x$c,b$c)
  expect_equal(x$b,a$b)
})

test_that("digit1", {
  expect_equal(digit1(1.2345), "1.2")
  expect_equal(digit1(100202.2345), "100202.2")
})

test_that("check if regular expression is valid", {
  expect_true(pmtables:::is_regex("^abc$"))
  expect_false(pmtables:::is_regex("\\textbf{foo}"))
  expect_true(pmtables:::is_str_regex(fixed("\\textbf{foo}")))
  x <- pmtables:::as_str_regex("\\textbf{foo}")
  expect_is(x, "fixed")
  x <- pmtables:::as_str_regex(NULL)
  expect_is(x, "fixed")
  expect_match(x, "invalid-regex-")
})

test_that("repattern data frame", {
  a <- data.frame(a = 1, b = 2, c = 3)
  b <- data.frame(a = 4, d = 5, z = 5, c = 4)
  c <- data.frame(j = 5, m = 2)
  d <- data.frame(c = 100)
  x1 <- repattern_df(data = a, pattern = b)
  expect_identical(names(x1), names(b))
  x2 <- repattern_df(data = c, pattern = b)
  expect_equal(nrow(x2),0)
  x3 <- repattern_df(data = d, pattern = b)
  expect_identical(names(x3), names(b))
  x4 <- repattern_df(data = data.frame(), pattern = b)
  expect_equal(nrow(x4),0)
})
