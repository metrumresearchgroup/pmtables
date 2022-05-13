
context("test-utils")

test_that("Update_List [PMT-TEST-0240]", {
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

test_that("digit1 [PMT-TEST-0241]", {
  expect_equal(digit1(1.2345), "1.2")
  expect_equal(digit1(100202.2345), "100202.2")
})

test_that("rnd is a very simple wrapper for round [PMT-TEST-0242]", {
  x <- rnorm(1000)
  a <- round(x, 5)
  b <- rnd(x, 5, foo = "bar")
  expect_identical(a,b)
})

test_that("check if regular expression is valid [PMT-TEST-0243]", {
  expect_true(pmtables:::is_regex("^abc$"))
  expect_false(pmtables:::is_regex("\\textbf{foo}"))
  expect_true(pmtables:::is_str_regex(fixed("\\textbf{foo}")))
  x <- pmtables:::as_str_regex("\\textbf{foo}")
  expect_is(x, "fixed")
  x <- pmtables:::as_str_regex(NULL)
  expect_is(x, "fixed")
  expect_match(x, "invalid-regex-")
})

test_that("repattern data frame [PMT-TEST-0244]", {
  a <- data.frame(a = 1, b = 2, c = 3)
  b <- data.frame(a = 4, d = 5, z = 5, c = 4)
  c <- data.frame(j = 5, m = 2)
  d <- data.frame(c = 100)
  x1 <- pmtables:::repattern_df(data = a, pattern = b)
  expect_identical(names(x1), names(b))
  x2 <- pmtables:::repattern_df(data = c, pattern = b)
  expect_equal(nrow(x2),0)
  x3 <- pmtables:::repattern_df(data = d, pattern = b)
  expect_identical(names(x3), names(b))
  x4 <- pmtables:::repattern_df(data = data.frame(), pattern = b)
  expect_equal(nrow(x4),0)
})


test_that("add parens to vector if not there [PMT-TEST-0245]", {
  a <- letters[1:3]
  ans <- ensure_parens(a)
  expect_equal(ans, c("(a)", "(b)", "(c)"))

  b <- a
  b[2] <- "(b)"
  ans <- ensure_parens(b)
  expect_equal(ans, c("(a)", "(b)", "(c)"))

  c <- b
  c[3] <- "(c"
  ans <- ensure_parens(c)
  expect_equal(ans, c("(a)", "(b)", "(c"))

  aa <- as.list(a)
  ans <- ensure_parens(aa)
  expect_equal(ans, list("(a)", "(b)", "(c)"))

  bb <- as.list(b)
  ans <- ensure_parens(bb)
  expect_equal(ans, list("(a)", "(b)", "(c)"))

  cc <- as.list(c)
  ans <- ensure_parens(cc)
  expect_equal(ans, list("(a)", "(b)", "(c"))

  d <- seq(3)
  ans <- ensure_parens(d)
  expect_equal(ans, c("(1)", "(2)", "(3)"))
})

test_that("sig returns character when passed int [PMT-UTIL-0001]", {
  expect_is(sig(1L), "character")
})
