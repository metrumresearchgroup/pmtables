library(testthat)
library(pmtables)

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
  expect_true(pmtables:::is_str_regex(stringr::fixed("\\textbf{foo}")))
  x <- pmtables:::as_str_regex("\\textbf{foo}")
  expected <- if (utils::packageVersion("stringr") >= "1.5.0") {
    "stringr_fixed"
  } else {
    "fixed"
  }
  expect_is(x, expected)
  x <- pmtables:::as_str_regex(NULL)
  expect_is(x, expected)
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

test_that("set maxex via option", {
  a <- sig(123455666)
  expect_equal(a, "123000000")
  options(pmtables.maxex = 2)
  b <- sig(123455666)
  expect_equal(b, "1.23e+08")
  options(pmtables.maxex = NULL)
  c <- sig(123455666)
  expect_identical(a, c)
})

test_that("sig behavior - maxex", {
  a <- sig(123455666)
  expect_equal(a, "123000000")
  aa <- sig(123455666, maxex = 7)
  expect_equal(aa, "1.23e+08")
  ab <- sig(123455666, maxex = 8)
  expect_equal(ab, "1.23e+08")
  ac <- sig(123455666, maxex = 9)
  expect_equal(ac, "123000000")
})

test_that("sig behavior - zero", {
  a <- sig(0, digits = 3)
  expect_equal(a, "0.00")
  b <- sig(0, digits = 4)
  expect_equal(b, "0.000")
  d <- sig(as.double(0), digits = 3)
  expect_equal(d, a)
  e <- sig(0L)
  expect_equal(e, "0")
})

test_that("sig behavior - negative numbers", {
  a <- sig(-101, digits = 3)
  expect_equal(a, "-101")
  b <- sig(-1.23, digits = 3)
  expect_equal(b, "-1.23")
  c <- sig(-12342, digits = 3)
  expect_equal(c, "-12300")
})

test_that("sig behavior - big.mark", {
  a <- sig(1.2345, digits = 3, big.mark = ",")
  expect_equal(a, "1.23")

  b1 <- sig(12345, digits = 3)
  expect_equal(b1, "12300")
  b2 <- sig(12345, digits = 3, big.mark = ",")
  expect_equal(b2, "12,300")

  c1 <- sig(0.000012345, digits = 3)
  expect_equal(c1, "0.0000123")
  c2 <- sig(0.000012345, digits = 3, big.mark = ",")
  expect_equal(c1, c2)

})

test_that("sig big.mark handles vector", {
  x <- c(0.1, 1, 10, 100, 1000, 10000, 100000)
  a <- sig(x, big.mark = ",")
  expect_equal(a, c("0.100", "1.00", "10.0", "100", "1000", "10,000", "100,000"))
})

sig0 <- function(..., maxex = Inf) pmtables:::sig_legacy(..., maxex = maxex)
test_that("sig compare to legacy", {
  x <- 123
  expect_identical(sig(x), sig0(x))

  x <- 123.2556
  expect_identical(sig(x), sig0(x))

  x <- 12323423.2556
  expect_identical(sig(x), sig0(x))

  x <- 0.000012345
  expect_identical(sig(x), sig0(x))

  x <- c(0.00001, 1, 10, 100, 100000)
  expect_identical(sig(x), sig0(x))

  x <- c(0.00001, 1, 10, 100, 100000)
  expect_identical(sig(x, digits = 4), sig0(x, digits = 4))
  expect_identical(sig(x, digits = 2), sig0(x, digits = 2))

  expect_identical(
    sig(x,  digits = 4, maxex = 3),
    sig0(x, digits = 4, maxex = 3)
  )
  expect_identical(
    sig(x,  digits = 2, maxex = 3),
    sig0(x, digits = 2, maxex = 3)
  )
})

test_that("sig compare to legacy - large random tests", {
  set.seed(12323)
  x <- runif(1000, -3e4, 3e4)
  y <- runif(1000, -3, 3)
  z <- runif(1000, -0.001, 0.001)
  xyz <- c(x, y, z)

  a <- sig(xyz,  digits = 3)
  b <- sig0(xyz, digits = 3)
  expect_equal(a, b)

  a <- sig(xyz,  digits = 5)
  b <- sig0(xyz, digits = 5)
  expect_equal(a, b)

  a <- sig(xyz,  digits = 2)
  b <- sig0(xyz, digits = 2)
  expect_equal(a, b)

  a <- sig(xyz,  digits = 3, maxex = 4)
  b <- sig0(xyz, digits = 3, maxex = 4)
  expect_equal(a, b)

  a <- sig(xyz,  digits = 5, maxex = 4)
  b <- sig0(xyz, digits = 5, maxex = 4)
  expect_equal(a, b)

  a <- sig(xyz,  digits = 2, maxex = 4)
  b <- sig0(xyz, digits = 2, maxex = 4)
  expect_equal(a, b)
})
