
context("test-digits")

test_that("new digit object", {
  x <- new_digits(.fun = pmtables:::sig)
  expect_is(x,"digits")
  expect_is(pmtables:::get_digits_fun(x), "function")
  expect_is(pmtables:::get_digits_list(x), "list")
})

test_that("new digit object, with defaults", {
  x <- new_digits(.fun = pmtables:::sig, A = 1, B = 2)
  expect_is(x,"digits")
  expect_equal(x$.default,3)
  expect_is(x$.digits, "list")
  expect_equal(names(x$.digits), c("A", "B"))
  expect_equal(x$.digits$A,1)
  expect_equal(x$.digits$B,2)
})

test_that("update digit object", {
  x <- new_digits(.fun = pmtables:::sig, A = 1, B = 1, C = 1)
  x <- pmtables:::update_digits(x, cols = c("B", "Z", "A"))
  data <- pmtables:::get_digits_list(x)
  expect_is(data,"list")
  expect_named(data)
  expect_equal(names(data), c("B", "Z", "A"))
  expect_equal(data$B,1)
  expect_equal(data$A,1)
  expect_equal(data$Z,x$.default)
})
