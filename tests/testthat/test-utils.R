
context("test-utils")

test_that("data", {
  expect_is(pmtables:::data("id"), "data.frame")
  expect_is(pmtables:::data("obs"), "data.frame")
  expect_is(pmtables:::data("all"), "data.frame")
})

test_that(".cols", {
  expect_is(.cols(a,b,c), "quosures")
})

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

