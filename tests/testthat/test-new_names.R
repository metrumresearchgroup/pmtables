

context("test-new_names")

test_that("new names - character [PMT-TEST-0146]", {
  x <- pmtables:::new_names("a,b,c")
  expect_identical(unname(x),names(x))
  expect_is(x,"character")
  expect_length(x,3)
  x <- pmtables:::new_names(c("a", "b", "c"))
  expect_equal(x, c(a = "a", b = "b", c = "c"))
  x <- pmtables:::new_names(c("a", B = "b", "c", zz = "z"))
  expect_equal(x, c(a = "a", B = "b", c = "c", zz = "z"))
})

test_that("new names - quosure, unnamed [PMT-TEST-0147]", {
  x <- pmtables:::new_names(dplyr::vars(a,b,c))
  expect_identical(unname(x),names(x))
  expect_is(x,"character")
  expect_length(x,3)
})

test_that("new names - quosure, named [PMT-TEST-0148]", {
  x <- pmtables:::new_names(dplyr::vars(j = a, k = b, c))
  expect_is(x,"character")
  expect_length(x,3)
  expect_identical(unname(x), c("a", "b", "c"))
  expect_identical(names(x), c("j", "k", "c"))
})

test_that("new names - quosure, table [PMT-TEST-0149]", {
  table <- list(c = "d")
  x <- pmtables:::new_names(dplyr::vars(a,b,c),table)
  expect_identical(names(x),c("a", "b", "d"))
  expect_is(x,"character")
  expect_length(x,3)
})

test_that("new names - panel [PMT-TEST-0150]", {
  x <- rowpanel("a")
  x <- pmtables:::new_names(x)
  expect_identical(names(x),c("a"))
  expect_equivalent(x,"a")
  expect_is(x,"character")
  expect_length(x,1)
})

test_that("new names - list [PMT-TEST-0151]", {
  x <- list(a = "A", z = "Z")
  x <- pmtables:::new_names(x)
  expect_identical(names(x),c("a", "z"))
  expect_equivalent(x, c("A", "Z"))
  expect_is(x,"character")
  expect_length(x,2)
})

test_that("duplicated values is error [PMT-TEST-0152]", {
  expect_error(pmtables:::new_names("a,b,c,a"))
  expect_error(pmtables:::new_names(dplyr::vars(a,b,c,a)))
})

