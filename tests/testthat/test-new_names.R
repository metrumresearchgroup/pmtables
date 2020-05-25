

context("test-new_names")

test_that("new names - character", {
  x <- pmtables:::new_names("a,b,c")
  expect_identical(unname(x),names(x))
  expect_is(x,"character")
  expect_length(x,3)
})

test_that("new names - quosure, unnamed", {
  x <- pmtables:::new_names(dplyr::vars(a,b,c))
  expect_identical(unname(x),names(x))
  expect_is(x,"character")
  expect_length(x,3)
})

test_that("new names - quosure, named", {
  x <- pmtables:::new_names(dplyr::vars(j = a, k = b, c))
  expect_is(x,"character")
  expect_length(x,3)
  expect_identical(unname(x), c("a", "b", "c"))
  expect_identical(names(x), c("j", "k", "c"))
})

