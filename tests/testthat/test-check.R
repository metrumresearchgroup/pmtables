
context("test-check")

test_that("continuous [PMT-TEST-0007]", {
  data <- data.frame(a = 1L, aa = 1.5, b = 'b')
  expect_true(pmtables:::check_continuous(data, "a"))
  expect_true(pmtables:::check_continuous(data, "aa"))
  expect_error(pmtables:::check_continuous(data,"b"))
  expect_error(pmtables:::check_continuous(data,"c"))
})

test_that("discrete [PMT-TEST-0008]", {
  data <- data.frame(a = 1L, as = 0.5, b = 'b', d = factor(11))
  expect_true(pmtables:::check_discrete(data, "a"))
  expect_true(pmtables:::check_discrete(data,"b"))
  expect_error(pmtables:::check_discrete(data,"c"))
})

test_that("check exists [PMT-TEST-0009]", {
  data <- data.frame(A = 1, B = 2, C = 3)
  expect_true(pmtables:::check_exists(data, c("B", "C", "A")))
  expect_error(
    pmtables:::check_exists(data, c("B", "Z", "D")),
    "there were problems with input data"
  )
})

