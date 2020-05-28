
context("test-check")

test_that("continuous", {
  data <- data.frame(a = 1L, aa = 1.5, b = 'b')
  expect_true(pmtables:::check_continuous(data, "a"))
  expect_true(pmtables:::check_continuous(data, "aa"))
  expect_error(pmtables:::check_continuous(data,"b"))
  expect_error(pmtables:::check_continuous(data,"c"))
})

test_that("discrete", {
  data <- data.frame(a = 1L, as = 0.5, b = 'b', d = factor(11))
  expect_true(pmtables:::check_discrete(data, "a"))
  expect_true(pmtables:::check_discrete(data,"b"))
  expect_error(pmtables:::check_discrete(data,"c"))
})

