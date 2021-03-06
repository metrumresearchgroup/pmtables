
context("test-cat-data")

cols <- pmtables:::new_names(c("CPf", "SEXf", "RFf"))

test_that("discrete data summary long - simple", {
  data <- pmt_first
  ans <- cat_data(data, cols = cols)
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"name")
  expect_identical(names(ans)[2],"level")
  nr <- sum(
    length(unique(data[[cols[1]]])),
    length(unique(data[[cols[2]]])),
    length(unique(data[[cols[3]]]))
  )
  expect_equal(nrow(ans),nr)
  expect_equal(ncol(ans),3)
})

test_that("discrete data summary long - by", {
  data <- pmt_first
  ans <- cat_data(data, cols = cols, by = "STUDY")
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"name")
  expect_identical(names(ans)[2],"level")
  l <- sum(
    length(unique(data[[cols[1]]])),
    length(unique(data[[cols[2]]])),
    length(unique(data[[cols[3]]]))
  )
  expect_equal(nrow(ans),l)
  nby <- length(unique(data[["STUDY"]]))
  expect_equal(ncol(ans),nby + 2)
})

test_that("discrete data summary wide - simple", {
  data <- pmt_first
  ans <- cat_data(data, cols = cols, wide = TRUE)
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],".total")
  l <- sum(
    length(unique(data[[cols[1]]])),
    length(unique(data[[cols[2]]])),
    length(unique(data[[cols[3]]]))
  )
  expect_equal(nrow(ans),1)
  # add 1 for summary, 1 for N
  expect_equal(ncol(ans),l + 1 + 1)
})

test_that("discrete data summary wide - by", {
  data <- pmt_first
  ans <- cat_data(data, cols = cols, wide = TRUE, by = "STUDY")
  expect_is(ans,"data.frame")
  expect_identical(names(ans)[1],"STUDY")
  l <- sum(
    length(unique(data[[cols[1]]])),
    length(unique(data[[cols[2]]])),
    length(unique(data[[cols[3]]]))
  )
  expect_equal(nrow(ans),length(unique(data[["STUDY"]])))
  # add 1 for summary, 1 for N
  expect_equal(ncol(ans),l + 1 + 1)
})
