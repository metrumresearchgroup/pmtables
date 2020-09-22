library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-hline")

data <- tibble(
  a = c(1,2,3,1,2),
  b = c(4,5,6,1,2),
  c = c(7,8,9,1,2),
  d = c("a", "a", "b", "a", "b")
)

test_that("test-hline-hline-at", {
  x <- inspect(data = data, hline_at = c(2,4))
  ans <- grep("hline", x$tab)
  expect_equal(c(2,4)-1, ans)
})

test_that("test-hline-hline-from", {
  x <- inspect(data = data, hline_from = "d")
  ans <- grep("hline", x$tab)
  expect_equal(c(3,4,5)-1, ans)
  expect_error(inspect(data = data, hline_from = "kyle"))
})

test_that("test-hline st_hline pattern", {
  x <- st_new(stdata()) %>% st_hline(pattern = "cap", col = "FORM") %>% st_make()
  y <- st_new(stdata()) %>% st_hline(pattern = "cap") %>% st_make()
  expect_identical(x,y)
})

test_that("test-hline st_hline nudge", {
  data <- tibble(a = letters[1:10])
  x <- st_new(data) %>%
    st_hline(pattern = 'd') %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_match(x$tab[3], "hline")
  y <- st_new(data) %>%
    st_hline(at = data$a=='d') %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_identical(x,y)
  z <- st_new(data) %>%
    st_hline(pattern = 'd', nudge = 1) %>%
    st_make(inspect=TRUE) %>%
    get_stable_data()
  expect_match(z$tab[4], "hline")
  expect_false(grepl("hline", z$tab[3]))
})


