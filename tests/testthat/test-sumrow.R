library(testthat)
library(pmtables)

inspect <- function(...) {
  get_stable_data(stable(..., inspect = TRUE))
}

context("test-sumrow")

test_that("summary row", {
  file <- system.file("datasets", "with-total.RDS", package = "pmtables")
  data <- readRDS(file)
  n <- nrow(data)
  sumr <- sumrow(rows = data$STUDY=="all", bold = TRUE, label = "All data")
  out <- inspect(data, sumrows = sumr )
  expect_match(out$tab[n], "textbf{All data}", fixed = TRUE)
  expect_match(out$tab[n-1], "\\hline", fixed = TRUE)
})
