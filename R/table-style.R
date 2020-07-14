
#' @export
summaryrow <- function(rows, hline = TRUE, bold_cols = NULL, blank_cols = NULL,
                       labels = NULL) {
  if(is.logical(rows)) rows <- which(rows)
  ans <- list(
    rows = rows, hline = as.logical(hline), bold_cols = bold_cols,
    blank_cols = blank_cols, labels = labels, nrows = length(rows)
  )
  structure(ans, class = "summaryrow")
}
