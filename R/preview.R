
#' Render a table a pdf document
#'
#' @param text [stable()] output
#' @param preview if `TRUE`, the rendered pdf file is opened using
#' [fs::file_show()]
#'
#' @export
st2doc <- function(text, preview = TRUE) {
  assert_that(requireNamespace("rmarkdown"))
  assert_that(requireNamespace("fs"))
  file <- system.file("rmd", "st2doc.Rmd", package = "pmtables")
  ans <- rmarkdown::render(
    input = file,
    intermediates_dir=tempdir(),
    output_dir = tempdir(),
    output_file = "st2doc.pdf",
    params=list(table_text = as.character(text))
  )
  if(file.exists(ans) && isTRUE(preview)) {
    fs::file_show(ans)
  }
  return(invisible(ans))
}
