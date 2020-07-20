
#' Render a table a pdf document
#'
#' @param text [stable()] output
#' @param preview if `TRUE`, the rendered pdf file is opened using
#' [fs::file_show()]
#' @param output_dir passed to [rmarkdown::render()]
#' @param output_file passed to [rmarkdown::render()]
#'
#' @return the `text` is returned invisibly
#'
#' @export
st2doc <- function(text, preview = TRUE, output_dir = tempdir(),
                   output_file = "st2doc.pdf") {
  assert_that(requireNamespace("rmarkdown"))
  assert_that(requireNamespace("fs"))
  file <- system.file("rmd", "st2doc.Rmd", package = "pmtables")
  tab <- as.character(text)
  if(!any(grepl("begin{table}", tab, fixed = TRUE))) {
    tab <- pt_wrap(as.character(tab))
  }
  ans <- rmarkdown::render(
    input = file,
    intermediates_dir = tempdir(),
    output_dir = output_dir,
    output_file = output_file,
    params = list(table_text = tab)
  )
  if(file.exists(ans) && isTRUE(preview)) {
    fs::file_show(ans)
  }
  return(invisible(text))
}

#' Preview an stable object
#'
#' @param x an stable object
#' @param ... passed to [texPreview::tex_preview()]
#'
#' @export
st_preview <- function(x,...) {
  assert_that(requireNamespace("texPreview"))
  if(length(x) > 1) {
    x <- paste0(x,collapse = "\n")
  }
  pk <- texPreview::build_usepackage(
    c("threeparttable", "array", "booktabs")
  )
  texPreview::tex_preview(
    x,
    usrPackages = pk,
    ...
  )
}

