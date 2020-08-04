
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
#' @examples
#'
#' \dontrun{
#'   library(dplyr)
#'   ptdata()  %>% stable() %>% st2doc()
#' }
#'
#' @export
st2doc <- function(text, preview = TRUE, output_dir = tempdir(), # nocov start
                   output_file = "st2doc.pdf") {
  assert_that(requireNamespace("rmarkdown"))
  assert_that(requireNamespace("fs"))
  file <- system.file("rmd", "st2doc.Rmd", package = "pmtables")

  if(is.list(text)) {
    tab <- map(text, .f = function(this_table) {
      this_table <- as.character(this_table)
      if(!any(grepl("begin{table}", this_table, fixed = TRUE))) {
        this_table <- pt_wrap(as.character(this_table))
      }
      c(this_table, "\\clearpage")
    })
    tab <- flatten_chr(tab)
  } else {
    tab <- as.character(text)
    if(!any(grepl("begin{table}", tab, fixed = TRUE))) {
      tab <- pt_wrap(as.character(tab))
    }
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
} # nocov end

#' Preview an stable object
#'
#' @param x an stable object
#' @param ... passed to [texPreview::tex_preview()]
#'
#' @examples
#'
#' \dontrun{
#' ptdata() %>% stable() %>% st_preview()
#' }
#' @export
st_preview <- function(x,...) { # nocov start
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
} # nocov end

