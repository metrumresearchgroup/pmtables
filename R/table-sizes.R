

#' Adjustments for row padding, column padding, and font size
#'
#'
#' @param row relative increase or decrease spacing between rows; use
#' `row_space > <default>` to increase; this parameter applies to both long tables
#' ans tabular tables; see also [st_space()]
#' @param col absolute column spacing amount (`pt`);  this parameter applies to both
#' long tables and tabular tables; see also [st_space()]
#' @param font for the table (e.g. `normalsize`, `small`, `scriptsize`, etc)
#' @param header_row extra (or less) space between rows in the header
#' section of the table (column names and units); this should be a negative
#' number if you wish to compress header rows; the default is `-0.4*row`;
#' recommend fixing to `-0.2` when `row` is 1 and smaller if `row` is less
#' than 1
#' @param span_title_row extra (or less) space betwen rows of the span titles
#' when they are broken across multiple lines; this is equivalent to
#' `header_row` (which applies to table column titles), but this applies to
#' spanner titles
#' @param lt_row extra row space for longtables; this is only needed for
#' extra control over row height; this can be a positive or negative
#' number and a value of 0 indicates to neither add or subtract row space
#'
#' @export
tab_size <- function(row = 1.3, col = 5, font = NULL,
                     header_row = -0.4*row,
                     span_title_row = header_row,
                     lt_row = 0.0) {

  assert_that(is.numeric(row))
  assert_that(is.numeric(col))
  assert_that(is.null(font) || is.character(font))
  assert_that(is.numeric(lt_row))
  assert_that(is.numeric(header_row))

  # Font size ----------------------------------
  font_size <- list()
  if(is.character(font)) {
    font_size$start <- paste0("{\\", font)
    font_size$end <- "}"
  }

  ans <- list(
    row_space = row,
    col_space = col,
    lt_row_space = lt_row,
    font_size = font_size,
    header_row = header_row,
    span_title_row = span_title_row
  )

  structure(ans, class = "from_tab_sizes")
}
