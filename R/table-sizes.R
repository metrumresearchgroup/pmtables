

#' Adjustments for row padding, column padding, and font size
#'
#'
#' @param row relative increase or decrease spacing between rows; use
#' `row_space > <default>` to increase; ; see also [st_space()]
#' @param col absolute column spacing amount (`pt`); see also [st_space()]
#' @param font for the table (e.g. `normalsize`, `small`, `scriptsize`, etc)
#' @param header_row extra space between rows in the header
#' section of the table (column names and units); this should be a negative
#' number if you wish to compress header rows; the default is `-0.4*row`;
#' recommend fixing to `-0.2` when `row` is 1 and smaller if `row` is less
#' than 1
#' @param lt_row extra row space for longtables; this is only needed for
#' extra control over row height; this can be a positive or negative
#' number and a value of 0 indicates to neither add or subtract row space
#'
#' @export
tab_size <- function(row = 1.3, col = 5, font = NULL,
                     header_row = -0.4*row, lt_row = 0.0) {

  assert_that(is.numeric(row))
  assert_that(is.numeric(col))
  assert_that(is.null(font) || is.character(font))
  assert_that(is.numeric(lt_row))
  assert_that(is.numeric(header_row))

  # Column and row stretch --------------
  col_row_sp <- list()
  col_row_sp$start <- "{\\def\\arraystretch{<row>}\\tabcolsep=<col>pt"
  col_row_sp$end <- "}"
  col_row_sp$start <- gluet(col_row_sp$start)

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
    col_row_sp = col_row_sp,
    header_row = header_row
  )

  structure(ans, class = "from_tab_sizes")
}
