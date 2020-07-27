

#' Adjustments for row padding, column padding, and font size
#'
#'
#' @param row relative increase or decrease spacing between rows; use
#' `row_space > <default>` to increase; ; see also [st_space()]
#' @param col absolute column spacing amount (`pt`); see also [st_space()]
#' @param font for the table (e.g. `normalsize`, `small`, `scriptsize`, etc)
#'
#' @export
tab_size <- function(row = 1.4, col = 5, font = NULL) {

  assert_that(is.numeric(row))
  assert_that(is.numeric(col))
  assert_that(is.null(font) || is.character(font))

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

  list(
    row_space = row,
    col_space = col,
    font_size = font_size,
    col_row_sp = col_row_sp
  )

}
