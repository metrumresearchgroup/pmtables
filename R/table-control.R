
#' Table control parameters
#'
#' @param row_space multiplier to increase or decrease padding between
#' table rows; for example, `row_space = 1.4` means increase by 40%
#' @param col_space padding between columns; unit is `pt`
#' @param lt_row_space **extra** space between rows of longtable;
#' unit is `em`
#' @param fontsize font size
#' @param header_row amount of space that will be **removed** between
#' rows of the table header; unit is `em`; this must be a positive
#' number
#' @param note_type `tpt` puts notes in the third part of `threeparttable` and
#' `minipage` puts notes in a minipage below the table
#' @param note_hline where to place hlines when `minipage` notes are used
#' @param note_hline_pt pointsize for `minipage` note `hline`
#' @param note_table_skip vertical space between the bottom of the table
#' and the start of `minipage` notes; unit is `cm`
#' @param note_skip vertical space after the top `hline` where the `minipage`
#' notes start
#' @param note_sanitize if `TRUE`, notes will be sanitized
#' @param note_escape a vector of characters to sanitize
#' @param ... used only in the generic
#'
#'
#' @export
st_control <- function(...) UseMethod("st_control")
#' @rdname st_control
#' @export
st_control.default <- function(row_space = 1.4,
                               col_space = 5,
                               lt_row_space = 0.3,
                               fontsize = "normalsize",
                               header_row = 0.56,
                               note_type = c("tpt", "minipage"),
                               note_hline = c("top", "bottom", "both", "none"),
                               note_hline_pt = 0.4,
                               note_table_skip = 0.67,
                               note_skip = 0.02,
                               note_sanitize = TRUE,
                               note_escape = "_" ,...) {
  ans <- list(
    row_space = row_space,
    col_space = col_space,
    lt_row_space = lt_row_space,
    fontsize = fontsize,
    header_row_space = 0.56,
    note_type = match.arg(note_type),
    note_hline = match.arg(note_hline),
    note_table_skip = note_table_skip,
    note_skip = note_skip,
    note_hline_pt = note_hline_pt,
    note_sanitize = isTRUE(note_sanitize),
    note_escape = note_escape
  )
  structure(ans, class = "st_control")
}
is.st_control <- function(x) inherits(x, "st_control")

#' @rdname st_control
#' @param x an stobject
#' @export
st_control.stobject <- function(x,...) {
  if(!is.st_control(x$control)) {
    x$control <- st_control(...)
    return(x)
  }
  x$control <- Update_List(x$control, list(...))
  x <- structure(x, class = "st_control")
  x
}
