#' Form table column names
#'
#' @inheritParams stable
#' @param cols the starting set of table column names
#' @param col_bold if `TRUE`, table column names are rendered with bold font
#' @param col_rename a `name = value` character vector to translate column names
#' to table names; ; see also [st_rename()]
#' @param col_blank a character vector of column names that will not be printed
#' in the table header; see also [st_blank()]
#' @param col_replace a character vector with the same length as the number of
#' output table columns; use this to completely replace the names (as opposed
#' to one by on editing with `col_rename`)
#' @param col_split a string that is used to split column labels into tag
#' (on the left) or name (on the right); if supplied, then `col_split` will be
#' used to remove the tag; for example, a column named `x.WT` would be renamed
#' `WT` if `col_split` was set to `.`
#' @param ... not used
#'
#' @export
tab_cols <- function(cols, col_replace = NULL, col_rename = NULL, col_blank = NULL,
                     col_split = NULL, col_bold = FALSE, units = NULL, ...) {
  cols0 <- cols

  # Work on columns and column names
  if(is.character(col_replace)) {
    if(length(col_replace) != length(cols)) {
      stop(
        "'col_replace' length is not equal to the number of columns in 'data'",
        call.=FALSE
      )
    }
    cols <- col_replace
    col_rename <- NULL
  }

  cols <- esc_underscore(cols)
  cols_new <- rename_cols(cols, relabel = col_rename, blank = col_blank)
  if(!is.null(col_split)) {
    split_cols <- str_split(cols_new, fixed(col_split), n = 2)
    cols_new <- map_chr(split_cols, last)
  }
  cols_tex <- form_tex_cols(cols_new, col_bold, units)
  ans <- list(tex = cols_tex, new = cols_new, cols = cols)
  structure(ans, class = "from_tab_cols")
}