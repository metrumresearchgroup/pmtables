#' Form table column names
#'
#' @param cols the starting set of table column names
#' @param cols_bold if `TRUE`, table column names are rendered with bold font
#' @param cols_rename a `name = value` character vector to translate column names
#' to table names; ; see also [st_rename()]
#' @param cols_blank a character vector of column names that will not be printed
#' in the table header; see also [st_blank()]
#' @param cols_replace a character vector with the same length as the number of
#' output table columns; use this to completely replace the names (as opposed
#' to one by on editing with `col_rename`)
#' @param cols_split a string that is used to split column labels into tag
#' (on the left) or name (on the right); if supplied, then `col_split` will be
#' used to remove the tag; for example, a column named `x.WT` would be renamed
#' `WT` if `cols_split` was set to `.`
#' @param cols_break character sequence to break column names into new lines
#' @param ... not used
#'
#' @export
tab_cols <- function(cols, cols_replace = NULL, cols_rename = NULL,
                     cols_blank = NULL, cols_split = NULL, cols_bold = FALSE,
                     cols_break = "...", ...) {
  cols0 <- cols

  # Work on columns and column names
  if(is.character(cols_replace)) {
    if(length(cols_replace) != length(cols)) {
      stop(
        "'col_replace' length is not equal to the number of columns in 'data'",
        call.=FALSE
      )
    }
    cols <- cols_replace
    cols_rename <- NULL
  }

  cols_new <- rename_cols(cols, relabel = cols_rename, blank = cols_blank)

  if(!is.null(cols_split)) {
    split_cols <- str_split(cols_new, fixed(cols_split), n = 2)
    cols_new <- map_chr(split_cols, last)
  }

  ans <- list(
    new = cols_new, cols = cols0, newline = cols_break,
    bold = cols_bold
  )

  structure(ans, class = "from_tab_cols")
}

header_matrix <- function(cols, cols_new, units = NULL, newline = "...",
                          bold = FALSE) {

  sp <- str_split(cols_new, fixed(newline))
  sp <- map(sp, trimws)
  nsplit <- map_int(sp,length)
  u <- header_matrix_unit(sp, cols, units)
  nunit <- !map_int(u, is.null)
  nrows <- max(nsplit+nunit)
  if(isTRUE(bold)) {
    sp <- map(sp, bold_each)
  }
  sp <- map2(sp, u, ~c(.x,.y))
  sp <- header_matrix_resize(sp, nrows)
  names(sp) <- paste0("V", seq_along(sp))
  sp <- bind_cols(sp)
  sp <- modify(sp, replace_na, "")
  sp
}

header_matrix_tex <- function(sp, sizes = tab_size()) {
  sp <- unname(split(sp, seq(nrow(sp))))
  sp <- map(sp, flatten_chr)
  esc <- getOption("pmtables.escape", c("_", "%"))
  sp <- map(sp, tab_escape, escape = esc)
  sp <- map_chr(sp, form_tex_cols)
  nr <- length(sp)
  header_space <- sizes$header_row
  pb <- paste0(" [", header_space, "em]")
  if(nr > 1) {
    w <- seq(nr-1)
    sp[w] <- paste0(sp[w], pb)
  }
  sp
}

header_matrix_resize <- function(sp, n) {
  if(n==1) return(sp)
  sp <- map(sp, rev)
  sp <- map(sp, .f=function(x) {
    length(x) <- n
    rev(x)
  })
  sp
}

header_matrix_unit <- function(sp, cols, units = NULL) {
  if(is.null(units)) return(vector("list", length(sp)))
  units <- units[names(units) %in% cols]
  units <- keep(units, ~nchar(.x) > 0)
  uni <- match(cols, names(units))
  unit <- vector("list", length(cols))
  unit[which(!is.na(uni))] <- units[uni[!is.na(uni)]]
  unit
}

rename_cols <- function(cols, relabel = NULL, blank = NULL) {

  if(!is.null(blank)) {
    blank <- unname(new_names(blank))
    bl <- cols %in% blank
    cols[bl] <- rep("", sum(bl))
  }

  if(!is.null(relabel)) {
    relabel <- new_names(relabel)
    relabel <- relabel[relabel %in% cols]
    cols_match <- match(cols, relabel)
    cols_target <- !is.na(cols_match)
    cols_replace <- cols_match[which(cols_target)]
    cols[cols_target] <- names(relabel)[cols_replace]
    cols <- unname(cols)
  }

  cols
}

form_tex_cols <- function(cols) {
  cols <- paste0(cols, collapse = " & ")
  cols <- paste0(cols, " \\\\")
  cols <- paste0("", cols)
  cols
}
