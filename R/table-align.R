
#' Set table column alignment
#'
#' @param .default the default column alignment
#' @param ... exceptions to use from `default`; each argument should be named
#' by a column in the data set; values should be either "l", "c", or "r"
#' @param .rt column names as character vector or comma separated string
#' to right justify
#' @param .cent column names as character vector or comma separated string
#' to center
#' @param .lt column names as character vector or comma separated string
#' to right justify
#' @param .complete not used
#'
#' @export
cols_align <- function(.default  = c("l", "c", "r"), ...,
                       .rt = NULL, .cent = NULL, .lt = NULL,
                       .complete = NULL) {

  .default <- match.arg(.default)

  to_update <- list(...)
  to_update <- align_update(to_update, .rt,  "r")
  to_update <- align_update(to_update, .lt,  "l")
  to_update <- align_update(to_update, .cent,"c")

  ans <- list(
    complete = .complete,
    default = .default,
    update = to_update
  )
  structure(ans, class = "aligncol")
}

align_update <- function(to_update,cols,al) {
  if(is.null(cols)) return(to_update)
  cols <- cvec_cs(cols)
  for(col in cols) {
    to_update[[col]] <- al
  }
  to_update
}

#' @rdname cols_align
#' @export
cols_center <- function(...) {
  cols_align(.default = 'c', ..., .complete = NULL)
}

#' @rdname cols_align
#' @export
cols_left <- function(...) {
  cols_align(.default = 'l', ..., .complete = NULL)
}

#' @rdname cols_align
#' @export
cols_right <- function(...) {
  cols_align(.default = 'r', ..., .complete = NULL)
}

#' Format a table cell with fixed width
#'
#' @param size the width of the cell
#' @param unit to go with `size`
#' @param ragged use `right` to left justify and `left` to right justify
#' @param coltype column type
#'
#' @export
col_ragged <- function(size, unit = "cm", ragged = c("right","left"),
                       coltype = c("p","m","b")) {
  ragged <- match.arg(ragged)
  coltype <- match.arg(coltype)
  paste0(">{\\ragged",ragged,"}",coltype,"{",size,unit,"}")
}

form_align <- function(x,cols,pipes = FALSE) {
  if(is.character(x$complete)) return(x$complete)
  nc <- length(cols)
  ans <- rep(x$default, nc)
  replace <- match(names(x$update), cols)
  replace <- replace[!is.na(replace)]
  ans[replace] <- unlist(x$update,use.names=FALSE)
  if(isTRUE(pipes)) {
    where <- seq_along(ans)[-1]
    ans[where] <- paste0("|",ans[where])
  }
  ans
}

