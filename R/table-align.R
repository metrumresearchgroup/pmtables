
#' Set table column alignment
#'
#' @param .default the default column alignment
#' @param ... exceptions to use from `default`; each argument should be named
#' by a column in the data set; values should be either "l", "c", or "r"
#' @param .complete not used
#'
#' @export
cols_align <- function(.default  = c("l", "c", "r"), ..., .complete = NULL) {
  .default <- match.arg(.default)
  ans <- list(
    complete = .complete,
    default = .default,
    update = list(...)
  )
  structure(ans, class = "aligncol")
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

