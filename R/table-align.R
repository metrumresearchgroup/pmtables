
#' Set table column alignment
#'
#' @param .default the default column alignment
#' @param ... exceptions to use from `.default`; each argument should be named
#' by a column in the data set; values should be either "l", "c", or "r";
#' for example: `WT = "l"`
#' @param .r column names as character vector or comma separated string
#' to align right; for example: `.r = "WT,AGE"`
#' @param .c column names as character vector or comma separated string
#' to center; ; for example: `.c = "WT,AGE"`
#' @param .l column names as character vector or comma separated string
#' to align left; ; for example: `.l = "WT,AGE"`
#' @param  .coltype should be p, m, or b
#' @param .outer force the left-most column to the left (`l`), or the right-most
#' column on the right (`r`), or both (`lr`)
#' @param .complete not used
#'
#' @export
cols_align <- function(.default = 'l', ...,
                       .r = NULL, .c = NULL, .l = NULL,
                       .coltype = 'p',
                       .outer = c("none", "l", "r", "lr"),
                       .complete = NULL) {

  .outer <- match.arg(.outer)

  to_update <- list(...)
  to_update <- align_update(to_update, .r,  "r")
  to_update <- align_update(to_update, .l,  "l")
  to_update <- align_update(to_update, .c,  "c")

  ans <- list(
    complete = .complete,
    default = .default,
    update = to_update,
    coltype = .coltype,
    outer = .outer
  )
  structure(ans, class = "aligncol")
}

is.aligncol <- function(x) inherits(x, "aligncol")

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
#' @param center `logical`; if `TRUE`, then column will be centered when
#' `ragged` is `no`
#' @param coltype column type
#'
#' @export
col_fixed <- function(size, ragged = c("no", "right", "left"),
                      center = FALSE,
                      unit = "cm",
                      coltype = c("p","m","b")) {
  ragged <- match.arg(ragged)

  coltype <- match.arg(coltype)

  if(ragged=="no") {
    cntr <- ifelse(isTRUE(center), ">{\\centering\\arraybackslash}", "")
    ans <- paste0(cntr, coltype, "{", size, unit, "}")
    return(ans)
  }
  paste0(">{\\ragged",ragged,"\\arraybackslash}",coltype,"{",size,unit,"}")
}

#' @rdname col_fixed
#' @param ... arguments passed to col fixed
#' @export
col_ragged <- function(..., ragged = "right") {
  col_fixed(..., ragged = ragged)
}


form_align <- function(x,cols,pipes = FALSE) {

  if(is.character(x$complete)) return(x$complete)

  nc <- length(cols)
  ans <- rep(x$default, nc)

  if(x$outer=="lr") {
    ans[c(1,length(ans))] <- c('l', 'r')
  }
  if(x$outer=="l") {
    ans[1] <- 'l'
  }
  if(x$outer=="r") {
    ans[length(ans)] <- 'r'
  }
  up <- x$update
  up <- up[names(up) %in% cols]
  replace <- match(names(up), cols)
  replace <- replace[!is.na(replace)]
  ans[replace] <- unlist(up,use.names=FALSE)

  if(isTRUE(pipes)) {
    where <- seq_along(ans)[-1]
    ans[where] <- paste0("|",ans[where])
  }

  ans <- paste0(ans,collapse="")

  ans
}

#' @export
update.aligncol <- function(object, ...) {
  args <- list(...)
  if(length(args) ==0) return(object)
  ac <- do.call(cols_align, args)
  # Error out if the object changed at all.
  stopifnot(
    identical(
      names(ac),
      c("complete", "default", "update", "coltype", "outer")
    )
  )
  if(".complete" %in% names(args)) {
    object$complete <- ac$complete
  }
  if(".default" %in% names(args)) {
    object$default <- ac$default
  }
  if(".coltype" %in% names(args)) {
    object$coltype <- ac$coltype
  }
  if(".outer" %in% names(args)) {
    object$outer <- ac$outer
  }
  if(length(ac$update) > 0) {
    object$update <- combine_list(object$update, ac$update)
  }
  object
}
