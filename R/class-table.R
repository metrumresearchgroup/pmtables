
#' Create stable from pmtable
#'
#' @export
#'
as_stable <- function(x, ...) UseMethod("as_stable")

#' @export
as_stable.pmtable <- function(x, ..., wrap = FALSE, wrapw = FALSE) {
  up <- list(...)
  replace <- intersect(names(up),names(x))
  if(length(replace) > 0) {
    x[replace] <- up[replace]
    up[replace] <- NULL
  }
  x <- c(x,up)
  valid <- intersect(names(x),names(formals(stable)))
  x <- x[valid]
  ans <- do.call(stable, args = x)
  if(isTRUE(wrap) || isTRUE(wrapw)) {
    ans <- pt_wrap(ans)
  }
  if(isTRUE(wrapw)) {
    writeLines(ans)
  }
  return(invisible(ans))
}

#' @export
as_stable.stable <- function(x,...) {
  x
}


