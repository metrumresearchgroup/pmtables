
#' Create stable from pmtable
#'
#' @param x object to convert to stable
#' @param ... for the `pmtable` method, these are extra named arguments to pass to [stable()]
#' @param wrap if `TRUE`, the stable output will be wrapped in a latex table environment
#' @param wrapw if `TRUE`, the stable output will be wrapped in a latex table environment and
#' the output will be written to [stdout()]; use this along with `results = "asis"` when rendering
#' tables with [rmarkdown::render()]
#'
#' @export
#'
as_stable <- function(x, ...) UseMethod("as_stable")

#' @rdname as_stable
#' @keywords internal
#' @export
as_stable.pmtable <- function(x, ..., wrap = FALSE, wrapw = FALSE) {
  up <- list(...)
  replace <- intersect(names(up),names(x))
  if(length(replace) > 0) {
    x[replace] <- up[replace]
    up[replace] <- NULL
  }
  x <- c(x,up)
  valid <- intersect(names(x),stable_argument_names())
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

#' @rdname as_stable
#' @keywords internal
#' @export
as_stable.stable <- function(x,...) {
  x
}

#' Get debug information from stable object
#'
#' @param x an stable object
#'
#' @export
get_stable_data <- function(x) {
  ans <- list(output = as.character(x),stable_file = attr(x,"stable_file"))
  ans <- c(ans, as.list(attr(x,"stable_data")))
  ans
}


#' @export
print.stable_data <- function(x,...) {
  cat("table data is attached; extract with get_stable_data()")
  return(invisible(NULL))
}
