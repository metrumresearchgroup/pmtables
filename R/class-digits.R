#' Create a digits object
#'
#' The idea here is to capture three pieces of data: (1) a function
#' for formatting digits in an output number (2) a default number of
#' digits to use and (3) a named list with digit customization
#' information; the names correspond to potential future column
#' names and the values are custom digit information.
#'
#' @param fun function for modifying digits on a number
#' @param default the default value for digits
#' @param ... `name=value` pairs, where `name` references the data column name
#' and `value` is the number of digits for that columns
#' @param .data a named list to be used in place of `...`
#'
#' @examples
#'
#' x <- new_digit(round, default = 2, WT = 1, ALB = 3)
#'
#' as.list(x)
#'
#' @export
new_digits <- function(.fun = pmtables::sig, .default = 3, ..., .data = NULL) {
  force(.fun)
  force(.default)
  is.func <- is.function(.fun)
  .function <- deparse(substitute(.fun))
  .digits <- list(...)
  if(is.list(.data)) .digits <- .data
  if(identical(class(.fun),"function")) {
    if(!"digits" %in% names(formals(.fun))) {
      stop(
        "'digits' must be included as a formal argument for digit fun",
        call.=FALSE
      )
    }
  }
  rm(is.func)
  rm(.data)
  structure(environment(),class="digits")
}

#' Update digits object
#'
#' @param x a digits object created with [new_digits]
#' @param cols character vector of column names
#'
#' @noRd
update_digits <- function(x, cols) {
  diff <- setdiff(cols,names(x$.digits))
  if(length(diff) < 1) return(x)
  nw <- as.list(rep(x$.default,length(cols)))
  names(nw) <- cols
  x$.digits <- Update_List(nw,x$.digits)
  return(x)
}

#' Get digit function
#'
#' @param x a digits object
#' @noRd
get_digits_fun <- function(x) x$.fun

#' Get digit data
#'
#' @param x a digits object
#'
#' @noRd
get_digits_list <- function(x) x$.digits

#' @export
as.list.digits <- function(x,...) as.list(unclass(x))

#' @export
print.digits <- function(x,...) {
  func <- paste0("function: ", x$.function)
  cat(func,"\n")
  default <- paste0("default: ", x$.default)
  cat(default,"\n")
  lst <- get_digits_list(x)
  if(length(lst) > 0) {
    cat("customization:\n")
    width <- max(nchar(names(lst)))
  }
  for(i in seq_along(lst)) {
    label <- formatC(names(lst)[i],width = width)
    value <- lst[[i]]
    digit <- paste0(" ", label, ": ", value)
    cat(digit,"\n")
  }
  return(invisible(NULL))
}

is.digits <- function(x) inherits(x,"digits")
