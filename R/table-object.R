
st_arg_names <- c(
  "data", "panel", "notes",
  "align", "r_file", "output_file",
  "row_space", "col_space",
  "span", "span_split", "col_rename", "col_blank",
  "sumrows", "note_config", "clear_reps"
)

#' Create an st object
#'
#' The st object will collect various configuration settings and pass those
#' to [stable()] when the object is passed to [st_make()].
#'
#' @param data the data frame to pass to [stable()]
#'
#' @export
st_new <- function(data) {
  x <- new.env()
  form <- formals(stable)
  x$data <- data
  x$panel <- rowpanel(col = NULL)
  x$span <- form$span
  x$notes <- form$notes
  x$align <- form$align
  x$r_file <- form$r_file
  x$output_file <- form$output_file
  x$row_space <- form$row_space
  x$col_space <- form$col_space
  x$col_rename <- form$col_rename
  x$col_blank <- form$col_blank
  x$sumrows <- form$sumrows
  x$note_config <- form$note_config
  x$clear_reps <- form$clear_reps
  x$span_split <- form$span_split
  x$args <- list()
  structure(x, class = "stobject", argnames = st_arg_names)
}

is.stobject <- function(x) inherits(x, "stobject")

#' Convert st object to table output
#'
#' @param x an stobject
#' @param ... other arguments passed to [stable()]
#'
#' @export
st_make <- function(x, ...) {
  assert_that(is.stobject(x))
  args <- as.list(structure(x, class = NULL))
  args <- args[attr(x,"argnames")]

  if(is.list(x$args)) {
    args <- combine_list(args, x$args)
  }

  dots <- list(...)
  if(length(dots) > 0) {
    args <- combine_list(args,dots)
  }

  do.call(stable, args)
}

#' Add panel information to st object
#'
#' @param x an stobject
#' @param ... passed to [rowpanel()]
#'
#' @export
st_panel <- function(x,...) {
  assert_that(is.stobject(x))
  panel <- rowpanel(...)
  assert_that(is.rowpanel(panel))
  x$panel <- panel
  x
}

#' Add note information to st object
#'
#' @param x an stobject
#' @param ... table notes
#' @param config named list of arguments for [noteconf()]
#'
#' @export
st_notes <- function(x, ..., config = NULL) {
  assert_that(is.stobject(x))
  notes <- unlist(list(...))
  if(!is.null(notes)) {
    assert_that(is.character(notes))
    x$notes <- c(x$notes, notes)
  }
  if(is.list(config)) {
    x$note_config <- do.call(noteconf,config)
  }
  x
}

#' Add note config information to st object
#'
#' @param x an stobject
#' @param ... named arguments passed to [noteconf()]
#'
#' @export
st_noteconf <- function(x,...) {
  assert_that(is.stobject(x))
  x$note_config <- noteconf(...)
  x
}

#' Add column alignment information to st object
#'
#' @param x an stobject
#' @param ... named arguments passed to [cols_align()]
#'
#' @export
st_align <- function(x, ...) {
  assert_that(is.stobject(x))
  x$align <- cols_align(...)
  x
}

#' Add file name information to st object
#'
#' @param x an stobject
#' @param r set `r_file`, passed to [stable()]
#' @param output set `output_file`, passed to [stable()]
#'
#' @export
st_files <- function(x, r = NULL, output = NULL) {
  assert_that(is.stobject(x))
  x$r_file <- r
  x$output_file <- output
  x
}

#' Add row and column spacing information to st object
#'
#' @param x an stobject
#' @param row set `row_space`, passed to [stable()]
#' @param col set `col_space`, passed to [stable()]
#'
#' @export
st_space <- function(x, row = NULL, col = NULL) {
  assert_that(is.stobject(x))
  if(is.numeric(row)) {
    x$row_space <- row
  }
  if(is.numeric(col)) {
    x$col_space <- col
  }
  x
}

#' Add column spanner to st object
#'
#' @param x an stobject
#' @param ... passed to [colgroup()]
#'
#' @export
st_span <- function(x,...) {
  assert_that(is.stobject(x))
  span <- colgroup(...)
  if(is.null(x$span)) {
    x$span <- list(span)
    return(x)
  }
  if(is.list(x$span)) {
    x$span <- c(x$span,list(span))
    return(x)
  }
  x
}

#' Add column split spanner information to st object
#'
#' @param x an stobject
#' @param ... passed to [colsplit()]
#'
#' @export
st_span_split <- function(x,...) {
  assert_that(is.stobject(x))
  span <- colsplit(...)
  x$span_split <- span
  x
}

#' Add column rename information to st object
#'
#' @param x an stobject
#' @param ... column rename items in `new-name = old-name` format; passed
#' to [stable()] as `col_rename`
#' @export
st_rename <- function(x,...) {
  assert_that(is.stobject(x))
  l <- new_names(enquos(...))
  x$col_rename <- c(x$col_rename, l)
  x
}

#' Add column blank information to st object
#'
#' @param x an stobject
#' @param ... quoted or unquoted column names to be passed to [stable()] as
#' `col_blank`
#'
#' @export
st_blank <- function(x,...) {
  assert_that(is.stobject(x))
  l <- new_names(enquos(...))
  x$col_blank <- c(x$col_blank, l)
  x
}

#' Add summary row information to st object
#'
#' @param x an stobject
#' @param ... passed to [sumrow()]
#'
#' @export
st_sumrow <- function(x,...) {
  assert_that(is.stobject(x))
  sumr <- sumrow(...)
  if(is.list(x$sumrows)) {
    x$sumrows <- c(x$sumrows, list(sumr))
    return(x)
  }
  x$sumrows <- list(sumr)
  x
}

#' Add clear rep information to st object
#'
#' @param x an stobject
#' @param ... quoted or unquoted column names passed to [stable()] as
#' `clear_reps`
#'
#' @export
st_clear_reps <- function(x, ...) {
  assert_that(is.stobject(x))
  cols <- new_names(enquos(...))
  if(!is.null(cols)) {
    x$clear_reps <- cols
  }
  x
}

#' Add other arguments to st object
#'
#' @param x an stobject
#' @param ... named arguments to be passed to [stable()]
#'
#' @export
st_args <- function(x,...) {
  assert_that(is.stobject(x))
  args <- list(...)
  if(length(args) > 0) {
    args <- args[intersect(names(args),names(formals(stable)))]
    x$args <- c(x$args, args)
  }
  x
}
