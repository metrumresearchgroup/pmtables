
st_arg_names <- c(
  "data", "panel", "notes",
  "align", "r_file", "output_file",
  "row_space", "col_space",
  "span", "span_split", "col_rename", "col_blank",
  "sumrows", "note_config", "clear_reps"
)


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

#' @export
st_panel <- function(x,...) {
  assert_that(is.stobject(x))
  panel <- rowpanel(...)
  assert_that(is.rowpanel(panel))
  x$panel <- panel
  x
}


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

#' @export
st_noteconf <- function(x,...) {
  assert_that(is.stobject(x))
  x$note_config <- noteconf(...)
  x
}

#' @export
st_align <- function(x, ...) {
  assert_that(is.stobject(x))
  x$align <- cols_align(...)
  x
}

#' @export
st_files <- function(x, r = NULL, output = NULL) {
  assert_that(is.stobject(x))
  x$r_file <- r
  x$output_file <- output
  x
}

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

#' @export
st_span_split <- function(x,...) {
  assert_that(is.stobject(x))
  span <- colsplit(...)
  x$span_split <- span
  x
}


#' @export
st_rename <- function(x,...) {
  assert_that(is.stobject(x))
  l <- new_names(enquos(...))
  x$col_rename <- c(x$col_rename, l)
  x
}

#' @export
st_blank <- function(x,...) {
  assert_that(is.stobject(x))
  l <- new_names(enquos(...))
  x$col_blank <- c(x$col_blank, l)
  x
}

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

#' @export
st_clear_reps <- function(x, ...) {
  assert_that(is.stobject(x))
  cols <- new_names(enquos(...))
  if(!is.null(cols)) {
    x$clear_reps <- cols
  }
  x
}

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
