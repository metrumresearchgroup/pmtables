check_st <- function(x) {
  assert_that(
    is.stobject(x),
    msg = "the first argument (x) must be an st object"
  )
}
st_arg_names <- c(
  "data", "panel", "notes",
  "align", "r_file", "output_file",
  "row_space", "col_space",
  "span", "span_split", "col_rename", "col_blank",
  "sumrows", "note_config", "clear_reps",
  "hline_at", "hline_from", "sizes", "units"
)

#' Create an st object
#'
#' The st object will collect various configuration settings and pass those
#' to [stable()] when the object is passed to [st_make()].
#'
#' @param data the data frame to pass to [stable()]
#' @param ... additional arguments passed to [stable()]
#'
#' @examples
#' ob <- st_new(ptdata())
#' ob <- st_data(ptdata())
#'
#' @export
st_new <- function(data, ...) {
  assert_that(is.data.frame(data))
  x <- new.env()
  x$data <- data
  x$args <- list(...)
  structure(x, class = c("stobject","environment"), argnames = st_arg_names)
}

#' @rdname st_new
#' @export
st_data <- function(data,...) st_new(data,...)


is.stobject <- function(x) inherits(x, "stobject")

#' Convert st object to table output
#'
#' @param x an stobject
#' @param ... other arguments passed to [stable()]
#' @param .preview if `TRUE`, pass result to [st_preview()]
#' @param .cat if `TRUE`, pass result to [st_wrap()]
#' @param long render as longtable
#'
#' @return The latex code for the table.
#'
#' @examples
#'
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#' st_make(ob)
#' st_make(ob, .cat = TRUE)
#' \dontrun{
#' st_make(ob, .preview = TRUE)
#' }
#'
#' @export
st_make <- function(x, ..., .preview = FALSE, .cat = FALSE, long = FALSE) {
  check_st(x)
  long <- isTRUE(long)
  # accumulated by the functions
  args <- as.list(x)
  argnames <- attr(x,"argnames")
  if(long) argnames <- c(argnames, "cap_text", "cap_macro")
  args <- args[intersect(names(args),argnames)]

  # misc args
  if(is.list(x$args)) {
    args <- combine_list(args, x$args)
  }

  dots <- list(...)
  if(length(dots) > 0) {
    args <- combine_list(args,dots)
  }

  if(long) {
    ans <- do.call(stable_long, args)
    if(isTRUE(.preview)) {
      warning("cannot preview a long table; use st2doc() instead",call.=FALSE)
      .preview <- FALSE
    }
  } else {
    ans <- do.call(stable, args)
  }

  if(.preview) {
    .null <- st_preview(ans)
    return(invisible(ans))
  }

  if(.cat) {
    .null <- pt_wrap(ans,stdout())
    return(invisible(ans))
  }

  return(ans)
}

#' Add panel information to st object
#'
#' See the `panel` argument to [stable()].
#'
#' @param x an stobject
#' @param ... passed to [rowpanel()]
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_panel("STUDYf") %>% st_make()
#'
#' @export
st_panel <- function(x,...) {
  check_st(x)
  panel <- rowpanel(...)
  assert_that(is.rowpanel(panel))
  x$panel <- panel
  x
}

#' Add note information to st object
#'
#' See the `notes` and `note_config` arguments to [stable()]. The function can
#' be called multiple times and will accumulate `notes` data.
#'
#' @param x an stobject
#' @param ... table notes
#' @param esc passed to [tab_escape()]; use `NULL` to bypass escaping the notess
#' @param config named list of arguments for [noteconf()]
#' @param collapse if `is.character`, then the note will be collapsed into a
#' single line separated by value of `collapse` (see [base::paste0()])
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_notes("ALB: albumin (g/dL)") %>% st_make()
#'
#' @export
st_notes <- function(x, ..., esc = NULL, config = NULL, collapse = NULL) {
  check_st(x)
  notes <- unlist(list(...))
  if(!is.null(notes)) {
    assert_that(is.character(notes))
    if(is.character(esc)) {
      notes <- tab_escape(notes, esc = esc)
    }
    if(is.character(collapse) && length(notes) > 1) {
      notes <- paste0(notes, collapse = collapse)
    }
    x$notes <- c(x$notes, notes)
  }
  if(is.list(config)) {
    x$note_config <- do.call(noteconf,config)
  }
  x
}

#' Add note config information to st object
#'
#' See the `note_config` argument to [stable()].
#'
#' @param x an stobject
#' @param ... named arguments passed to [noteconf()]
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>%
#'   st_notes("ALB: albumin (g/dL)") %>%
#'   st_noteconf(type = "minipage") %>%
#'   st_make()
#'
#' @export
st_noteconf <- function(x,...) {
  check_st(x)
  x$note_config <- noteconf(...)
  x
}

#' Add column alignment information to st object
#'
#' See the `align` argument to [stable()].
#'
#' @param x an stobject
#' @param ... named arguments passed to [cols_align()]
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_align(.default = 'l') %>% st_make()
#' ob %>% st_center(N = 'l') %>% st_make()
#'
#' @export
st_align <- function(x, ...) {
  check_st(x)
  x$align <- cols_align(...)
  x
}

#' @rdname st_align
#' @export
st_center <- function(x,...) {
  st_align(x,.default = "c",...)
}

#' @rdname st_align
#' @export
st_left <- function(x,...) {
  st_align(x,.default = "l",...)
}

#' @rdname st_align
#' @export
st_right <- function(x,...) {
  st_align(x,.default = "r",...)
}

#' Add file name information to st object
#'
#' See the `r_file` and `output_file` arguments to [stable()].
#'
#' @param x an stobject
#' @param r set `r_file`, passed to [stable()]
#' @param esc passed to [tab_escape()]; use `NULL` to bypass escaping
#' @param output set `output_file`, passed to [stable()]
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_files(r = "foo.R", output = "foo.tex") %>% st_make()
#'
#' @export
st_files <- function(x, r = getOption("mrg.script", NULL), output = NULL,
                     esc = NULL) {
  check_st(x)
  if(!missing(r)) {
    if(!is.null(esc)) r <- tab_escape(r, esc = esc)
    x$r_file <- r
  }
  if(!missing(output)) {
    if(!is.null(esc)) output <- tab_escape(output, esc = esc)
    x$output_file <- output
  }
  x
}

#' Add row and column spacing information to st object
#'
#' See the `row_space` and `col_space` arguments to [stable()].
#'
#' @param x an stobject
#' @param row set `row_space`, passed to [stable()]
#' @param col set `col_space`, passed to [stable()]
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_space(row = 1) %>% st_make()
#'
#' @export
st_space <- function(x, row = NULL, col = NULL) {
  check_st(x)
  if(!missing(row)) {
    x$row_space <- row
  }
  if(!missing(col)) {
    x$col_space <- col
  }
  x
}

#' Add column spanner to st object
#'
#' See the `span` argument to [stable()]. This function can be called multiple
#' times and will accumulate `span` data.
#'
#' @param x an stobject
#' @param ... passed to [colgroup()]
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_span("Covariates", WT:ALB) %>% st_make()
#'
#' @export
st_span <- function(x,...) {
  check_st(x)
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
#' See the `span_split` argument to [stable()].
#'
#' @param x an stobject
#' @param sep passed to [colsplit()]
#' @param ... passed to [colsplit()]
#'
#' @examples
#' library(dplyr)
#'
#' file <- system.file("datasets", "with-dots.RDS", package = "pmtables")
#'
#' data <- readRDS(file)
#'
#' st_new(data) %>% st_span_split('.') %>% st_make()
#'
#' @export
st_span_split <- function(x, sep,...) {
  check_st(x)
  x$span_split <- colsplit(sep = sep, ...)
  x
}

#' Add column rename information to st object
#'
#' See the `col_rename` argument to [stable()]. This function can be called
#' multiple times and will accumulate `col_rename` data.
#'
#' @param x an stobject
#' @param ... column rename items in `new-name = old-name` format; passed
#' to [stable()] as `col_rename`
#'
#' @examples
#' library(dplyr)
#'
#' st_new(ptdata()) %>% st_rename(Weight = WT) %>% st_make()
#'
#' @export
st_rename <- function(x,...) {
  check_st(x)
  l <- new_names(enquos(...))
  x$col_rename <- c(x$col_rename, l)
  x
}

#' Add column blank information to st object
#'
#' See the `col_blank` argument to [stable()].  This function can be called
#' multiple times and will accumulate `col_blank` data.
#'
#' @param x an stobject
#' @param ... quoted or unquoted column names to be passed to [stable()] as
#' `col_blank`
#'
#' @export
st_blank <- function(x,...) {
  check_st(x)
  l <- new_names(enquos(...))
  x$col_blank <- c(x$col_blank, l)
  x
}

#' Add summary row information to st object
#'
#' See the `sumrows` argument to [stable()]. This function can be called
#' multiple times and will accumulate `sumrows` data.
#'
#' @param x an stobject
#' @param ... passed to [sumrow()]
#'
#' @export
st_sumrow <- function(x,...) {
  check_st(x)
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
#' See the `clear_reps` argument to [stable()].
#'
#' @param x an stobject
#' @param ... quoted or unquoted column names passed to [stable()] as
#' `clear_reps`
#'
#' @export
st_clear_reps <- function(x, ...) {
  check_st(x)
  dots <- enquos(...)
  if(length(dots) > 0) {
    cols <- new_names(dots)
    x$clear_reps <- cols
  }
  x
}

#' Add hline information to st object
#'
#' See the `hline_at` and `hline_from` arguments to [stable()].
#'
#' @param x and stobject
#' @param at logical or integer locations for hline passed to [stable()] as
#' `hline_at`
#' @param from character column name used to divide a table; passed to
#' [stable()] as `hline_from`
#'
#' @export
st_hline <- function(x, at = NULL, from = NULL) {
  check_st(x)
  if(!missing(at)) {
    x$hline_at <- at
  }
  if(!missing(from)) {
    x$hline_from <- from
  }
  x
}

#' Add table size information to st object
#'
#' @param x an stobject
#' @param ... passed to [tab_size()]
#'
#' @export
st_sizes <- function(x,...) {
  check_st(x)
  x$sizes <- tab_size(...)
  x
}

#' Add other arguments to st object
#'
#' @param x an stobject
#' @param ... named arguments to be passed to [stable()]
#'
#' @export
st_args <- function(x,...) {
  check_st(x)
  args <- list(...)
  if(length(args) > 0) {
    args <- args[intersect(names(args),st_arg_names)]
    x$args <- c(x$args, args)
  }
  x
}

#' Add unit information to st object
#'
#' Units can be passed either as `name=value` pairs or as a named list
#' with [st_args()]. Units can alternatively be passed as an argument
#' to [stable()] as a pre-formed, named list using [st_args()].  Passing
#' as an argument this way will overwrite units specified with
#' [st_units()]. It is recommended to use either [st_units()] or
#' [st_args()] but not both.
#'
#' @param x an stobject
#' @param ... named items of the form `COL = unit` or a named list
#' of units
#' @param parens if `TRUE`, parens will be added to any unit whose first
#' character is not `(`
#'
#' @export
st_units <- function(x, ..., parens = TRUE) {
  check_st(x)
  units <- flatten(list(...))
  units <- map(units, trimws)
  if(isTRUE(parens)) {
    w <- substr(unlist(units, use.names=FALSE),1,1) != "("
    units[w] <- paste0("(",units[w],")")
  }
  if(is.list(x$units)) {
    x$units <- combine_list(x$units, units)
  } else {
    x$units <- units
  }
  x
}
