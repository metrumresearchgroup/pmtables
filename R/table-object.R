check_st <- function(x) {
  assert_that(
    is.stobject(x),
    msg = "the first argument (x) must be an st object"
  )
}

st_arg_names <- c(
  "data", "panel", "notes",
  "align", "r_file", "output_file",
  "span", "span_split", "cols_rename", "cols_blank",
  "sumrows", "note_config", "clear_reps", "clear_grouped_reps",
  "hline_at", "hline_from", "sizes", "units", "drop",
  "lt_cap_text", "lt_cap_macro", "lt_cap_label", "lt_cap_short", "lt_continue",
  "args"
)

#' Create an st object
#'
#' The st object will collect various configuration settings and pass those
#' to [stable()] when the object is passed to [st_make()].
#'
#' @param data the data frame to pass to [stable()]; the user should filter
#' or subset so that `data` contains exactly the rows (and columns) to be
#' processed; pmtables will not add or remove rows prior to processing `data`
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
#' @param .cat if `TRUE`, pass result to [st_wrap()]; use this option to write
#' table text to standard output while creating Rmarkdown documents with pdf
#' output
#' @param long render as longtable
#'
#' @return The latex code for the table.
#'
#' @examples
#'
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' st_make(ob)
#'
#' st_make(ob, .cat = TRUE)
#'
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
  # misc args passed in via st_args
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
      warning("cannot preview a long table; use st2report() instead", call.=FALSE)
      .preview <- FALSE
    }
  } else {
    ans <- do.call(stable, args)
  }
  if(.preview) { # nocov start
    .null <- st_preview(ans)
    return(invisible(ans))
  }
  if(.cat) {
    .null <- pt_wrap(ans,stdout())
    return(invisible(ans))
  } # nocov end

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
#' ob %>% st_panel("STUDY") %>% st_make()
#'
#' @export
st_panel <- function(x, ...) {
  check_st(x)
  panel <- rowpanel(...)
  assert_that(is.rowpanel(panel))
  x$panel <- panel
  x
}

#' Add note information to st object
#'
#' See the `notes` and `note_config` arguments passed to [stable()] and then to
#' [tab_notes()]. The function can be called multiple times and will accumulate
#' `notes` data.
#'
#' @param x an stobject
#' @param ... table notes
#' @param esc passed to [tab_escape()]; use `NULL` to bypass escaping the notes
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
#' See the `note_config` argument passed to [stable()] and then to
#' [tab_notes()].
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
#' See the `align` argument to [stable()]. Note: these functions
#' always replace the current alignment in total.
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
#'
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
#' See the `r_file` and `output_file` arguments passed to [stable()] and then
#' to [tab_notes()].
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
#' See the `sizes` argument to [stable()] and the `row_space` and `col_space`
#' arguments to [tab_size()].
#'
#' @param x an stobject
#' @param row set `row_space`, passed to [stable()] and then to [tab_size()]
#' @param col set `col_space`, passed to [stable()] and then to [tab_size()]
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_space(row = 1) %>% st_make()
#'
#' @export
st_space <- function(x, row = 1.5, col = 5) {
  check_st(x)
  if(is.null(x$sizes)) {
    x$sizes <- tab_size(row = row, col = col)
  } else {
    x$sizes$row_space <- row
    x$sizes$col_space <- col
  }
  x
}

#' Add column spanner to st object
#'
#' See the `span` argument to [stable()]. This function can be called multiple
#' times and will accumulate `span` data.
#'
#' @param x an stobject
#' @param split if `TRUE`, then [st_span_split()] is called
#' @param ... passed to [colgroup()] or [st_span_split()] if `split` is `TRUE`
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_span("Covariates", WT:ALB) %>% st_make()
#'
#' @export
st_span <- function(x, ..., split = FALSE) {
  if(isTRUE(split)) {
    return(st_span_split(x, ..., split = split))
  }
  check_st(x)
  span <- colgroup(..., split = split)
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
#' See the `span_split` argument passed to [stable()] and then [tab_spanners()].
#'
#' @param x an stobject
#' @param split passed to [colsplit()], if `split` is `FALSE`, then
#' an error is generated
#' @param ... passed to [colsplit()]
#'
#' @details
#' There can only be one `span_split` per table; if `st_span_split` is
#' called more than once in a pipeline, a warning will be issued on every
#' call after the first one and only the latest `span_split` data will be
#' retained in the table.
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
st_span_split <- function(x, ..., split = TRUE) {
  assert_that(
    isTRUE(split),
    msg = "the `split` argument is FALSE; use `st_span()` instead"
  )
  check_st(x)
  span <- colsplit(..., split = split)
  if(!is.null(x$span_split)) {
    warning(
      "`span_split` is already set and will be replaced",
      call. = FALSE
    )
  }
  x$span_split <- span
  x
}

#' Add column rename information to st object
#'
#' See the `cols_rename` argument passed to [stable()] and then [tab_cols()].
#' This function can be called multiple times and will accumulate `cols_rename`
#' data.
#'
#' @param x an stobject
#' @param ... column rename items in `new-name = old-name` format; passed
#' to [stable()] as `cols_rename`
#' @param .list a named list of rename data with the format
#' `old-name = new-name`; this specification is similar passing items via
#' `...`, but note that rename specification is reversed. The intended use for
#' this argument is to utilize list output from the `yspec` package which takes
#' the form `column-name = short-name` (e.g. `WT = weight` for the `WT` column).
#'
#' @examples
#' library(dplyr)
#'
#' st_new(stdata()) %>% st_rename(weight = WT) %>% stable()
#'
#' st_new(stdata()) %>% st_rename(.list = list(WT = "weight")) %>% stable()
#'
#' @export
st_rename <- function(x, ..., .list = NULL) {
  check_st(x)
  if(!is.null(.list)) {
    # This is also checked in new_names, but asserting here too to avoid breakage
    assert_that(is_named(.list))
    .old <- names(.list)
    .new <- unlist(.list, use.names = FALSE)
    if(!any(.old %in% names(x[["data"]]))) {
      warning(
        "rename data was passed as `.list`, but zero columns were matched\n",
        "please check that the list was properly specified (?st_rename)",
        call.=FALSE
      )
    }
    .list <- setNames(.old, .new)
    l <- new_names(.list)
  } else {
    l <- new_names(enquos(...))
  }
  x$cols_rename <- c(x$cols_rename, l)
  x$cols_rename <- x$cols_rename[!duplicated(x$cols_rename)]
  x
}


#' Add column blank information to st object
#'
#' See the `cols_blank` argument passed to [stable()] and then [tab_cols()].
#' This function can be called multiple times and will accumulate `cols_blank`
#' data.
#'
#' @param x an stobject
#' @param ... quoted or unquoted column names to be passed to [stable()] as
#' `cols_blank`
#'
#' @export
st_blank <- function(x,...) {
  check_st(x)
  l <- new_names(enquos(...))
  x$cols_blank <- c(x$cols_blank, l)
  x
}

#' Add summary row information to st object
#'
#' See the `sumrows` argument to [stable()]. This function can be called
#' multiple times and will accumulate `sumrows` data.
#'
#' @inheritParams sumrow
#' @param x an stobject
#' @param pattern a regular expression to search in the data frame; when this
#' argument is provided, `rows` are calculated using [df_grep_rows()]
#' @param cols a character vector of column names to search
#' @param ... passed to [sumrow()]
#'
#' @details
#' Please take careful note of the argument order for [st_sumrow()] compared to
#' [sumrow()].
#'
#' @export
st_sumrow <- function(x, pattern = NULL, cols = names(x$data), rows = integer(0),
                      ...) {
  check_st(x)
  if(is.character(pattern)) {
    rows <- df_grep_rows(x$data, pattern, cols)
  }
  sumr <- sumrow(rows = rows, ...)
  if(is.list(x$sumrows)) {
    x$sumrows <- c(x$sumrows, list(sumr))
    return(x)
  }
  x$sumrows <- list(sumr)
  x
}

#' Add clear rep information to st object
#'
#' See the `clear_reps` argument passed to [stable()] and then to
#' [tab_clear_reps()].
#'
#' @param x an stobject
#' @param ... quoted or unquoted column names passed to [stable()] as
#' `clear_reps`
#' @param .now if `TRUE`, the data is immediately processed; otherwise, the
#' processing is done after the pipeline completes
#'
#' @export
st_clear_reps <- function(x, ..., .now = FALSE) {
  check_st(x)
  dots <- enquos(...)
  if(length(dots) > 0) {
    cols <- new_names(dots)
    if(isTRUE(.now)) {
      x$data <- tab_clear_reps(x$data, clear_reps = cols)
    } else {
      x$clear_reps <- cols
    }
  }
  x
}

#' @rdname st_clear_reps
#' @export
st_clear_grouped <- function(x, ...) {
  check_st(x)
  dots <- enquos(...)
  if(length(dots) > 0) {
    cols <- new_names(dots)
    x$clear_grouped_reps <- cols
  }
  x
}

#' Add hline information to st object
#'
#' See the `hline_at` and `hline_from` arguments passed to [stable()] and
#' then to [tab_hlines()],
#'
#' @param x and stobject
#' @param pattern a regular expression to find rows where an `hline` will be
#' placed; passed to [stringr::str_detect()]
#' @param cols data columns to scan using `pattern`
#' @param n number of `hlines` to add when a hit is found
#' @param at logical or integer locations for hline passed to [stable()] as
#' `hline_at`
#' @param from character column name used to divide a table; passed to
#' [stable()] as `hline_from`
#' @param nudge push an hline down or up in the table; only applies to
#' indices found from using either the `at` or `pattern` arguments
#'
#' @export
st_hline <- function(x, pattern = NULL, cols = names(x$data), n = 1,
                     at = NULL, from = NULL, nudge = 0) {
  check_st(x)
  if(!missing(at)) {
    if(is.logical(at)) at <- which(at)
    at <- at + nudge
    if(n > 1) at <- sort(rep(at, n))
    x$hline_at <- c(x$hline_at,at)
  }
  if(!missing(from)) {
    x$hline_from <- c(x$hline_from,from)
  }
  if(is.character(pattern)) {
    hline_re <- unique(find_hline_df(x$data, pattern, cols))
    hline_re <- hline_re[hline_re > 1] + nudge
    if(n > 1) {
      hline_re <- sort(rep(hline_re,n))
    }
    x$hline_at <- c(x$hline_at, hline_re)
  }
  x
}

#' Add table size information to st object
#'
#' See the `sizes` argument to [stable()], passed to [tab_size()].
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
#' The arguments entered here are passed to [stable()] or [stable_long()].
#'
#' @param x an stobject
#' @param ... named arguments to be passed to [stable()]
#'
#' @export
st_args <- function(x,...) {
  check_st(x)
  args <- list(...)
  assert_that(
    is_named(args),
    msg = "arguments passed to st_args must be named"
  )
  if(length(args) > 0) {
    x$args <- combine_list(x$args, args)
  }
  x
}

#' Add unit information to st object
#'
#' See the `units` argument to [stable()]. Units can be passed either as
#' `name=value` pairs or as a named list with [st_args()]. Units can
#' alternatively be passed as an argument to [stable()] as a pre-formed, named
#' list using [st_args()].  Passing as an argument this way will overwrite units
#' specified with [st_units()]. It is recommended to use either [st_units()] or
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
    u <- unlist(units, use.names=FALSE)
    w <- substr(u,1,1) != "(" & nchar(u) > 0
    units[w] <- paste0("(",units[w],")")
  }
  if(is.list(x$units)) {
    x$units <- combine_list(x$units, units)
  } else {
    x$units <- units
  }
  x
}

#' Render table data in bold or italic font
#'
#' These functions modify the input data frame prior to passing it to
#' [stable()] or [stable_long()].
#'
#' @param x an stobject
#' @param cols columns to make bold
#' @param pattern passed to [tex_bold()] or [tex_it()]
#'
#' @export
st_bold <- function(x, cols, pattern = "*") {
  cols <- new_names(cols)
  assert_that(all(cols %in% names(x$data)))
  for(col in cols) {
    x$data[[col]] <- tex_bold(as.character(x$data[[col]]), pattern = pattern)
  }
  x
}

#' @rdname st_bold
#' @export
st_it <- function(x, cols, pattern = "*") {
  cols <- new_names(cols)
  assert_that(all(cols %in% names(x$data)))
  for(col in cols) {
    x$data[[col]] <- tex_it(as.character(x$data[[col]]), pattern = pattern)
  }
  x
}

#' Drop data columns
#'
#' See the `drop` argument to [stable()].
#'
#' @param x an stobject
#' @param ... column names to drop
#'
#' @export
st_drop <- function(x, ...) {
  dots <- new_names(enquos(...))
  x$drop <- c(x$drop, dots)
  x
}

#' Filter, select, or mutate data
#'
#' These functions modify the input data frame prior to passing it to
#' [stable()] or [stable_long()].
#'
#' @param x an stobject
#' @param ... passed to [dplyr::select()], or [dplyr::mutate()]
#'
#' @export
st_select <- function(x, ...) {
  x$data <- dplyr::select(x$data, ...)
  x
}

#' @rdname st_select
#' @export
st_mutate <- function(x, ...) {
  x$data <- dplyr::mutate(x$data, ...)
  x
}

#' Edit table contents
#'
#' These functions modify the input data frame prior to passing to [stable()]
#' or [stable_long()].
#'
#' @param x an stobject
#' @param ... arguments passed to [tab_edit()]
#'
#' @export
st_edit <- function(x, ...) {
  check_st(x)
  x$data <- tab_edit(x$data, ...)
  x
}

#' @param data a data frame
#' @param pattern passed to [stringr::str_replace()]
#' @param replacement passed to [stringr::str_replace()]
#' @param cols data columns to check for `pattern`
#' @rdname st_edit
#' @export
tab_edit <- function(data, pattern, replacement, cols = names(data)) {
  if(!missing(cols)) {
    cols <- cols[cols %in% names(data)]
  }
  if(length(cols)==0) return(data)
  for(col in cols) {
    data[[col]] <- str_replace(data[[col]],pattern,replacement)
  }
  data
}


#' Methods for stobject
#'
#' @param x an stobject
#' @param ... other arguments
#' @export
print.stobject <- function(x, ...) {
  print(as_tibble(x$data))
}

#' @rdname print.stobject
#' @export
names.stobject <- function(x) names(x$data)
