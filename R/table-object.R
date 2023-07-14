check_st <- function(x) {
  assert_that(
    is.stobject(x),
    msg = "the first argument (x) must be an st object"
  )
}

stop_if_ptobject <- function(x) {
  if(is.ptobject(x)) {
    caller <- as.character(sys.call(-1))[1]
    stop(
      glue("the {caller}() function cannot be used to operate on pmtble objects."),
      call. = FALSE
    )
  }
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
#' @details
#' Methods are included for `data.frame` and `pmtable`, an object that comes
#' from one of the data summary functions (e.g. [pt_cont_wide()], or
#' [pt_cat_long()] or [pt_demographics()]).
#'
#' If using the data frame method, the user should filter or subset so that
#' the data (`x`) contains exactly the rows (and columns) to be processed;
#' pmtables will not add or remove rows prior to processing `x`.
#'
#' @param x either a data frame or an object of class `pmtable`; see details.
#' @param ... additional arguments which will eventually get passed to the
#' table render function (e.g. [stable()] or [stable_long()]).
#'
#' @return
#' And object with class `stobject` which can get piped to other functions. The
#' `pmtable` method returns an object that also has class `ptobject`.
#'
#' @examples
#' ob <- st_new(ptdata())
#' ob <- st_data(ptdata())
#'
#' ob <- st_new(pt_data_inventory(pmt_obs))
#'
#' @export
st_new <- function(x, ...) UseMethod("st_new")
#' @rdname st_new
#' @export
st_new.data.frame <- function(x, ...) {
  e <- new.env()
  e$data <- x
  e$args <- list(...)
  structure(e, class = c("stobject", "environment"), argnames = st_arg_names)
}
#' @rdname st_new
#' @export
st_new.pmtable <- function(x, ...) {
  valid_arg_names <- c(
    "data", "panel", "cols_rename", "align", "notes", "cols_extra",
    "cols_blank", "span", "span_split", "units", "bold_cols"
  )
  incoming <- names(x)
  if(!all(incoming %in% valid_arg_names)) {
    stop("internal error: invalid item in pmtable object.")
  }
  ans <- st_new(x$data)
  foo <- lapply(incoming, function(slot) {
    assign(slot, value = x[[slot]], envir = ans)
  })
  structure(
    ans,
    class = c("stobject", "ptobject", "environment"),
    argnames = st_arg_names
  )
}

#' @rdname st_new
#' @export
st_data <- function(x,...) st_new(x = x,...)

is.stobject <- function(x) inherits(x, "stobject")
is.ptobject <- function(x) inherits(x, "ptobject")

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
#' See the `panel` argument to [stable()]. This function cannot be used to
#' operate on pmtable objects.
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
  stop_if_ptobject(x)
  panel <- rowpanel(...)
  assert_that(is.rowpanel(panel))
  x$panel <- panel
  x
}

#' Add note information to st object
#'
#' See the `notes` and `note_config` arguments passed to [stable()] and then to
#' [tab_notes()]. The function can be called multiple times and can accumulate
#' `notes` data in various ways. Use [st_notes_app()] as a short cut to append
#' a note to the previous line and [st_notes_str()] to convert all existing
#' notes into a single string.
#'
#' @param x an stobject.
#' @param ... character; one or more table notes.
#' @param esc passed to [tab_escape()]; use `NULL` to bypass escaping the notes.
#' @param config named list of arguments for [noteconf()].
#' @param collapse a character string to separate notes which are pasted
#' together when flattening or appending; this should usually end in a single
#' space (see default).
#' @param append logical; if `TRUE`, then incoming notes are appended to the
#' previous, single note in the notes list. When `...` contains multiple
#' notes, then the notes are pasted together first.
#' @param to_string logical; if `TRUE`, then all notes are collapsed to a single
#' string.
#'
#' @examples
#' library(dplyr)
#'
#' ob <- st_new(ptdata())
#'
#' ob %>% st_notes("ALB: albumin (g/dL)") %>% stable()
#'
#' @seealso
#' [st_notes_detach()], [st_notes_rm()], [st_notes_str()], [st_notes_app()]
#'
#' @export
st_notes <- function(x, ..., esc = NULL, config = NULL, collapse = "; ",
                     append = FALSE, to_string = FALSE) {
  check_st(x)
  notes <- unlist(list(...))
  if(!is.null(notes)) {
    assert_that(is.character(notes))
    if(is.character(esc)) {
      notes <- tab_escape(notes, esc = esc)
    }
    append <- isTRUE(append)    && length(x$notes) > 0
    tostr  <- isTRUE(to_string) && length(notes)   > 1
    if(tostr || append) {
      notes <- paste0(notes, collapse = collapse)
    }
    if(isTRUE(append)) {
      l <- length(x$notes)
      x$notes[l] <- paste0(c(x$notes[l], notes), collapse = collapse)
    } else {
      x$notes <- c(x$notes, notes)
    }
  }
  if(is.list(config)) {
    x$note_config <- do.call(noteconf, config)
  }
  x
}

#' Append a note to the previous position of a note vector
#'
#' @details
#' Note that the call to [st_notes()] will force in the argument
#' `append = TRUE`.
#'
#' @param ... passed to [st_notes()].
#'
#' @export
st_notes_app <- function(...) {
  st_notes(..., append = TRUE)
}

#' Convert existing note vector into a single string
#'
#' @inheritParams st_notes
#'
#' @return
#' An updated object with class `stobject`, which can be piped to other
#' functions.
#'
#' @export
st_notes_str <- function(x, collapse = "; ") {
  check_st(x)
  if(length(x$notes) == 0) return(x)
  x$notes <- paste0(x$notes, collapse = collapse)
  x
}

#' Remove notes from the table
#'
#' The can be useful when manipulating an object from one of the pmtable
#' functions (e.g. [pt_cont_long()] or [pt_demographics()], when notes are
#' automatically added to the table.
#'
#' @inheritParams st_notes
#'
#' @return
#' An updated object with class `stobject`, which can be piped to other
#' functions.
#'
#' @export
st_notes_rm <- function(x) {
  rm("notes", envir = x)
  x
}

#' Edit lines in table notes
#'
#' This function allows the replacement of _an entire line_ in table notes.
#' The line which is replaced is matched by a regular expression or identified
#' directly with the integer position  in the notes vector to replace.
#'
#' @details
#' A warning is generated if there are no notes already existing in `x`. A
#' warning is also generated if a regular expression fails to match any lines.
#' In case multiple lines are matched, only the first matching line is
#' substituted.
#'
#' @inheritParams st_notes
#' @param where a regular expression for finding a line in table notes to
#' replace; alternatively, this can be an integer specifying the line to
#' replace.
#' @param replacement the replacement text for line matching by `where`.
#' @param fixed passed to [grep()] when `where` is character.
#'
#' @return
#' An updated object with class `stobject`, which can be piped to other
#' functions.
#'
#' @export
st_notes_sub <- function(x, where, replacement, fixed = FALSE) {
  check_st(x)
  if(length(x$notes)==0) {
    warning("did not find any notes in the object; returning.")
    return(x)
  }
  assert_that(
    inherits(where, c("character", "numeric")),
    msg = "`where` must be either character or numeric."
  )
  assert_that(length(where)==1)
  if(is.character(where)) {
    m <- grep(where, x$notes, fixed = fixed)
    if(length(m)==0) {
      warning("did not find any matching notes; returning.")
      return(x)
    }
    where <- m[1]
  } else {
    where <- floor(where)
  }
  note_number <- where
  assert_that(note_number >= 0 && note_number <= length(x$notes))
  x$notes[note_number] <- replacement
  x
}

#' Detach table notes from the table
#'
#' Detached notes are rendered underneath the table, in a separate minipage
#' format. By default, there is an `hline` rendered between the table and the
#' notes. It is common to adjust the width of the minipage holding the notes
#' depending on the width of the table and the extent of the notes.
#'
#' @inheritParams st_notes
#' @param width passed to [noteconf()] via [st_noteconf()].
#' @param type passed to [noteconf()] via [st_noteconf()]; this argument should
#' not be changed if detached notes are desired.
#' @param ... other arguments passed to [noteconf()] via [st_noteconf()].
#'
#' @return
#' An updated object with class `stobject`, which can be piped to other
#' functions.
#'
#' @export
st_notes_detach <- function(x, width = 0.8, type = "minipage", ...) {
  check_st(x)
  st_noteconf(x, width = width, type = type, ...)
}

#' Add note config information to st object
#'
#' See the `note_config` argument passed to [stable()] and then to
#' [tab_notes()].
#'
#' @param x an stobject.
#' @param ... named arguments passed to [noteconf()].
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

#' @rdname st_noteconf
#' @export
st_notes_conf <- st_noteconf

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
    x$span <- c(x$span, list(span))
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
    msg = "the `split` argument is FALSE; use `st_span()` instead."
  )
  check_st(x)
  stop_if_ptobject(x)
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
#' See the `units` argument to [stable()]. This function cannot be used to
#' work on pmtable objects.
#'
#' @details
#' Units can be passed either as
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
  stop_if_ptobject(x)
  units <- flatten(list(...))
  units <- map(units, trimws)
  if(isTRUE(parens)) {
    u <- unlist(units, use.names = FALSE)
    w <- substr(u, 1, 1) != "(" & nchar(u) > 0
    units[w] <- paste0("(", units[w], ")")
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
  check_st(x)
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
  check_st(x)
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
  check_st(x)
  dots <- new_names(enquos(...))
  x$drop <- c(x$drop, dots)
  x
}

#' Filter, select, or mutate data
#'
#' These functions modify the input data frame prior to passing it to
#' [stable()] or [stable_long()].
#'
#' @param x an stobject.
#' @param ... passed to [dplyr::select()], [dplyr::mutate()], or
#' [dplyr::filter()].
#'
#' @details
#' - `st_select` calls `dplyr::select` on the data
#' - `st_mutate` calls `dplyr::mutate` on the data
#' - `st_filter` calls `dplyr::filter` on the data
#'
#' @examples
#' tab <- pt_data_inventory(pmt_obs, by = "FORM")
#' obj <- st_new(tab)
#' st_filter(obj, FORM != "troche")
#' st_select(obj, -contains("BQL"))
#' st_mutate(obj, FORM = ifelse(FORM=="tablet", "ODT", FORM))
#'
#' @export
st_select <- function(x, ...) {
  check_st(x)
  x$data <- select(x$data, ...)
  x
}

#' @rdname st_select
#' @export
st_mutate <- function(x, ...) {
  check_st(x)
  x$data <- mutate(x$data, ...)
  x
}

#' @rdname st_select
#' @export
st_filter <- function(x, ...) {
  check_st(x)
  x$data <- filter(x$data, ...)
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


#' Clone an stobject
#'
#' @param x an stobject object.
#'
#' @return
#' A copy (`y`) of `x` such that `y` can be modified without modifying `x`.
#'
#' @examples
#' x <- st_new(stdata())
#' y <- st_clone(x)
#'
#' y$data$STUDY <- NULL
#' x$data
#' y$data
#'
#' # Get back to a regular environment
#' class(x) <- "environment"
#' class(y) <- "environment"
#'
#' x
#' y
#'
#' @export
st_clone <- function(x) {
  if(!inherits(x, "stobject")) {
    stop("Can only clone stobjects.")
  }
  y <- env_clone(x)
  attributes(y) <- attributes(x)
  y
}
