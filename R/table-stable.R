
start_tpt <- "\\begin{threeparttable}"
end_tpt <- "\\end{threeparttable}"
begin_tn <- "\\begin{tablenotes}[flushleft]"
end_tn <- "\\end{tablenotes}"
hl <- "\\hline"
note_space <- 0.1

stable_argument_names <- function() {
  unique(
    c(
      names(formals(stable.data.frame)),
      names(formals(tab_hlines)),
      names(formals(tab_spanners)),
      names(formals(tab_notes)),
      names(formals(tab_clear_reps)),
      names(formals(make_tabular)),
      names(formals(tab_cols)),
      names(formals(tab_size)),
      names(formals(stable_long))
    )
  )
}


validate_sumrows <- function(x) {
  if(inherits(x, "sumrow")) {
    x <- list(x)
  }
  if(!is.null(x)) {
    assert_that(is.list(x))
  }
  return(x)
}

validate_units <- function(x, data) {
  if(is.null(x)) return(x)
  assert_that(
    is.list(x),
    msg = "'units' must be a named list"
  )
  assert_that(
    rlang::is_named(x),
    msg = "'units' must be a named list"
  )
  ok <- names(x) %in% names(data)
  if(!any(ok)) {
    warning("there were no valid units found; returning", call.=FALSE)
    return(NULL)
  }
  x[ok]
}

#' Get data ready for processing with s table
#'
#' @param data a data frame
#'
#' @details
#' 1. `data` must be a data frame
#' 1. `data` is ungrouped with [dplyr::ungroup()]
#' 1. factor columns in `data` are converted to character
#'
#' @export
triage_data <- function(data) {
  assert_that(is.data.frame(data), msg = "'data' must be a data frame")
  data <- ungroup(data)
  fct <- map_lgl(data, is.factor)
  data <- modify_if(data, fct, as.character)
  data
}

#' Create tabular output from an R data frame
#'
#' @param data a data.frame to convert to tabular table; the user should filter
#' or subset so that `data` contains exactly the rows (and columns) to be
#' processed; pmtables will not add or remove rows prior to processing `data`;
#' see also [st_new()]
#' @param align an alignment object created by [cols_align()], [cols_left()],
#' [cols_center()], or [cols_right()]; see also [st_align()]
#' @param panel character column name to use to section the table; sections will
#' be created from unique values of `data[[panel]]`; see also [st_panel()]
#' @param span a list of objects created with [colgroup()]; ; see also [st_span()]
#' @param notes a character vector of notes to include at the foot of the table;
#' use `r_file` and `output_file` for source code and output file annotations;
#' see [tab_notes()] for arguments to pass in order to configure the way notes
#' appear in the output; see also [st_notes()]
#' @param sumrows an object created with [sumrow()]; identifies summary rows
#' and adds styling; see also [st_sumrow()]
#' @param units a named list with unit information; names should correspond to
#' columns in the data frame
#' @param drop columns to remove prior to rendering the table
#' @param sizes an object returned from [tab_size()]
#' @param control not used at the moment
#' @param escape_fun a function passed to `prime_fun` that will sanitize column
#' data
#' @param inspect if `TRUE`, extra information is attached to the output
#' as an attribute called `stable_data`; see [get_stable_data()]
#' @param ... passed to other functions: [tab_hlines()], [tab_spanners()],
#' [tab_notes()], [tab_cols()], [tab_clear_reps()] and [make_tabular()]
#'
#' @examples
#' data <- ptdata()
#'
#' a <- stable(data, r_file = "example.R", output_file = "output.tex")
#'
#' b <- stable(data, panel = "STUDY")
#'
#' c <- stable(data, span = colgroup("Covariates", STUDY:ALB))
#'
#' @export
stable <- function(data, ...) UseMethod("stable")

#' @rdname stable
#' @export
stable.data.frame <- function(data,
                              align = cols_left(),
                              panel = NULL,
                              span = NULL,
                              notes = NULL,
                              sumrows = NULL,
                              units = NULL,
                              drop = NULL,
                              sizes = tab_size(),
                              control = st_control(),
                              escape_fun = tab_escape,
                              inspect = FALSE,
                              ... ) {

  data <- triage_data(data)
  assert_that(
    inherits(sizes, "from_tab_sizes"),
    msg = "'sizes' must be an object created from tab_size()"
  )
  assert_that(
    is.function(escape_fun),
    msg = "'escape_fun' must be a function"
  )
  assert_that(
    is.aligncol(align),
    msg = "'align' must be created from cols_align() or other helper in ?cols_align"
  )
  assert_that(
    is.character(notes) || is.null(notes),
    msg = "'notes' must be character or NULL"
  )
  assert_that(
    inherits(control, "st_control"),
    msg = "'control' must be created with st_control()"
  )
  sumrows <- validate_sumrows(sumrows)
  panel <- as.panel(panel)

  # hlines
  add_hlines <- tab_hlines(data, ...)

  # clear reps
  data <- tab_clear_reps(data, panel = panel, ...)

  # Drop
  drop_cols <- function(data,drop) {
    if(is.null(drop)) return(data)
    drop <- new_names(drop)
    for(col in drop) {
      data[[col]] <- NULL
    }
    data
  }

  data <- drop_cols(data, drop)

  # panel
  panel_insert <- tab_panel(data, panel, sumrows)
  data <- panel_insert$data

  # units / after panel is done
  units <- validate_units(units, data)

  # sumrows
  sumrow_insert <- tab_find_sumrows(data, sumrows)
  data <- sumrow_insert$data

  add_hlines <- c(add_hlines, sumrow_insert$hlines)

  # Colgroups
  cols <- names(data)
  span_data <- tab_spanners(data = data, cols = cols, span = span, ...)
  cols <- span_data$cols

  # Format cols
  cols_data <- tab_cols(cols, ...)
  cols <- cols_data$cols

  header_data <- header_matrix(
    cols = cols,
    cols_new = cols_data$new,
    units = units,
    newline = cols_data$newline,
    bold = cols_data$bold,
    extra = cols_data$extra,
    panel = panel
  )

  cols_tex <- header_matrix_tex(header_data, sizes)

  # column alignments
  align_tex <- form_align(align,names(data))
  open_tabular <- form_open(align_tex)

  # start working on the tabular text
  tab <- make_tabular(data, escape_fun = escape_fun, ... )

  # add hlines
  tab <- tab_add_hlines(tab, add_hlines, sumrows)

  # execute panel insertions
  tab <- tab_panel_insert(tab, panel_insert)

  # notes
  note_data <- tab_notes(notes, escape_fun = escape_fun,  ...)

  row_space <- gluet("\\renewcommand{\\arraystretch}{<sizes$row_space>}")
  col_space <- gluet("\\setlength{\\tabcolsep}{<sizes$col_space>pt} ")

  out <- c(
    sizes$font_size$start,
    col_space,
    start_tpt,
    row_space,
    open_tabular,
    "\\hline",
    span_data$tex,
    cols_tex,
    "\\hline",
    tab,
    "\\hline",
    "\\end{tabular}",
    note_data$t_notes,
    end_tpt,
    note_data$m_notes,
    sizes$font_size$end
  )

  out <- structure(out, class = "stable", stable_file = note_data$output_file)

  if(isTRUE(inspect)) {
    stable_data <- structure(list(), class = "stable_data")
    stable_data$data <- data
    stable_data$cols <- cols_data$cols
    stable_data$nc <- ncol(data)
    stable_data$cols_new <- cols_data$new
    stable_data$cols_tex <- cols_tex
    stable_data$units <- units
    stable_data$tpt_notes <- note_data$t_notes
    stable_data$mini_notes <- note_data$m_notes
    stable_data$notes <- note_data$notes
    stable_data$note_config <- note_data$note_config
    stable_data$panel <- panel
    stable_data$align <- align
    stable_data$tab <- tab
    stable_data$align_tex <- align_tex
    stable_data$sizes <- sizes
    stable_data$span_data <- span_data
    out <- structure(out, stable_data = stable_data)
  }

  out
}

#' @rdname stable
#' @export
stable.pmtable <- function(data, ...) as_stable(data, ...)

#' @rdname stable
#' @export
stable.stable <- function(data, ...) return(data)

#' Create stable from pmtable
#'
#' @param x object to convert to stable
#' @param ... for the `pmtable` method, these are extra named arguments to pass
#' to [stable()]
#' @param wrap if `TRUE`, the stable output will be wrapped in a latex table
#' environment using [st_wrap()]
#' @param wrapw if `TRUE`, the stable output will be wrapped in a latex table
#' environment and the output will be written to [stdout()]; use this along with
#' `results = "asis"` when rendering tables with [rmarkdown::render()]
#'
#' @export
#'
as_stable <- function(x, ...) UseMethod("as_stable")

#' @param long if `TRUE`, render with [stable_long()] to create a longtable;
#' otherwise, by default process with [stable()]
#' @param con passed to [st_wrap()]; used when `wrap` is `TRUE`
#' @rdname as_stable
#' @keywords internal
#' @export
as_stable.pmtable <- function(x, ..., long = FALSE, wrap = FALSE, wrapw = FALSE,
                              con = NULL) {
  up <- list(...)
  replace <- intersect(names(up),names(x))
  if(length(replace) > 0) {
    x[replace] <- up[replace]
    up[replace] <- NULL
  }
  x <- c(x,up)
  valid <- intersect(names(x),stable_argument_names())
  x <- x[valid]

  fun <- ifelse(isTRUE(long), stable_long, stable)

  ans <- do.call(fun, args = x)

  if(isTRUE(wrap) || isTRUE(wrapw)) {
    ans <- st_wrap(ans, con = con)
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
