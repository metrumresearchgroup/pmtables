
start_tpt <- "\\begin{threeparttable}"
end_tpt <- "\\end{threeparttable}"
begin_tn <- "\\begin{tablenotes}[flushleft]"
end_tn <- "\\end{tablenotes}"
hl <- "\\hline"
note_space <- 0.1



#' Create tabular output from an R data frame
#'
#' @param data a data.frame to convert to tabular table
#' @param align an alignment object created by [cols_align], [cols_left],
#' [cols_center], or [cols_right]
#' @param panel character column name to use to section the table; sections will
#' be created from unique values of `data[[panel]]`
#' @param units a named list with unit information; names should correspond to
#' columns in the data frame
#' @param rm_dups character vector of column names where duplicate values will
#' be made blank (overwritten with `""`)
#' @param span a list of objects created with [colgroup]
#' @param span_split not implemented at this time
#' @param notes a character vector of notes to include at the foot of the table;
#' use `r_file` and `output_file` for source code and output file annotations
#' @param hline_at logical or integer vector specifying rows above which an
#' `\hline` will be placed
#' @param hline_from a character column name from which to separate the table
#' with `\hline`; non-duplicated values of `hline_from` will be used to create
#' the split
#' @param sumrows an object created with [sumrow]; identifies summary rows
#' and adds styling
#' @param bold_cols if `TRUE`, table column names are rendered with bold font
#' @param col_rename a `name = value` character vector to translate column names
#' to table names
#' @param col_replace a character vector with the same length as the number of
#' output table columns; use this to completely replace the names (as opposed
#' to one by on editing with `col_rename`)
#' @param row_space relative increase or decrease spacing between rows; use
#' `row_space > <default>` to increase
#' @param col_space absolute column spacing amount (`pt`)
#' @param fontsize for the table (e.g. `normalsize`, `small`, `scriptsize`, etc)
#' @param prime_fun function to prime the data frame to be converted to tabular
#' @param escape_fun a function passed to `prime_fun` that will sanitize column
#' data
#' @param note_config a [noteconf()] object used to configure how table notes
#' are displayed
#' @param r_file the name of the R file containg code to generate the table; the
#' file name will be included in the notes in the table footer
#' @param r_file_label prefix text for `r_file` note
#' @param output_file the name of the output file where the table text will be
#' saved; the file name will be included in the notes in the table footer
#' @param output_file_label prefix text for `output_file` note
#'
#' @export
stable <- function(data,
                   align = cols_left(),
                   panel = rowpanel(col = NULL),
                   units = NULL,
                   rm_dups = NULL,
                   span = NULL,
                   span_split = NULL,
                   notes = NULL,
                   hline_at = NULL,
                   hline_from = NULL,
                   sumrows = NULL,
                   bold_cols = missing(panel),
                   col_rename = NULL,
                   col_replace = NULL,
                   row_space = 1.4,
                   col_space = 5,
                   fontsize = NULL,
                   prime_fun = tab_prime,
                   escape_fun = tab_escape,
                   note_config = noteconf(type = "tpt"),
                   r_file = NULL,
                   r_file_label = getOption("r.file.label","Source code:"),
                   output_file = NULL,
                   output_file_label = getOption("out.file.label","Source file: ")) {

  assert_that(is.data.frame(data))
  assert_that(is.noteconfig(note_config))

  has_panel <- !missing(panel)

  if(has_panel && !is.rowpanel(panel)) {
    panel <- rowpanel(new_names(panel))
  }

  if(inherits(sumrows, "sumrow")) {
    sumrows <- list(sumrows)
  }
  if(!is.null(sumrows)) {
    assert_that(is.list(sumrows))
  }

  add_hlines <- NULL

  if(!is.null(hline_at)) {
    if(is.logical(hline_at)) {
      hline_at <- which(hline_at)
    }
    add_hlines <- c(add_hlines,hline_at)
  }

  if(!is.null(hline_from)) {
    assert_that(is.character(hline_from))
    for(this_col in hline_from) {
      require_col(data,this_col)
      hline_row <- !duplicated(chunk_runs(data[[this_col]]))
      hline_row[1] <- FALSE
      add_hlines <- c(add_hlines, which(hline_row)-1)
    }
  }

  if(is.character(rm_dups)) {
    if(!missing(panel)) {
      paneln <- tidyselect::eval_select(panel$col, data = data)
      panelcol <- names(data)[paneln[1]]
      data <- group_by(data,!!sym(panelcol))
    }
    for(this_col in rm_dups) {
      data <- mutate(
        data,
        {{this_col}} :=  ifelse(
          duplicated(chunk_runs(!!sym(this_col))),
          "",
          !!sym(this_col)
        )
      )
    }
    data <- ungroup(data)
  }

  do_panel <- FALSE
  if(!panel$null) {
    require_col(data,panel$col,context = "panel column input name")
    paneln <- match(panel$col,names(data))
    if(any(is.na(paneln))) {
      stop("panel column not found: ", squote(panel$col), call.=FALSE)
    }
    data[[paneln]] <- replace_na(data[[paneln]],"")
    # check summary rows
    if(!is.null(sumrows)) {
      dep <- map(sumrows, sumrow_depanel_rows)
      dep <- flatten_int(dep)
      dep <- sort(unique(dep))
      data[[paneln]][dep] <- rep(".panel.waiver.", length(dep))
    }
    ins <- panel_by(data, panel)
    data[[panel$col]] <- NULL
    do_panel <- TRUE
  }

  if(!is.null(sumrows)) {
    hline_sums <- map(sumrows, sumrow_get_hline)
    hline_sums <- flatten_int(hline_sums)-1
    add_hlines <- c(add_hlines, hline_sums)
    for(this_sumrow in sumrows) {
      data <- sumrow_add_style(this_sumrow,data)
    }
  }

  cols <- colnames(data)

  # Colgroups ------------------------------------
  do_span_split <- is.colsplit(span_split)
  spans_from_split <- NULL
  if(do_span_split) {
    spans <- find_span_split(cols,span_split)
    if(isTRUE(spans$any)) {
      data <- data[,spans$recol]
      cols <- spans$data$newcol
      spans_from_split <- spans$data
    }
  }

  if(is.null(span)) {
    span <- list()
  } else {
    if(is.colgroup(span)) span <- list(span)
    assert_that(is.list(span))
    span <- map(span, process_colgroup, cols = cols)
  }

  all_span_tex <- NULL

  if(length(span) > 0 || length(spans_from_split) > 0) {
    all_spans <- combine_spans(span, spans_from_split, cols = cols)

    all_span_tex <- map(rev(all_spans), make_span_tex)

    all_span_tex <- flatten_chr(unname(all_span_tex))
  }

  # Units --------------------------------------
  units <- form_unit(units,cols)


  # Work on columns and column names
  if(is.character(col_replace)) {
    if(length(col_replace) != length(cols)) {
      stop(
        "'col_replace' length is not equal to the number of columns in 'data'",
        call.=FALSE
      )
    }
    cols <- col_replace
    col_rename <- NULL
  }

  cols <- esc_underscore(cols)

  cols <- form_cols(
    cols,
    bold = bold_cols,
    relabel = col_rename,
    units = units
  )

  if(is.character(prime_fun)) {
    prime_fun <- get(prime_fun, mode = "function")
  }
  if(is.character(escape_fun)) {
    escape_fun <- get(escape_fun, mode = "function")
  }
  assert_that(is.function(prime_fun))
  assert_that(is.function(escape_fun))
  data <- prime_fun(data, escape_fun)

  # Column alignments -----------------------------
  align_tex <- form_align(align,names(data))
  assert_that(length(align_tex)==ncol(data))
  align_tex <- paste0(align_tex,collapse="")
  open_tabular <- form_open(align_tex)

  # Start working on the tabular text -------------------------
  tab <- make_tabular(data)

  # Add hlines ---------------------------------------
  if(!is.null(add_hlines)) {
    add_hlines <- sort(unique(add_hlines))
    tab[add_hlines] <- paste0(tab[add_hlines], " \\hline")
  }

  # Execute panel insertions ------------------------
  if(do_panel) {
    tab <- insrt_vec(tab, ins$to_insert, where = ins$where)
  }

  # NOTES ----------------------------------------------
  # check behavior: do we want these basenamed or not?
  r_note <- NULL
  out_note <- NULL

  if(is.character(r_file)) {
    r_note <- paste(r_file_label, basename(r_file))
  }

  if(is.character(output_file)) {
    out_note <- paste(output_file_label,basename(output_file))
  }

  notes <- c(notes, r_note, out_note)

  if(!is.null(notes)) {
    if(isTRUE(pt_opts$notes.sanitize)) {
      notes <- escape_fun(notes)
    }
  }

  m_notes <- t_notes <- NULL

  if(note_config$tpt) {
    t_notes <- tpt_notes(notes, note_config)
  } else {
    m_notes <- mini_notes(notes, note_config)
  }
  # END notes --------------------------------------------------

  # Table row and column spacing -------------------
  col_row_sp <- list()
  col_row_sp$start <- "{\\def\\arraystretch{<row_space>}\\tabcolsep=<col_space>pt"
  col_row_sp$end <- "}"
  col_row_sp$start <- gluet(col_row_sp$start)

  # Font size ----------------------------------
  font_size <- list()
  if(is.character(fontsize)) {
    font_size$start <- paste0("{\\", fontsize)
    font_size$end <- "}"
  }

  tab <- c(
    font_size$start,
    col_row_sp$start,
    start_tpt,
    open_tabular,
    "\\hline",
    all_span_tex,
    cols,
    "\\hline",
    tab,
    "\\hline",
    "\\end{tabular}",
    t_notes,
    end_tpt,
    col_row_sp$end,
    m_notes,
    font_size$end
  )

  tab <- structure(tab, class = "stable", stable_file = output_file)

  tab
}
