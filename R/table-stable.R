
start_tpt <- "\\begin{threeparttable}"
end_tpt <- "\\end{threeparttable}"
begin_tn <- "\\begin{tablenotes}[flushleft]"
end_tn <- "\\end{tablenotes}"
hl <- "\\hline"
note_sp <- 0.1
stretch_start <- "{\\def\\arraystretch{<row_stretch>}\\tabcolsep=<tab_sep>pt"
stretch_end <- "}"


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
#' @param span_split_level not implemented at this time
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
#' @param row_stretch increase or decrease spacing between rows
#' @param tab_sep set column padding level
#' @param note_sp separation for table notes
#' @param fontsize for the table (e.g. `normalsize`, `small`, `scriptsize`, etc)
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
                   panel = NULL,
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
                   row_stretch = 1.4,
                   tab_sep = 5,
                   note_sp = 0.1,
                   fontsize = NULL,
                   r_file = NULL,
                   r_file_label = getOption("r.file.label","source code:"),
                   output_file = NULL,
                   output_file_label = getOption("out.file.label","source file")) {

  assert_that(is.data.frame(data))

  if(missing(panel) && (length(group_vars(data)) > 0)) {
    panel <- group_vars(data)[1]
    panel <- sym(panel)
  }

  if(inherits(sumrows, "sumrow")) sumrows <- list(sumrows)
  if(!is.null(sumrows)) assert_that(is.list(sumrows))

  add_hlines <- NULL

  if(!is.null(hline_at)) {
    if(is.logical(hline_at)) hline_at <- which(hline_at)
    add_hlines <- c(add_hlines,hline_at)
  }

  if(!is.null(hline_from)) {
    assertthat::assert_that(is.character(hline_from))
    for(this_col in hline_from) {
      require_col(data,this_col)
      hline_row <- !duplicated(chunk_runs(data[[this_col]]))
      hline_row[1] <- FALSE
      add_hlines <- c(add_hlines, which(hline_row)-1)
    }
  }

  if(!is.null(sumrows)) {
    hline_sums <- map(sumrows, sumrow_get_hline)
    hline_sums <- flatten_int(hline_sums)-1
    add_hlines <- c(add_hlines, hline_sums)

    for(this_sumrow in sumrows) {
      data <- sumrow_add_style(this_sumrow,data)
    }
  }

  if(is.character(rm_dups)) {
    if(!missing(panel)) {
      paneln <- tidyselect::eval_select(enquo(panel),data = data)
      panelcol <- names(data)[paneln[1]]
      data <- group_by(data,!!sym(panelcol))
    }
    for(this_col in rm_dups) {
      data <- mutate(
        data,
        {{this_col}} :=  ifelse(
          duplicated(chunk_runs(!!sym(this_col))), "", paste0("{", !!sym(this_col), "}")
        )
      )
    }
    data <- ungroup(data)
  }


  do_panel <- FALSE
  panel_prefix <- ""
  if(!missing(panel)) {
    paneln <- tidyselect::eval_select(enquo(panel),data = data)
    panel <- names(data)[paneln[1]]
    panel_prefix <- names(paneln)[1]
    require_col(data,panel,context = "panel column input name")
    ins <- panel_by(data,panel,prefix = panel_prefix)
    data[[panel]] <- NULL
    do_panel <- TRUE
  }

  nc <- ncol(data)
  nr <- nrow(data)
  cols <- colnames(data)

  do_span_split <- is.colgroup(span_split)
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
    span <- map(span,process_colgroup, cols = names(data))
  }

  all_span_tex <- NULL

  if(length(span) > 0 || length(spans_from_split) > 0) {
    all_spans <- combine_spans(span, spans_from_split, cols = names(data))

    all_span_tex <- map(rev(all_spans), make_span_tex)

    all_span_tex <- flatten_chr(unname(all_span_tex))
  }

  if(is.character(col_replace)) {
    if(length(col_replace) != length(cols)) {
      stop("'col_replace' length is not equal to the number of columns in 'data'", call.=FALSE)
    }
    cols <- col_replace
    col_rename <- NULL
  }

  units <- form_unit(units,names(data))

  cols <- form_cols(
    cols,
    bold = bold_cols,
    relabel = enquo(col_rename),
    units = units
  )

  align_tex <- form_align(align,names(data))
  assert_that(length(align_tex)==ncol(data))
  align_tex <- paste0(align_tex,collapse="")

  if(is.character(r_file)) {
    r_note <- paste(r_file_label, basename(r_file))
    notes <- c(notes,r_note)
  }

  if(is.character(output_file)) {
    out_note <- paste(output_file_label,basename(output_file))
    notes <- c(notes,out_note)
  }

  tab <- make_tabular(data)

  if(!is.null(add_hlines)) {
    add_hlines <- sort(unique(add_hlines))
    tab[add_hlines] <- paste0(tab[add_hlines], " \\hline")
  }

  if(do_panel) {
    tab <- insrt_vec(tab, ins$to_insert, where = ins$where)
  }

  open <- form_open(align_tex)

  if(!is.null(notes)) {
    notes <- form_notes(notes, note_sp)
  }

  stretch_start <- gluet(stretch_start)

  text <- list()
  if(is.character(fontsize)) {
    text$start <- paste0("{\\", fontsize)
    text$end <- "}"
  }

  tab <- c(
    text$start,
    stretch_start,
    start_tpt,
    open,
    "\\hline",
    all_span_tex,
    cols,
    "\\hline",
    tab,
    "\\hline",
    "\\end{tabular}",
    notes,
    end_tpt,
    stretch_end,
    text$end
  )
  tab <- structure(tab, stable_file = output_file)
  tab
}