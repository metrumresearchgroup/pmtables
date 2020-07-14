
start_tpt <- "\\begin{threeparttable}"
end_tpt <- "\\end{threeparttable}"
begin_tn <- "\\begin{tablenotes}[flushleft]"
end_tn <- "\\end{tablenotes}"
hl <- "\\hline"
note_sp <- 0.1
stretch_start <- "{\\def\\arraystretch{<row_stretch>}\\tabcolsep=<tab_sep>pt"
stretch_end <- "}"



#' @export
stable <- function(data,
                   align = 'l',
                   panel = NULL,
                   units = NULL,
                   rm_dups = NULL,
                   span = NULL,
                   span_split = NULL,
                   span_split_level = 1,
                   notes = NULL,
                   hline_at = NULL,
                   hline_from = NULL,
                   bold_cols = missing(panel),
                   col_rename = NULL,
                   col_replace = NULL,
                   gather = FALSE,
                   row_stretch = 1,
                   tab_sep = 5,
                   note_sp = 0.1,
                   r_file = NULL,
                   r_file_label = getOption("r.file.label","source code:"),
                   output_file = NULL,
                   output_file_label = getOption("out.file.label","source file")) {

  if(missing(panel) && (length(group_vars(data)) > 0)) {
    panel <- group_vars(data)[1]
    panel <- sym(panel)
  }

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

  do_span_split <- is.character(span_split)
  spans_from_split <- NULL
  if(do_span_split) {
    spans <- find_span_split(cols,sep = span_split, gather = gather,
                             level = span_split_level)
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
  cols <- form_cols(cols, bold = bold_cols,
                    relabel = enquo(col_rename), units = units)

  if(length(align)==1L && nchar(align) == 1L) {
    align <- rep(align,nc)
    align <- paste0(align, collapse="")
  }

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
    tab[add_hlines] <- paste0(tab[add_hlines], "\\hline")
  }

  if(do_panel) {
    tab <- insrt_vec(tab, ins$to_insert, where = ins$where)
  }

  open <- form_open(align)

  if(!is.null(notes)) notes <- form_notes(notes, note_sp)

  stretch_start <- gluet(stretch_start)
  #stretch_start <- gluet("{\\renewcommand*{\\arraystretch}{<row_stretch>}")

  tab <- c(
    stretch_start,
    start_tpt,

    open,
    hl,
    all_span_tex,
    cols,
    hl,
    tab,
    hl,
    "\\end{tabular}",
    notes,
    end_tpt,
    stretch_end
  )
  tab <- structure(tab, stable_file = output_file)
  tab
}
