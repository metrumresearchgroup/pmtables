
start_tpt <- "\\begin{threeparttable}"
end_tpt <- "\\end{threeparttable}"
begin_tn <- "\\begin{tablenotes}[flushleft]"
end_tn <- "\\end{tablenotes}"
hl <- "\\hline"
note_space <- 0.1

triage_data <- function(data) {
  assert_that(is.data.frame(data))
  data <- ungroup(data)
  fct <- map_lgl(data, is.factor)
  data <- modify_if(data, fct, as.character)
  data
}


#' Create tabular output from an R data frame
#'
#' @param data a data.frame to convert to tabular table; see also [st_new()]
#' @param align an alignment object created by [cols_align()], [cols_left()],
#' [cols_center()], or [cols_right()]; see also [st_align()]
#' @param panel character column name to use to section the table; sections will
#' be created from unique values of `data[[panel]]`; see also [st_panel()]
#' @param units a named list with unit information; names should correspond to
#' columns in the data frame
#' @param clear_reps character vector of column names where duplicate values will
#' be made blank (overwritten with `""`); ; see also [st_clear_reps()]
#' @param notes a character vector of notes to include at the foot of the table;
#' use `r_file` and `output_file` for source code and output file annotations;
#' see also [st_notes()]
#' @param sizes an object returned from [tab_size()]
#' @param sumrows an object created with [sumrow()]; identifies summary rows
#' and adds styling; see also [st_sumrow()]
#' @param col_bold if `TRUE`, table column names are rendered with bold font
#' @param col_rename a `name = value` character vector to translate column names
#' to table names; ; see also [st_rename()]
#' @param col_blank a character vector of column names that will not be printed
#' in the table header; see also [st_blank()]
#' @param col_replace a character vector with the same length as the number of
#' output table columns; use this to completely replace the names (as opposed
#' to one by on editing with `col_rename`)
#' @param col_split a string that is used to split column labels into tag
#' (on the left) or name (on the right); if supplied, then `col_split` will be
#' used to remove the tag; for example, a column named `x.WT` would be renamed
#' `WT` if `col_split` was set to `.`
#' @param escape_fun a function passed to `prime_fun` that will sanitize column
#' data
#' @param inspect if `TRUE`, extra information is attached to the output
#' as an attribute called `stable_data`; see [get_stable_data()]
#' @param ... passed to other functions: [tab_hlines()], [tab_spanners()],
#' [tab_notes()], and [make_tabular()]
#'
#' @examples
#' data <- ptdata()
#'
#' a <- stable(data, r_file = "example.R", output_file = "output.tex")
#'
#' b <- stable(data, panel = "STUDYf")
#'
#' c <- stable(data, span = colgroup("Covariates", STUDYf:ALB))
#'
#' @export
stable <- function(data,
                   align = cols_left(),
                   panel = rowpanel(col = NULL),
                   units = NULL,
                   clear_reps = NULL,
                   notes = NULL,
                   sizes = tab_size(),
                   sumrows = NULL,
                   col_bold = NULL,
                   col_rename = NULL,
                   col_blank = NULL,
                   col_replace = NULL,
                   col_split = NULL,
                   escape_fun = tab_escape,
                   inspect = FALSE,
                   ... ) {

  data <- triage_data(data)

  assert_that(inherits(sizes, "from_tab_sizes"))

  has_panel <- !missing(panel)
  has_sumrows <- !is.null(sumrows)

  if(has_panel && !is.rowpanel(panel)) {
    panel <- rowpanel(new_names(panel))
  }

  if(missing(col_bold) && panel$null) {
    col_bold <- TRUE
  }

  if(inherits(sumrows, "sumrow")) {
    sumrows <- list(sumrows)
  }

  if(!is.null(sumrows)) {
    assert_that(is.list(sumrows))
  }

  add_hlines <- tab_hlines(data, ...)

  data <- do_clear_reps(data, clear_reps, panel)

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
    hline_sums_top <- flatten_int(hline_sums)-1
    hline_sums_bot <- hline_sums_top + 1
    hline_sums_bot <- hline_sums_bot[hline_sums_bot != nrow(data)]
    add_hlines <- c(add_hlines, hline_sums_top, hline_sums_bot)
    for(this_sumrow in sumrows) {
      data <- sumrow_add_style(this_sumrow,data)
    }
  }

  cols <- colnames(data)

  # Colgroups ------------------------------------
  all_span_tex <- col_spanners(..., cols = cols)

  # Units --------------------------------------
  units_tex <- form_unit(units,cols)

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
  cols_new <- rename_cols(cols, relabel = col_rename, blank = col_blank)
  if(!is.null(col_split)) {
    split_cols <- str_split(cols_new, fixed(col_split), n = 2)
    cols_new <- map_chr(split_cols, last)
  }
  cols_tex <- form_tex_cols(cols_new, col_bold, units)

  # Column alignments -----------------------------
  align_tex <- form_align(align,names(data))

  open_tabular <- form_open(align_tex)

  # Start working on the tabular text -------------------------
  tab <- make_tabular(data, escape_fun = escape_fun, ... )

  # Add hlines ---------------------------------------
  if(!is.null(add_hlines)) {
    add_hlines <- sort(unique(add_hlines))
    tab[add_hlines] <- paste0(tab[add_hlines], " \\hline")
    if(has_sumrows) {
      hlinex <- map(sumrows, sumrow_get_hlinex2)
      above <- sort(unique(flatten_int(hlinex)-1))
      below <- above + 1
      tab[above] <- paste0(tab[above], " \\hline")
      tab[below] <- paste0(tab[below], " \\hline")
    }
  }

  # Execute panel insertions ------------------------
  if(do_panel) {
    tab <- insrt_vec(tab, ins$to_insert, where = ins$where)
  }

  # NOTES ----------------------------------------------
  # check behavior: do we want these basenamed or not?

  note_data <- tab_notes(notes, escape_fun = escape_fun,  ...)

  out <- c(
    sizes$font_size$start,
    sizes$col_row_sp$start,
    start_tpt,
    open_tabular,
    "\\hline",
    all_span_tex,
    cols_tex,
    units_tex,
    "\\hline",
    tab,
    "\\hline",
    "\\end{tabular}",
    note_data$t_notes,
    end_tpt,
    sizes$col_row_sp$end,
    note_data$m_notes,
    sizes$font_size$end
  )

  out <- structure(out, class = "stable", stable_file = note_data$output_file)

  if(isTRUE(inspect)) {
    stable_data <- structure(list(), class = "stable_data")
    stable_data$data <- data
    stable_data$cols <- cols
    stable_data$nc <- ncol(data)
    stable_data$cols <- cols
    stable_data$cols_new <- cols_new
    stable_data$cols_tex <- cols_tex
    stable_data$units <- units
    stable_data$units_tex <- units_tex
    stable_data$tpt_notes <- note_data$t_notes
    stable_data$mini_notes <- note_data$m_notes
    stable_data$notes <- note_data$notes
    stable_data$note_config <- note_data$note_config
    stable_data$panel <- panel
    stable_data$align <- align
    stable_data$tab <- tab
    stable_data$align_tex <- align_tex
    out <- structure(out, stable_data = stable_data)
  }

  out
}
