
#' Form table notes
#'
#' @inheritParams stable
#' @param note_config a [noteconf()] object used to configure how table notes
#' are displayed; ; see also [st_noteconf()]
#' @param r_file the name of the R file containg code to generate the table; the
#' file name will be included in the notes in the table footer; ; see also
#' [st_files()]
#' @param output_file the name of the output file where the table text will be
#' saved; the file name will be included in the notes in the table footer; see
#' also [st_files()]
#' @param ... not used
#'
#'@export
tab_notes <- function(notes, escape_fun = tab_escape,
                      note_config = noteconf(type = "tpt"),
                      r_file = NULL, output_file = NULL, ...) {

  assert_that(is.noteconfig(note_config))

  r_file_label <-  getOption("r.file.label","Source code: ")

  output_file_label <-  getOption("out.file.label","Source file: ")

  r_note <- out_note <- NULL

  if(is.character(r_file)) {
    r_note <- paste(r_file_label, basename(r_file))
  }

  if(is.character(output_file)) {
    out_note <- paste(output_file_label,basename(output_file))
  }

  notes <- c(notes, r_note, out_note)

  if(isTRUE(pt_opts$notes.sanitize)) {
    assert_that(is.character(notes) || is.null(notes))
    notes <- escape_fun(notes)
  }

  m_notes <- t_notes <- NULL

  if(note_config$tpt) {
    t_notes <- tpt_notes(notes, note_config)
  } else {
    m_notes <- mini_notes(notes, note_config)
  }

  # END notes --------------------------------------------------
  list(
    m_notes = m_notes, t_notes = t_notes, config = note_config,
    notes = notes, r_file = r_file, output_file = output_file
  )

}


#' Configure table notes
#'
#' @param width the width (as fraction of linewidth) is `minipage` notes
#' @param type put table notes in either the `tablenotes` section of
#' threeparttable (`tpt`) or in a minipage below the table (`minipage`)
#' @param hline where to include horizintal lines
#' @param table_skip vertical space (`cm`) between the last row of the table
#' and the hline (if any) at the top of the table notes
#' @param note_skip vertical space (`cm`) between the top hline of the table
#' notes and the first note line
#' @param linespread spacing between note lines for `minipage` notes
#' @param hline_pt the point size for hlines
#' @param note_sp spacing between note lines for `tpt` notes
#'
#' @export
noteconf <- function(width = 0.8,
                     type = c("tpt", "minipage"),
                     hline = c("top", "bottom", "both", "none"),
                     table_skip = 0.67, note_skip = 0.02,
                     linespread = 1.1, hline_pt = 0.4,
                     note_sp = 0.1) {

  hline <- match.arg(hline)

  hline1 <- all(hline %in% c("top",    "both"), hline != "none")
  hline2 <- all(hline %in% c("bottom", "both"), hline != "none")

  type <- match.arg(type)

  ans <- list(
    width = width,
    linespread = linespread,
    hline_pt = hline_pt,
    table_skip = table_skip,
    note_skip = note_skip,
    hline = hline,
    type = type,
    tpt = type == "tpt",
    hline1 = hline1,
    hline2 = hline2
  )
  structure(ans, class = "noteconfig")
}

is.noteconfig <- function(x) inherits(x, "noteconfig")

mini_notes <- function(notes, x) {
  hline1 <- hline2 <- paste0("\\rule{", 1, "\\linewidth}{",x$hline_pt, "pt}")
  tskip <- paste0("\\vskip ", x$table_skip, "cm")
  nskip <- paste0("\\vskip ", x$note_skip,  "cm")
  if(!x$hline1) {
    hline1 <- NULL
  }
  if(!x$hline2) {
    hline2 <- NULL
  }
  notes <- paste(notes, "\\newline")
  out <- c(
    tskip,
    paste0("\\begin{minipage}{",x$width,"\\linewidth}"),
    paste0("\\linespread{", x$linespread,"}\\selectfont"),
    hline1,
    nskip,
    notes,
    hline2,
    paste0("\\end{minipage}")
  )
  out
}

tpt_notes <- function(notes,x) {
  note_sp <- force(x$note_sp)
  c(
    "\\begin{tablenotes}[flushleft]",
    gluet("\\setlength\\itemsep{<note_sp>em}"),
    paste0("\\item ", notes),
    "\\end{tablenotes}"
  )
}

