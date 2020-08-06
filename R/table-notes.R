
#' Form table notes
#'
#' @inheritParams stable
#' @param note_config a [noteconf()] object used to configure how table notes
#' are displayed; ; see also [st_noteconf()]
#' @param r_file the name of the R file containg code to generate the table; the
#' file name will be included in the notes in the table footer; ; see also
#' [st_files()]
#' @param r_file_label a prefix for `r_file`
#' @param output_file the name of the output file where the table text will be
#' saved; the file name will be included in the notes in the table footer; see
#' also [st_files()]
#' @param output_file_label a prefix for `output_file`
#' @param ... not used
#'
#'@export
tab_notes <- function(notes, escape_fun = tab_escape,
                      note_config = noteconf(type = "tpt"),
                      r_file = getOption("mrg.script", NULL),
                      r_file_label = "Source code: ",
                      output_file = NULL,
                      output_file_label = "Source file: ",
                      ...) {

  assert_that(is.noteconfig(note_config))

  file_notes <- form_file_notes(r_file, r_file_label, output_file,
                                output_file_label)

  notes <- c(notes, file_notes)

  if(note_config$sanitize) {
    assert_that(is.character(notes) || is.null(notes))
    notes <- escape_fun(notes, escape = note_config$escape)
  }

  m_notes <- t_notes <- NULL

  if(length(notes) > 0) {
    if(note_config$tpt) {
      t_notes <- tpt_notes(notes, note_config)
    } else {
      m_notes <- mini_notes(notes, note_config)
    }
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
#' @param hline_pt the point size for hlines
#' @param sanitize if `TRUE`, notes will be sanitized
#' @param escape vector of characters to escape in notes
#'
#' @export
noteconf <- function(width = 0.8,
                     type = c("tpt", "minipage"),
                     hline = c("top", "bottom", "both", "none"),
                     table_skip = 0.67, note_skip = 0.02,
                     hline_pt = 0.4, sanitize = TRUE, escape = "_") {

  hline <- match.arg(hline)

  hline1 <- all(hline %in% c("top",    "both"), hline != "none")
  hline2 <- all(hline %in% c("bottom", "both"), hline != "none")

  type <- match.arg(type)

  assert_that(is.character(escape))

  ans <- list(
    width = width,
    hline_pt = hline_pt,
    table_skip = table_skip,
    note_skip = note_skip,
    hline = hline,
    type = type,
    tpt = type == "tpt",
    hline1 = hline1,
    hline2 = hline2,
    escape = escape,
    sanitize = isTRUE(sanitize)
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
    paste0("\\linespread{1.1}\\selectfont"),
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
    paste0("\\item ", notes),
    "\\end{tablenotes}"
  )
}

form_file_notes <- function(r_file, r_file_label, output_file,
                            output_file_label, ...) {
  r_note <- output_note <- NULL
  if(is.character(r_file)) {
    r_note <- paste0(r_file_label, r_file)
  }
  if(is.character(output_file)) {
    output_note <- paste0(output_file_label, output_file)
  }
  c(r_note, output_note)
}

valid_file_notes_fun <- function(x) {
  identical(formals(x), formals(form_file_notes))
}

