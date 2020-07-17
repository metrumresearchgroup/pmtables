
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

