form_notes <- function(notes,note_sp) {
  note_sp <- force(note_sp)
  c(
    "\\begin{tablenotes}[flushleft]",
    gluet("\\setlength\\itemsep{<note_sp>em}"),
    paste0("\\item ", notes),
    "\\end{tablenotes}"
  )
}
