head <- '
\\endhead
\\hline
\\multicolumn{<nc>}{r}{<lt_continued>}
\\endfoot
\\hline
\\endlastfoot
'

ltcaption <- function(macro = NULL, text = NULL, label = NULL) {
  lab <- NULL
  if(is.character(label)) {
    lab <- paste0("\\label{",label,"}")
  }
  cap1 <- "\\caption{"
  cap3 <- "}\\\\"
  cap2 <- NULL
  if(is.character(macro)) {
    if(substr(macro, 1, 1)=="\\") {
      macro <- substr(macro, 2, nchar(macro))
    }
    if(str_detect(macro, "[^a-zA-Z]")) {
      stop(macro, " appears to be invalid for use in latex", call.=FALSE)
    }
    cap2 <- paste0("\\", macro)
  }
  if(is.character(text)) cap2 <- c(cap2, text)
  cap3 <- paste0(lab, cap3)
  paste0(c(cap1,cap2,cap3),collapse = "")
}
#' Create longtable output from an R data frame
#' @inheritParams tab_notes
#' @param ... passed to [stable()]
#' @param inspect fixed to `TRUE` and passed to [stable()]
#' @param cap_macro the name of a macro that will hold caption text; to not lead with
#' `\\` - this will be added for you
#' @param cap_text caption text
#' @param cap_label table label for use in latex document
#'
#'
#' @export
stable_long <- function(note_config = noteconf(type="minipage"), ...,
                        inspect = TRUE, cap_macro = NULL, cap_text = NULL,
                        cap_label = NULL) {

  assert_that(note_config$type=="minipage")

  x <- stable( note_config = note_config, inspect = TRUE, ...)
  x <- get_stable_data(x)

  cap <- ltcaption(cap_macro, cap_text, cap_label)

  start <- paste0("\\begin{longtable}{", x$align_tex, "}")
  end <- "\\end{longtable}"

  extra_height <- max(x$sizes$lt_row_space,0)
  extra <- gluet("\\setlength{\\extrarowheight}{<extra_height>em}")

  nc <- x$nc
  lt_continued <- pt_opts$get("longtable.foot")
  head <- gluet(head)

  longtab <- c(
    "{\\normalsize",
    "\\setlength{\\extrarowheight}{0.3em}",
    start,
    head,
    cap,
    "\\hline",
    x$cols_tex,
    "\\endfirsthead",
    "\\hline",
    x$cols_tex,
    x$units_tex,
    "\\hline",
    "\\endhead",
    x$units_tex,
    "\\hline",
    x$tab,
    "\\hline",
    "\\end{longtable}",
    "\\setlength{\\extrarowheight}{0em}",
    "\\begin{center}",
    x$mini_notes,
    "\\end{center}",
    "}" # Ends
  )
  structure(longtab, class = c("stable_long", "stable"))
}
