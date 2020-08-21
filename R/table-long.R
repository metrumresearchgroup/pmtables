head <- '
\\endhead
\\hline
\\multicolumn{<nc>}{r}{<lt_continue>}
\\endfoot
\\hline
\\endlastfoot
'

ltcaption <- function(macro = NULL, text = NULL, label = NULL) {
  if(all(is.null(macro),is.null(text), is.null(label))) {
    return(NULL)
  }
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

longtable_notes <- function(notes) {
  if(is.null(notes)) return(NULL)
  c("\\begin{center}",notes,"\\end{center}")
}

#' Create longtable output from an R data frame
#' @inheritParams tab_notes
#' @param data data frame passed to [stable()]
#' @param ... passed to [stable()]
#' @param inspect fixed to `TRUE` and passed to [stable()]
#' @param lt_cap_macro the name of a macro that will hold caption text; to not lead with
#' `\\` - this will be added for you
#' @param lt_cap_text caption text
#' @param lt_cap_label table label for use in latex document
#' @param lt_continue longtable continuation message
#'
#'
#' @export
stable_long <- function(data,
                        note_config = noteconf(type="minipage"),
                        inspect = TRUE,
                        lt_cap_macro = NULL,
                        lt_cap_text = NULL,
                        lt_cap_label = NULL,
                        lt_continue = "\\footnotesize{continued on next page}",
                        ...) {

  assert_that(note_config$type=="minipage")

  x <- stable(data = data, note_config = note_config, inspect = TRUE, ...)
  x <- get_stable_data(x)

  cap <- ltcaption(lt_cap_macro, lt_cap_text, lt_cap_label)

  start <- paste0("\\begin{longtable}{", x$align_tex, "}")
  end <- "\\end{longtable}"

  extra_height <- x$sizes$lt_row_space
  extra_row_height <- gluet("\\setlength{\\extrarowheight}{<extra_height>em}")
  row_space <- gluet("\\renewcommand{\\arraystretch}{<x$sizes$row_space>}")
  col_space <- gluet("\\setlength{\\tabcolsep}{<x$sizes$col_space>pt} ")

  nc <- x$nc
  head <- gluet(head)

  lt_notes <- longtable_notes(x$mini_notes)

  longtab <- c(
    "{\\normalsize",
    row_space,
    col_space,
    extra_row_height,
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
    lt_notes,
    "}" # Ends
  )
  out <- structure(longtab, class = c("stable_long", "stable"))

  if(isTRUE(inspect)) {
    stable_data <- x
    stable_data$tab  <- longtab
    stable_data$lt_notes <- lt_notes
    out <- structure(out, stable_data = stable_data)
  }

  out
}
