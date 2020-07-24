head <- '
\\endhead
\\hline
\\multicolumn{<nc>}{r}{{\\footnotesize {\\it continued ...}}}
\\endfoot
\\hline
\\endlastfoot
'

#' Create longtable output from an R data frame
#'
#' @param ... passed to [stable()]
#' @param debug_data fixed to `TRUE` and passed to [stable()]
#'
#'
#' @export
stable_long <- function(...,debug_data = TRUE) {

  x <- stable(..., debug_data = TRUE)
  x <- get_debug_data(x)

  start <- paste0("\\begin{longtable}{", x$align_tex, "}")
  end <- "\\end{longtable}"
  nc <- x$nc
  longtab <- c(
    "{\\normalsize",
    "\\setlength{\\extrarowheight}{0.3em}",
    start,
    gluet(head),
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



