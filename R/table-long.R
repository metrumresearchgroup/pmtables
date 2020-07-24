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

  tab <- x$tab
  start <- paste0("\\begin{longtable}{", x$align_tex, "}")
  end <- "\\end{longtable}"
  nc <- x$nc
  longtab <- c(
    "{",
    "\\renewcommand*{\\arraystretch}{1.4}",
    start,
    gluet(head),
    "\\hline",
    paste0(x$cols,collapse = " & "),
    "\\endfirsthead",
    "\\hline",
    paste0(x$cols,collapse = " & "),
    x$units,
    "\\hline",
    "\\endhead",
    x$units,
    "\\hline",
    tab,
    "\\hline",
    "\\end{longtable}",
    "\\begin{center}",
    x$notes,
    "\\end{center}",
    "}"
  )

  structure(longtab, class = "stable_long")
}



