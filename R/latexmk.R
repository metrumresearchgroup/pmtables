#' Generate document with `latexmk`
#'
#' @param inputfile Input file name for document (with or without trailing
#'   ".tex")
#' @param name Use this as the base name for output files rather than taking it
#'   from `inputfile`.
#' @param command Which command `latexmk` should call underneath.
#' @param ... Arguments passed to `system2()`.
#' @noRd
latexmk <- function(
  inputfile,
  name = NULL,
  command = c("pdflatex", "latex"),
  ...
) {
  command <- match.arg(command)

  prog <- "latexmk"
  if (!nzchar(Sys.which(prog))) {
    rlang::abort(paste(prog, "is required but not in PATH"))
  }

  args <- c("-halt-on-error", "-interaction=nonstopmode", "-g")
  if (identical(command, "pdflatex")) {
    args <- c(args, "-pdf")
  } else if (identical(command, "latex")) {
    args <- c(args, "-latex")
  } else {
    rlang::abort(paste("bug: unknown command:", command))
  }

  if (!is.null(name)) {
    args <- c(args, paste0("-jobname=", shQuote(name)))
  }
  env <- c("SOURCE_DATE_EPOCH=1000000000", "FORCE_SOURCE_DATE=1")
  system2(prog, c(args, shQuote(inputfile)), ..., env = env)
}

warn_ntex <- function(call = rlang::caller_env()) {
  rlang::warn(
    c(
      "'ntex' argument is ignored.",
      "i" = "Output is built with `latexmk`, which reruns the tex engine as needed."
    ),
    call = call
  )
}
