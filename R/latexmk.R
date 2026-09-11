#' Generate document with `latexmk`
#'
#' @param inputfile Input file name for document (with or without trailing
#'   ".tex")
#' @param name Use this as the base name for output files rather than taking it
#'   from `inputfile`.
#' @param command Which command `latexmk` should call underneath.
#' @param env Passed to `system2()`. If missing, `SOURCE_DATE_EPOCH`, and
#' `FORCE_SOURCE_DATE` are set to make a static document timestamp.
#' @param ... Other arguments passed to `system2()`.
#' @noRd
latexmk <- function(
  inputfile,
  name = NULL,
  command = c("pdflatex", "latex"),
  env = character(),
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

  if (missing(env)) {
    env <- c("SOURCE_DATE_EPOCH=1000000000", "FORCE_SOURCE_DATE=1")
  }

  system2(prog, c(args, shQuote(inputfile)), env = env, ...)
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
