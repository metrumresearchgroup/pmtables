#' Read and parse a tex glossary file
#'
#' @param file path to the tex glossary file.
#'
#' @return
#' A named list of glossary definitions with class `tex_glossary` and
#' `glossary`. The list names are acronym entry labels (i.e., what you would
#' pass to `\gls{}` when writing a tex document).
#'
#'
#' @details
#' The glossary file is expected to contain acronym entries with the form
#'
#' ```
#' \newacronym{label}{abbreviation}{definition}
#' ```
#'
#' or
#'
#' ```
#' \newacronym[options]{label}{abbreviation}{definition}
#' ```
#'
#' @examples
#' file <- system.file("tex", "glossary.tex", package = "pmtables")
#'
#' x <- read_glossary(file)
#'
#' names(x)
#'
#' x$WT
#'
#' x$SC
#'
#' @export
read_glossary <- function(file) {
  if(!file.exists(file)) {
    abort(glue("glossary file {file} does not exist."))
  }
  txt <- readLines(file, warn = FALSE)
  if(!length(txt)) abort("glossary file was empty.")
  ans <- parse_glossary(txt)
  class(ans) <- c("tex_glossary", "glossary", "list")
  ans
}

#' @rdname read_glossary
#' @export
as_glossary <- function(glossary, ...) {
  if(!is.list(glossary) || !is_named(glossary)) {
    abort("`glossary` must be a named list")
  }
  type_chr <- vapply(glossary, inherits, what = "character", FUN.VALUE = TRUE)
  len <- vapply(glossary, length, FUN.VALUE = 1)
  if(!all(type_chr & len==1)) {
    abort("Each `glossary` entry must have type character and length 1.")
  }
  glossary <- Map(glossary, names(glossary), f = function(def, label) {
    as_glossary_entry(c(definition = def, abbreviation = label))
  })
  abbrev <- new_names(enquos(...))
  for(a in seq_along(abbrev)) {

  }
  class(glossary) <- c("tex_glossary", "glossary", "list")
  glossary
}

parse_glossary <- function(txt) {
  txt <- trimws(txt)
  txt <- txt[grepl("^\\\\newacronym", txt)]
  m <- regexec("\\{(.+)\\}\\{(.+)\\}\\{(.+)\\}.*$", txt)
  parsed <- regmatches(txt, m)
  if(!length(parsed)) {
    abort("no acronym entries were found in `file`.")
  }
  if(!all(vapply(parsed, length, 1L)==4)) {
    abort("there was a problem parsing the glossary file.")
  }
  label <- vapply(parsed, FUN = "[", 2L, FUN.VALUE = "a")
  data <- lapply(parsed, FUN = "[", c(3L, 4L))
  data <- lapply(data, as_glossary_entry)
  data <- lapply(data, setNames, c("abbreviation", "definition"))
  names(data) <- label
  data
}

as_glossary_entry <- function(x) {
  x <- as.list(x)
  class(x) <- c("glossary_entry", "list")
  x
}


#' @export
print.glossary_entry <- function(x, ...) {
  print(paste0(x$definition, " (", x$abbreviation, ")"))
}
