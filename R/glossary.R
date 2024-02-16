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
#' @seealso [as_glossary()]
#' @export
read_glossary <- function(file) {
  if(!file.exists(file)) {
    abort(glue("Glossary file {file} does not exist."))
  }
  txt <- readLines(file, warn = FALSE)
  if(!length(txt)) abort("The glossary file was empty.")
  ans <- parse_glossary(txt)
  class(ans) <- c("tex_glossary", "glossary", "list")
  ans
}

#' Coerce a named list to glossary
#'
#' @param x a named list.
#' @param ... unquoted `new abbreviation = old abbreviation` pairs.
#' @seealso [read_glossary()]
#'
#' @examples
#'
#' l <- list(VPC = "visual predictive check", tz = "timezone")
#'
#' as_glossary(l)
#'
#' as_glossary(l, tzone = tz)
#'
#' @export
as_glossary <- function(x, ...) {
  if(!is.list(x) || !is_named(x)) {
    abort("`x` must be a named list")
  }
  type_chr <- vapply(x, inherits, what = "character", FUN.VALUE = TRUE)
  len <- vapply(x, length, FUN.VALUE = 1)
  if(!all(type_chr & len==1)) {
    abort("Every `glossary` entry must have type character and length 1.")
  }
  glossary <- Map(x, names(x), f = function(def, label) {
    data <- c(definition = def, abbreviation = label)
    as_glossary_entry(data)
  })
  abbrev <- new_names(enquos(...))
  for(a in seq_along(abbrev)) {
    glossary[[abbrev[a]]]$abbreviation <- names(abbrev[a])
  }
  class(glossary) <- c("tex_glossary", "glossary", "list")
  glossary
}

#' @rdname read_glossary
#' @export
simplify_abbrev <- function(x, ...) {
  require_glossary(x)
  items <- new_names(enquos(...))
  abort_bad_glo_labels(x, items)
  for(m in seq_along(items)) {
    x[[items[[m]]]]$abbreviation <- names(items[m])
  }
  x
}

parse_glossary <- function(txt) {
  txt <- trimws(txt)
  txt <- txt[grepl("^\\\\newacronym", txt)]
  m <- regexec("\\{(.+)\\}\\{(.+)\\}\\{(.+)\\}.*$", txt)
  parsed <- regmatches(txt, m)
  if(!length(parsed)) {
    abort("No acronym entries were found in `file`.")
  }
  if(!all(vapply(parsed, length, 1L)==4)) {
    abort("There was a problem parsing the glossary file.")
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

abort_bad_glo_labels <- function(x, what) {
  if(!all(what %in% names(x))) {
    bad <- setdiff(what, names(x))
    names(bad) <- rep("x", length(bad))
    msg <- c("Requested definitions not found in glossary object", bad)
    abort(msg)
  }
}

#' Return formatted notes from a tex glossary file or glossary object
#'
#' @param x path to a tex glossary file, a glossary object, or a list that can
#' be coerced to a glossary object with [as_glossary()].
#'
#' @inheritParams read_glossary
#' @inheritParams st_notes_glo
#'
#' @examples
#' file <- system.file("tex", "glossary.tex", package = "pmtables")
#'
#' glossary_notes(file, WT, CRCL)
#'
#' @seealso [st_notes_glo()], [read_glossary()]
#' @export
glossary_notes <- function(x, ...) UseMethod("glossary_notes")

#' @rdname glossary_notes
#' @export
glossary_notes.character <- function(x, ...) {
  x <- read_glossary(x)
  glossary_notes(x, ...)
}

#' @rdname glossary_notes
#' @export
glossary_notes.list <- function(x, ...) {
  x <- as_glossary(x)
  glossary_notes(x, ...)
}

#' @rdname glossary_notes
#' @export
glossary_notes.glossary <- function(x, ..., sep = ": ", collapse = "; ",
                                    labels = NULL) {
  labels <- cvec_cs(labels)
  labels <- c(new_names(enquos(...)), labels)
  if(!length(labels)) labels <- names(x)
  build_glossary_notes(x, labels, sep, collapse)
}

build_glossary_notes <- function(glossary, labels, sep, collapse) {
  abort_bad_glo_labels(glossary, labels)
  glossary <- glossary[labels]
  abb <- vapply(glossary, "[[", "abbreviation", FUN.VALUE = "a")
  notes <- lapply(glossary, "[[", "definition")
  notes <- paste0(abb, sep, notes)
  if(is.character(collapse)) {
    notes <- paste0(notes, collapse = collapse)
  }
  notes
}

#' @export
print.glossary_entry <- function(x, ...) {
  print(paste0(x$definition, " (", x$abbreviation, ")"))
}

#' @export
as.list.glossary <- function(x, ...) {
  class(x) <- "list"
  x <- lapply(x, as.list)
  x
}

#' @export
as.list.glossary_entry <- function(x, ...) {
  class(x) <- "list"
  x
}

require_glossary <- function(x) {
  if(!inherits(x, "glossary")) {
    abort("`x` is not a glossary object")
  }
}
