#' Read and parse a glossary file
#'
#' Files may be formatted as TeX glossary file or
#' in yaml format suitable for reading using
#' [yaml_as_df()] (see details).
#'
#' @param file path to the tex or yaml glossary file.
#' @param format the glossary file format; could be either `"tex"` or `"yaml"`
#' and will be inferred by default from the extension of `file`.
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
#' file <- system.file("glo", "glossary.tex", package = "pmtables")
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
read_glossary <- function(file, format = guess_glo_fmt(file)) {
  if(!file.exists(file)) {
    abort(glue("Glossary file {file} does not exist."))
  }
  if(!format %in% c("tex", "yaml", "yml")) {
    abort("format should be either tex, yaml, or yml")
  }
  if(format == "tex") {
    ans <- read_tex_glossary(file)
  } else {
    ans <- read_yaml_glossary(file)
  }
  class(ans) <- c("tex_glossary", "glossary", "list")
  ans
}

read_yaml_glossary <- function(file) {
  parsed <- try(yaml_as_df(file, row_var = "label"), silent = TRUE)
  if(inherits(parsed, "try-error")) {
    abort(
      c("Failed to parse glossary file; see ?yaml_as_df() for help formatting this file.",
        as.character(parsed)
      )
    )
  }
  if(!all(c("label", "abb", "def") %in% names(parsed))) {
    abort("yaml/yml glossary format requires names label, abb, and def.")
  }
  labels <- parsed[["label"]]
  parsed <- select(parsed, "abb", "def")
  data <- split(parsed, seq(nrow(parsed)))
  data <- lapply(data, unlist)
  data <- lapply(data, as_glossary_entry)
  data <- lapply(data, setNames, c("abbreviation", "definition"))
  names(data) <- labels
  data
}

read_tex_glossary <- function(file) {
  txt <- readLines(file, warn = FALSE)
  if(!length(txt)) abort("The glossary file was empty.")
  parse_tex_glossary(txt)
}

parse_tex_glossary <- function(txt) {
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

#' Update the abbreviation for a glossary entry
#'
#' @param x a glossary object.
#' @param ... `<label> = <new abbreviation>` pairs; if just the
#' `<label>` is passed, it will be used as the new update
#' abbreviation value.
#'
#' @examples
#' x <- as_glossary(list(ka = "absorption rate constant"))
#'
#' x <- update_abbrev(x, ka = "\\ensuremath{k_a}")
#'
#' x$ka
#'
#' x <- update_abbrev(x, ka)
#'
#' x$ka
#'
#' @seealso [as_glossary()]
#' @export
update_abbrev <- function(x, ...) {
  require_glossary(x)
  items <- new_names(enquos(...))
  abort_bad_glo_labels(x, names(items))
  for(m in seq_along(items)) {
    x[[names(items[m])]]$abbreviation <- items[[m]]
  }
  x
}

#' Coerce a named list to glossary
#'
#' @param x a named list.
#' @param ... unquoted `<label> = <new abbreviation>` pairs.
#' @seealso [read_glossary()]
#'
#' @examples
#'
#' l <- list(VPC = "visual predictive check", tz = "timezone")
#'
#' as_glossary(l)
#'
#' as_glossary(l, tz = "tzone")
#'
#'
#' @seealso [update_abbrev()]
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
  if(length(abbrev)) {
    abort_bad_glo_labels(glossary, names(abbrev))
    for(a in seq_along(abbrev)) {
      glossary[[names(abbrev[a])]]$abbreviation <- abbrev[a]
    }
  }
  class(glossary) <- c("tex_glossary", "glossary", "list")
  glossary
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
#' file <- system.file("glo", "glossary.tex", package = "pmtables")
#'
#' glossary_notes(file, WT, CRCL)
#'
#' g <- as_glossary(list(ss = "steady state", ALB = "albumin", WT = "weight"))
#'
#' glossary_notes(g, ALB, ss)
#'
#' @seealso [st_notes_glo()], [read_glossary()]
#' @export
glossary_notes <- function(x, ...) UseMethod("glossary_notes")

#' @param ... unquoted glossary labels to be included in the notes text.
#' @param format passed to [read_glossary()].
#' @rdname glossary_notes
#' @export
glossary_notes.character <- function(x, ..., format = guess_glo_fmt(x)) {
  x <- read_glossary(x, format = format)
  glossary_notes(x, ...)
}

#' @rdname glossary_notes
#' @export
glossary_notes.list <- function(x, format = guess_glo_fmt(x), ...) {
  x <- as_glossary(x, format = format)
  glossary_notes(x, ...)
}

#' @rdname glossary_notes
#' @export
glossary_notes.glossary <- function(x, ..., sep = ": ", collapse = "; ",
                                    labels = NULL) {
  labels <- cvec_cs(labels)
  dots <- eval_select(expr(c(...)), x)
  labels <- c(names(dots), labels)
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
  cat(ans <- paste0(x$definition, " (", x$abbreviation, ")"))
}

#' @export
print.glossary <- function(x, ...) {
  label <- names(x)
  def <- map_chr(x, "definition")
  def <- substr(def, 0, 40)
  pad <- ifelse(nchar(def) >= 40, "...", "")
  def <- paste0(def, pad)
  label <- formatC(label, width = max(nchar(label)), flag = "-")
  cat(paste0(label, " : ", def), sep = "\n")
}

#' @export
as.data.frame.glossary <- function(x, ...) {
  data.frame(
    label = names(x),
    definition = map_chr(x, "definition"),
    abbreviation = map_chr(x, "abbreviation"),
    row.names = NULL
  )
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

#' @export
c.glossary <- function(x, y, ...) {
  cl <- class(x)
  new_cols <- setdiff(names(y), names(x))
  if(!identical(new_cols, names(y))) {
    stop("`x` and `y` cannot share any names.")
  }
  ans <- structure(c(unclass(x), unclass(y)), class = cl)
  more <- list(...)
  if(length(more)) {
    return(c(ans, more[[1]]))
  }
  ans
}

#' @export
`$.glossary` <- function(x, name, ...) {
  if(!exists(name, x)) {
    return(NULL)
  }
  unclass(x)[[name]]
}

#' @export
`[.glossary` <- function(x, i, j, drop = FALSE) {
  cl <- class(x)
  x <- unclass(x)
  x <- x[i]
  structure(x, class = cl)
}

#' Select entries from a glossary object
#'
#' @param x a glossary object.
#' @param ... tidy-select syntax naming glossary entry labels to select.
#'
#' @return
#' A new glossary object with selected entries.
#'
#' @examples
#' gl <- as_glossary(list(a = "apple", b = "banana", c = "cat", d = "dog"))
#'
#' select_glossary(gl, b, d)
#'
#' @export
select_glossary <- function(x, ...) {
  what <- eval_select(expr(c(...)), x)
  x[what]
}

require_glossary <- function(x) {
  if(!inherits(x, "glossary")) {
    abort("`x` is not a glossary object")
  }
}

guess_glo_fmt <- function(filename) {
  ext <- tools::file_ext(filename)
  if(ext=="tex") return("tex")
  if(ext %in% c("yml", "yaml")) return("yaml")
  abort("Could not guess glossary format from file extension.")
}