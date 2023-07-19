#' Goal is to preserve space in between short and main caption
#' when short is repeated. If short not repeated, then ok to just trim.
#' trimws() is called quite a bit, but important to consider context of the
#' call.
#'
#' @param text a chunk of caption text that may or may not need to be parsed.
#' @param short optional short caption text
#' @noRd
parse_caption <- function(text, short = NULL, short_repeat = TRUE, short_sep = "") {
  text <- trimws(text)
  parsed_short <- str_extract(text, "^\\[.+?\\]")
  has_parsed_short <- !is.na(parsed_short)
  if(is.null(short) && !is.na(parsed_short)) {
    short <- parsed_short
  }
  if(is.null(short)) {
    ans <- list(main = text, short = short)
    return(ans)
  }
  text <- sub(short, "", text, fixed = TRUE)
  short <- trimws(short, which = "left")
  if(has_parsed_short) {
    short <- substr(short, 2, nchar(short)-1)
  }
  if(is.character(short) && isTRUE(short_repeat)) {
    text <- paste0(short, short_sep, text)
  }
  if(!isTRUE(short_repeat)) {
    text <- trimws(text, which = "left")
  }
  short <- trimws(short)
  ans <- list(main = text, short = short)
  return(ans)
}

cap_main <- function(x) {
  attributes(x)$caption
}

cap_short <- function(x) {
  attributes(attributes(x)$caption)$short
}

cap_write <- function(x) {
  isTRUE(attributes(attributes(x)$caption)$write)
}

#' Create caption
#'
#' @param x caption text.
#' @param short an abbreviated form of the caption to be used in a list of
#' tables.
#' @param short_repeat logical; if a short caption is provided, it will be
#' repeated at the start of the main caption.
#' @param short_sep a character sequence used to separate the short title
#' with the main caption when a short title is specified.
#' @param write logical; if `TRUE`, caption will be written to output file
#' when [stable_save()] is called; this argument does not apply to
#' `stable_long` objects whose captions are _always_ written to output the
#' output file.
#'
#' @examples
#'
#' as.caption("[Short]. Main title text.")
#' as.caption("Main title text.", short = "Short")
#'
#' @return
#' A character vector of (main) caption text is returned with
#' attributes `short` and `write`.
#'
#' @export
as.caption <- function(x, short = NULL, short_repeat = TRUE,
                       short_sep = NULL, write = FALSE) {

  if(inherits(x, "st_caption")) {
    return(x)
  }
  cap <- parse_caption(
    text = x,
    short = short,
    short_repeat = short_repeat,
    short_sep = short_sep
  )
  structure(
    cap$main,
    short = cap$short,
    write = isTRUE(write),
    class = c("st_caption", "character")
  )
}

# Is candidate caption text an `st_object`?
is.st_caption <- function(x) inherits(x, "st_caption")

# Does a table have caption info
has.st_caption <- function(x) {
  is.st_caption(attributes(x)$caption)
}

#' @export
print.st_caption <- function(x, ...) {
  write <- attributes(x)$write
  main <- as.character(x)
  short <- attributes(x)$short
  w <- NULL
  s <- NULL
  if(isTRUE(write)) w <- " %write%"
  if(is.character(short)) s <- paste0("[", short, "] ")
  print(paste0(s, main, w))
}
