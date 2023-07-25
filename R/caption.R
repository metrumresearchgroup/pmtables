#' More sophisticated parsing of caption
#'
#' @noRd
extract_short <- function(x) {
  x <- trimws(x)
  open <- gregexpr("[", x, fixed = TRUE)[[1]]
  close <- gregexpr("]", x, fixed = TRUE)[[1]]
  mismatch <- length(open) != length(close)
  positions <- c(open, close)
  if(length(open)==0 || length(close)==0 || open[1] != 1) {
    return(list(main = x, short = ""))
  }
  bits <- c(rep(1, length(open)), rep(-1, length(close)))
  o <- order(positions)
  bits <- bits[o]
  positions <- positions[o]
  done <- cumsum(bits)
  done <- positions[done==0]
  if(length(done)==0) {
    if(mismatch) {
      done <- positions[bits==-1][1]
    } else {
      return(list(main = x, short = ""))
    }
  }
  short <- substr(x, 2, done[1] - 1)
  main <- substr(x, done[1] + 1, nchar(x))
  list(main = main, short = short)
}

#' Goal is to preserve space in between short and main caption
#' when short is repeated. If short not repeated, then ok to just trim.
#' trimws() is called quite a bit, but important to consider context of the
#' call.
#'
#' @param text a chunk of caption text that may or may not need to be parsed.
#' @param short optional short caption text.
#' @noRd
parse_caption <- function(text, short = NULL, short_repeat = TRUE, short_sep = "") {
  ext <- extract_short(text)
  has_parsed_short <- nchar(ext$short) > 0
  if(is.null(short) && has_parsed_short) {
    short <- ext$short
  }
  if(is.null(short)) {
    ans <- list(main = text, short = short)
    return(ans)
  }
  text <- ext$main
  short <- trimws(short, which = "left")
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
#' @details
#' Caption text (in `x`) can contain a short caption title enclosed in brackets
#' (`[]`) as the opening characters (see examples). The short title is
#' assumed to start after the opening `[` and end prior to `]` when all brackets
#' are matched, allowing `[...]` in the short title as long as all brackets
#' are matched. For other scenarios where a single `]` may need to appear
#' in the short title, the user should use the `short` argument instead of
#' the bracket notation.
#'
#' @examples
#'
#' # No separate short title
#' as.caption("Main title text")
#'
#' # Note that the short title is repeated by default
#' as.caption("[Short title]. Main title text")
#'
#' unclass(as.caption("[Short title]. Main title text."))
#' unclass(as.caption(". Main title text.", short = "Short"))
#'
#' @return
#' An object inheriting from `st_caption` and `character`.
#'
#' @export
as.caption <- function(x, short = NULL, short_repeat = TRUE,
                       short_sep = NULL, write = FALSE) {

  if(inherits(x, "st_caption")) {
    return(x)
  }
  if(!is.character(x) || length(x)!=1) {
    stop("`x` must be character with length 1.")
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
