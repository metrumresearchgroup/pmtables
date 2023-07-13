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
    ans <- list(text = text, short = short)
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
  ans <- list(short = short, text = text)
  return(ans)
}
