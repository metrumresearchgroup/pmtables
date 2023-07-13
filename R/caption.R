parse_caption <- function(text) {
  text <- trimws(text)
  short <- str_extract(text, "^ *\\[.*\\]")
  if(is.na(short)) {
    ans <- list(text = text, short = NULL)
  } else {
    text <- sub(short, "", text, fixed = TRUE)
    short <- trimws(short)
    short <- substr(short, 2, nchar(short)-1)
    ans <- list(short = short, text = text)
  }
  return(ans)
}
