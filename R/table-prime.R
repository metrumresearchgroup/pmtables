
#' Prime a data frame for inclusion into a latex table
#'
#' @param data a data frame
#' @param escape_fun a function to sanitize data frame contents
#'
#' @export
tab_prime <- function(data, escape_fun = tab_escape, chrs = "_") {
  if(isTRUE(attr(data, "pmtables.primed"))) {
    return(data)
  }
  data <- modify(data, as.character)
  data <- modify(data, replace_na, "")
  math <- map_lgl(data, ~ any(str_count(.x, fixed("$")) >= 2))
  if(any(math)) {
    data[,!math] <- modify(data[,!math], escape_fun)
  } else {
    data <- modify(data, escape_fun)
  }
  structure(data, pmtables.primed = TRUE)
}

#' @rdname tab_prime
#' @param string data to sanitize
#' @param chrs a character vector of strings to escape
#' @param ... used only to allow arguments through
tab_escape <- function(string,
                       chrs = getOption("pmtables.san","_"), ...) {
  for(ch in chrs) {
    string <- gsub(ch, paste0("\\",ch), string, fixed = TRUE)
  }
  string <- gsub("~", "$\\sim$", string, fixed = TRUE)
  string <- gsub(">", "$>$", string, fixed = TRUE)
  string <- gsub("<", "$<$", string, fixed = TRUE)
  string
}
