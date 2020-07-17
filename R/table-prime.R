
#' Prime a data frame for inclusion into a latex table
#'
#' @param data a data frame
#' @param escape_fun a function to sanitize data frame contents
#'
#' @export
tab_prime <- function(data, escape_fun = tab_escape) {
  data <- modify(data, as.character)
  data <- modify(data, replace_na, "")
  math <- map_lgl(data, ~ any(str_count(.x, fixed("$")) >= 2))
  if(any(math)) {
    data[,!math] <- modify(data[,!math], escape_fun)
  }
  data
}

#' @rdname tab_prime
#' @param string data to sanitize
#' @param chrs a character vector of strings to escape
tab_escape <- function(string, chrs = c("_", "&", "#", "%")) {
  for(ch in chrs) {
    string <- gsub(ch, paste0("\\",ch), string, fixed = TRUE)
  }
  string <- gsub("~", "$\\sim$", string, fixed = TRUE)
  string <- gsub(">", "$>$", string, fixed = TRUE)
  string <- gsub("<", "$<$", string, fixed = TRUE)
  string
}



