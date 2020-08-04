#' Prime a data frame for inclusion into a latex table
#'
#' @param data a data frame
#' @param escape_fun a function to sanitize data frame contents
#'
#' @export
tab_prime <- function(data, escape_fun = tab_escape) {
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
#' @param esc a character vector of strings to escape
#' @param ... used only to allow arguments through
tab_escape <- function(string, esc = "_", ...) {
  for(ch in esc) {
    string <- gsub(ch, paste0("\\",ch), string, fixed = TRUE)
  }
  string <- gsub("~", "$\\sim$", string, fixed = TRUE)
  string <- gsub(">", "$>$", string, fixed = TRUE)
  string <- gsub("<", "$<$", string, fixed = TRUE)
  string
}

esc_underscore <- function(string) {
  gsub("_", "\\_", string, fixed = TRUE)
}

#' Create tabular environment from data frame
#'
#' @param data a data.frame
#' @param prime_fun a function to prime the data frame for rendering in TeX
#' @param escape_fun a function to escape characters that have special meaning
#' in TeX
#' @param ... not used
#' @export
make_tabular <- function(data, prime_fun = tab_prime,
                         escape_fun = tab_escape, ...) {

  if(is.character(prime_fun)) {
    prime_fun <- get(prime_fun, mode = "function")
  }
  if(is.character(escape_fun)) {
    escape_fun <- get(escape_fun, mode = "function")
  }

  assert_that(is.function(prime_fun))
  assert_that(is.function(escape_fun))

  data <- prime_fun(data, escape_fun)

  tab <- modify(data, function(x) {
    formatC(x, width = max(nchar(x)))
  })

  tab <- map_chr(seq(nrow(data)), function(i) {
    paste0(data[i,],collapse = " & ")
  })

  tab <- paste0(tab, " \\\\")

  tab
}

form_unit <- function(units, cols) {
  if(is.null(units)) return(NULL)
  ans <- vector(mode = "character", length = length(cols))
  units <- units[names(units) %in% cols]
  if(length(units)==0) {
    warning(
      "the 'units' argument was passed into 'stable()', ",
      "but no column matches were found.",
      call.=FALSE
    )
    return(NULL)
  }
  i <- match(names(units),cols)
  i <- i[!is.na(i)]
  ans[i] <- units
  ans <- paste0(ans, collapse = " & ")
  ans <- paste0(ans, " \\\\ ")
  ans
}

form_open <- function(align) {
  paste0("\\begin{tabular}[h]{", align, "}")
}
