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
  esc <- getOption("pmtables.escape", c("_", "%"))
  if(is.character(esc)) {
    data <- modify(data, escape_fun, esc = esc)
  }
  structure(data, pmtables.primed = TRUE)
}

any_skip_escape <- function(x) {
  any(str_count(x, fixed("$")) > 1) |
    any(str_count(x, fixed("\\")) > 0)
}

do_escape <- function(x) {
  str_count(x, fixed("$")) <= 1 &
    str_count(x, fixed("\\")) == 0
}

#' @rdname tab_prime
#' @param string data to sanitize
#' @param esc a character vector of strings to escape
#' @param ... used only to allow arguments through
tab_escape <- function(string, esc = getOption("pmtables.escape", c("_", "%")), ...) {
  if(is.null(esc)) return(string)
  w <- do_escape(string)
  for(ch in esc) {
    string[w] <- gsub(ch, paste0("\\",ch), string[w], fixed = TRUE)
  }
  string[w] <- gsub("~", "$\\sim$", string[w], fixed = TRUE)
  string[w] <- gsub(">", "$>$", string[w], fixed = TRUE)
  string[w] <- gsub("<", "$<$", string[w], fixed = TRUE)
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

form_unit <- function(units, cols) { # nocov start
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
} # nocov end

form_open <- function(align) {
  paste0("\\begin{tabular}[h]{", align, "}")
}

# all the rows above top headline for rows
form_headrows <- function(span_data, cols_tex, cols_data) {
  hl1 <-  hl2 <- "\\hline"
  if(cols_data$omit) {
    cols_tex <- NULL
    if(is.null(span_data$span)) {
      hl1 <- NULL
    } else {
      hl2 <- NULL
    }
  }
  c(hl1, span_data$tex, cols_tex, hl2)
}
