#' Save output from stable
#'
#' @param x a table formatted with [stable()]
#' @param file the file
#' @param dir the directory where the file is to be saved
#'
#' @export
stable_save <- function(x, file = attr(x, "stable_file"), dir = getOption("pmtables.dir")) {

  if(!inherits(x, "stable")) {
    stop(
      "bad input - x is not an 'stable' object; ",
      "maybe this object was corrupted or it wasn't generated from 'stable()'",
      call.=FALSE
    )
  }
  if(is.null(file)) {
    miss <- ifelse(missing(file), "and the 'file' argument is missing", "")
    stop(
      "the value of 'file' is NULL; ",
      "there is no stable_file attribute ",
      miss,
      call.=FALSE
    )
  }
  if(!is.null(dir)) {
    file <- file.path(dir,file)
  }
  writeLines(text = x, con = file)
}

insrt_vec <- function(vec, nw, where) {
  vec <- as.list(vec)
  for(i in seq_along(nw)) {
    wh <- where[[i]]
    vec[[wh]] <- c(nw[[i]], vec[[wh]])
  }
  unlist(vec)
}

chunk_runs <- function(x) {
  cumsum(x != c(x[1], x[-length(x)]))+1
}

non_rep <- function(x) {
  ans <- x != c(x[1],x[-length(x)])
  ans[1] <- TRUE
  ans
}

blank_each <- function(x) {
  rep("", length(x))
}

bold_each <- function(x) {
  flg <- nchar(x) > 0
  x[flg] <- paste0("\\textbf{", x[flg], "}")
  x
}

italics_each <- function(x) {
  flg <- nchar(x) > 0
  x[flg] <- paste0("\\textit{", x[flg], "}")
  x
}

it_each <- italics_each

require_col <- function(data,col,context=NULL) {
  if(!exists(col,data)) {
    if(!is.null(context)) {
      message("[context] ", context)
    }
    stop("column '",col,"' is missing from the data frame",call.=FALSE)
  }
}

gluet <- function(x,...) {
  x <- force(x)
  glue(x,.open = "<", .close = ">", .envir = parent.frame())
}

squote <- function(x) paste0("'", x, "'")

#' Make bold
#'
#' @param x a string
#' @param pattern a regular expression
#'
#' @export
tex_bold <- function(x, pattern = "*") {
  assert_that(is.character(x), msg = "'x' must be character")
  w <- grepl(pattern,x) & nchar(x) > 0
  x[w] <- paste0("\\textbf{", x[w], "}")
  x
}

#' @rdname tex_bold
#' @export
tex_it <- function(x, pattern = "*") {
  assert_that(is.character(x), msg = "'x' must be character")
  w <- grepl(pattern,x) & nchar(x) > 0
  x[w] <- paste0("\\textit{", x[w], "}")
  x
}
