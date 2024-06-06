#' Save output from stable
#'
#' @param x a table formatted with [stable()].
#' @param file the output file name.
#' @param dir the directory where the file is to be saved.
#' @param write_caption logical; if `TRUE` and a caption was specified, it
#' will be written in the output file. This argument is not utilized for
#' `stable_long` objects, where captions are _always_ written if they are
#' specified.
#'
#' @return
#' The `stable` or `stable_long` object is returned invisibly.
#'
#' @export
stable_save <- function(x,
                        file = attr(x, "stable_file"),
                        dir = getOption("pmtables.dir"),
                        write_caption = NULL) {
  if(inherits(x, "list")) {
    return(map(x, stable_save, dir = dir))
  }
  # Should we consider using the `dir` argument?
  if(!inherits(x, "stable")) {
    stop(
      "bad input - x is not an 'stable' object; ",
      "maybe this object was corrupted or it wasn't generated from 'stable()'",
      call.=FALSE
    )
  }
  if(!is.character(file)) {
    stop("Please provide a file name for saving the table.")
  }
  path_locked <- isTRUE(attr(x, "stable_file_locked"))
  file_has_dir <- dirname(file) != "."
  if(!missing(dir)) {
    if(file_has_dir | path_locked) {
      warning("ignoring `dir` argument; complete path provided via `file`.")
    }
  }
  con <- file
  if(!file_has_dir && !path_locked && is.character(dir)) {
    con <- file.path(dir, con)
  }
  # Priority goes to function argument so we can use this to
  # reshape what was specified when the caption was generated
  if(is.null(write_caption)) {
    write_caption <- cap_write(x)
  }
  if(isTRUE(write_caption) && !inherits(x, "stable_long")) {
    cap <- form_caption(cap_main(x), cap_short(x))
    x <- c(cap, x)
  }
  writeLines(text = x, con = con)
  return(invisible(x))
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

split_bold <- function(x) {
  if(!str_detect(x,fixed("..."))) {
    return(bold_each(x))
  }
  x <- str_split(x, fixed("..."))
  x <- map(x, bold_each)
  x <- flatten_chr(x)
  paste0(x, collapse = "...")
}

require_col <- function(data,col,context=NULL) {
  if(!exists(col,data)) {
    if(!is.null(context)) {
      message("[context] ", context)
    }
    stop("column '",col,"' is missing from the data frame",call.=FALSE)
  }
}

gluet <- function(x, .envir = parent.frame(), ...) {
  x <- force(x)
  glue(x,.open = "<", .close = ">", .envir = .envir)
}

mgluet <- function(x, ...) {
  if(length(x)==1) return(gluet(x, ...))
  w <- grepl("<", x, fixed = TRUE)
  x[w] <- map(x[w], gluet, ...)
  flatten_chr(x)
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

grep_col <- function(x,pattern) {
  which(str_detect(x,pattern))
}

#' Find matching rows in a data frame
#'
#' @param data a data frame to search
#' @param pattern a regular expression
#' @param cols a character vector of column names to search
#'
#'
#' @export
df_grep_rows <- function(data, pattern, cols = names(data)) {
  rows <- df_grepl_rows(data, pattern, cols)
  return(which(rows))
}

#' @rdname df_grep_rows
#' @export
df_grepl_rows <- function(data, pattern, cols = names(data)) {
  assert_that(is.character(cols))
  cols <- cols[cols %in% names(data)]
  if(length(cols)==0) return(NULL)
  rows <- map(data[,cols], grep_col, pattern = pattern)
  rows <- flatten_int(rows)
  seq(nrow(data)) %in% rows
}

paste_units <- function(cols, units) {
  if(is.null(units) || length(cols)==0) return(cols)
  unit_match <- match(cols,names(units))
  col_match <- which(!is.na(unit_match))
  unit_match <- na.omit(unit_match)
  for(i in seq_along(col_match)) {
    cols[[col_match[i]]] <- paste(cols[[col_match[i]]],units[[unit_match[i]]])
  }
  cols
}

indent_tex <- function(x, n) {
  if(n == 0) return(x)
  prefix <- paste0("\\hskip ", n, "ex ")
  paste0(prefix, x)
}
