#' Save output from stable
#'
#' @param x a table formatted with [stable()]
#' @param dir the directory where the file is to be saved
#'
#' @export
stable_save <- function(x, dir = NULL) {
  file <- attr(x, "stable_file")
  if(is.null(file)) {
    stop(
      "bad input - there is no stable_file attribute; ",
      "maybe this object was corrupted or it wasn't generated from 'stable()'",
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

blank_each <- function(x) {
  rep("", length(x))
}

bold_each <- function(x) {
  flg <- nchar(x) > 0
  x[flg] <- paste0("{\\bf ", x[flg], "}")
  x
}

italics_each <- function(x) {
  flg <- nchar(x) > 0
  x[flg] <- paste0("{\\it ", x[flg], "}")
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

# cvec_cs <- function(x) {
#   if(is.null(x) | length(x)==0) return(character(0))
#   if(!is.null(names(x))) return(x)
#   x <- unlist(strsplit(as.character(x),",",fixed=TRUE),use.names=FALSE)
#   x <- unlist(strsplit(x," ",fixed=TRUE),use.names=FALSE)
#   x <- x[x!=""]
#   if(length(x)==0) {
#     return(character(0))
#   } else {
#     return(x)
#   }
# }

gluet <- function(x,...) {
  x <- force(x)
  glue(x,.open = "<", .close = ">", .envir = parent.frame())
}

squote <- function(x) paste0("'", x, "'")
