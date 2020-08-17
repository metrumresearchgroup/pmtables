emessage <- function(x) message(paste0("[pmtables] ", x))

cvec_cs <- function(x) {
  if(is.null(x) | length(x)==0) return(character(0))
  if(!is.null(names(x))) return(x)
  x <- unlist(strsplit(as.character(x),",",fixed=TRUE),use.names=FALSE)
  x <- unlist(strsplit(x," ",fixed=TRUE),use.names=FALSE)
  x <- x[x!=""]
  if(length(x)==0) {
    return(character(0))
  } else {
    return(x)
  }
}

#' Format digits
#'
#' Use [sig()] to set the number of significant digits; use [digit1()] to limit
#' to one digit.  See examples.
#'
#' @param x numeric, value to manipulate
#' @param digits numeric, number of significant digits Default: 3
#' @param maxex numeric, maximum number of significant
#' digits before moving to scientific notation, Default: NULL
#'
#' @return character vector of formatted values
#'
#' @examples
#' sig(1.123455)
#' sig(0.123455)
#' sig(1.123455,digits = 5)
#' sig(1123,maxex = 3)
#' sig(1123,maxex = 4)
#'
#' digit1(1.234)
#' digit1(1321.123)
#'
#' @rdname sig
#' @export
sig <- function(x,digits=3,maxex=NULL) {
  if(class(x)=="integer") return(x)
  namez <- names(x)

  x <- as.numeric(x)
  x <- formatC(signif(x,digits=digits), digits=digits, format='g', flag='#')

  if(is.numeric(maxex)) {
    if(digits!=maxex) {
      ex <- "([-]*[0-9]\\.[0-9]+)e([+-][0-9]{2})"
      subit <- grepl(ex,x,perl=TRUE)
      b <- as.numeric(gsub(ex, "\\2", x))
      subit <- subit & abs(b) < maxex
      x <- ifelse(subit,formatC(signif(as.numeric(x),digits=digits),digits=digits, format="fg",flag="#"),x)
    }
  }
  x <- gsub("\\.$", "", x, perl=TRUE)
  names(x) <- namez
  return(x)
}

#' @rdname sig
#' @export
digit1 <- function(x) formatC(x,digits=1,format = 'f')

data_total_col <- function(data,all_name="all") {
  if(!exists(".total",data)) {
    data[[".total"]] <- all_name
  }
  return(data)
}

#' Alias to `dplyr::vars`
#'
#' @param ... passed to [dplyr::vars]
#'
#' @export
.cols <- function(...) { # nocov start
  stop("this function is deprecated; use `dplyr::vars` instead")
} # nocov end

combine_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  left[names(right)] <-  right
  left
}

Update_List <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  common <- intersect(names(left), names(right))
  left[common] <-  right[common]
  left
}

is_regex <- function(x) {
  if(!is.character(x)) return(FALSE)
  x <- suppressWarnings(try(grep(x, "abcde"),silent = TRUE))
  !inherits(x, "try-error")
}

is_str_regex <- function(x) {
  if(!is.character(x)) return(FALSE)
  is_regex(x) || (inherits(x, "fixed") && inherits(x, "pattern"))
}

as_str_regex <- function(x) {
  if(!is.character(x)) return(fixed(basename(tempfile("invalid-regex-"))))
  if(!is_str_regex(x)) {
    return(fixed(x))
  }
  x
}
