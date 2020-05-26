emessage <- function(x) message(paste0("[pmtables] ", x))

digit1 <- function(x) formatC(x,digits=1,format = 'f')

def_digits <- function(cols,default) {
  x <- as.list(rep(default, length(cols)))
  setNames(x,cols)
}

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

update_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  common <- intersect(names(left), names(right))
  left[common] <-  right[common]
  left
}

#' @title Significant Digits
#' @description Set significant digits according to metrum SOP.
#' @param x numeric, value to manipulate
#' @param digits numeric, number of significant digits Default: 3
#' @param maxex numeric, maximum number of significant
#' digits before moving to scientific notation, Default: NULL
#' @return character
#' @examples
#' sig(1.123455)
#' sig(0.123455)
#' sig(1.123455,digits = 5)
#' sig(1123,maxex = 3)
#' sig(1123,maxex = 4)
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

data_total_col <- function(data,all_name="all") {
  if(!exists(".total",data)) {
    data[[".total"]] <- all_name
  }
  return(data)
}

n_parens <- function(x) paste0("(n=",x,")")


data <- function(domain = c("id", "obs", "all")) {
  domain <- match.arg(domain)
  file <- paste0(domain,".RDS")
  file <- system.file("data", file, package = "pmtables")
  readRDS(file)
}
