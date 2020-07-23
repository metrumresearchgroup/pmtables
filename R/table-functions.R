tab_sp_delim <- mrggt::tab_spanner_delim

foot <- function(short,cols) {
  cols <- cvec_cs(cols)
  values <- unlist(short[cols], use.names=FALSE)
  paste0(cols, ": ",values,collapse="; ")
}

panel_labels <- function(name, value) {
  value
}

#' Wrap stable output in table environment
#'
#' @param x tabular text
#' @param center if `TRUE`, center the table
#' @param caption the table caption
#' @param con where to write the output
#'
#' @export
st_wrap <- function(x, con = NULL, center = TRUE, caption = NULL) {
  ans <- c()
  ans <- c(ans,"\\begin{table}[h]")
  if(isTRUE(center)) {
    ans <- c(ans, "\\centering")
  }
  if(is.character(caption)) {
    ans <- c(ans, paste0("\\caption{",caption,"}"))
  }
  ans <- c(ans, x)
  ans <- c(ans,"\\end{table}")
  if(!missing(con)) {
    writeLines(text=ans, con = con)
  }
  return(invisible(ans))
}

#' @rdname st_wrap
#' @export
pt_wrap <- st_wrap


