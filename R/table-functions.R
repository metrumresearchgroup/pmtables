
#' Wrap stable output in table environment
#'
#' @param x tabular text
#' @param center if `TRUE`, center the table
#' @param caption the table caption
#' @param con where to write the output
#'
#' @export
st_wrap <- function(x, con = NULL, center = TRUE, caption = NULL) { # nocov start
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
} # nocov end

#' @rdname st_wrap
#' @export
pt_wrap <- st_wrap


