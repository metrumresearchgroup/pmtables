
#' Wrap stable output in table environment
#'
#' @param x tabular text
#' @param con where to write the output
#' @param table if `TRUE`, the code is wrapped in latex table environment
#' @param center if `TRUE`, center the table
#' @param caption the table caption
#' @param context if `rmd`, then the code is enclosed in a pandoc `latex` fenced
#' code block; if `tex`, then the fencing is omitted

#'
#' @export
st_wrap <- function(x, con = stdout(), table = TRUE, center = TRUE, caption = NULL,
                    context = c("rmd", "tex")) { # nocov start
  context <- match.arg(context)
  ans <- c()
  if(isTRUE(table)) {
    ans <- c(ans, "\\begin{table}[h]")
    if(isTRUE(center)) ans <- c(ans, "\\centering")
    if(is.character(caption)) {
      ans <- c(ans, paste0("\\caption{",caption,"}"))
    }
    ans <- c(ans, x)
    ans <- c(ans,"\\end{table}")
  } else {
    ans <- x
  }
  if(isTRUE(center) && !isTRUE(table)) {
    ans <- c("\\begin{center}", ans, "\\end{center}")
  }
  if(context=="rmd") {
    ans <- c("```{=latex}", ans, "```")
  }
  if(!is.null(con)) {
    writeLines(text = ans, con = con)
  }
  return(invisible(ans))
} # nocov end

#' @rdname st_wrap
#' @export
pt_wrap <- st_wrap

#' @rdname st_wrap
#' @export
st_latex <- function(x, con = stdout(), center = TRUE, context = c("rmd", "tex")) {
  context <- match.arg(context)
  ans <- x
  if(isTRUE(center)) {
    ans <- c("\\begin{center}", ans, "\\end{center}")
  }
  if(context=="rmd") {
    ans <- c("```{=latex}", ans, "```")
  }
  if(!is.null(con)) {
    writeLines(text = ans, con = con)
  }
  return(invisible(ans))

}
