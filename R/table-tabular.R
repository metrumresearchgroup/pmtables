
make_tabular <- function(data, indent = NULL) {
  tab <- modify(data, function(x) {
    formatC(x, width = max(nchar(x)))
  })
  tab <- map_chr(seq(nrow(data)), function(i) {
    paste0(data[i,],collapse = " & ")
  })
  tab <- paste0(tab, " \\\\")
  if(is.character(indent)) {
    tab <- paste0("\\hskip 0.3cm", tab)
  }
  tab
}

form_cols <- function(cols, bold = FALSE, relabel = NULL, blank = NULL,
                      units = NULL) {
  if(!is.null(blank)) {
    blank <- unname(new_names(blank))
    bl <- cols %in% blank
    cols[bl] <- rep("", sum(bl))
  }

  if(!is.null(relabel)) {
    relabel <- new_names(relabel)
    relabel <- relabel[relabel %in% cols]
    newi <- match(cols, relabel)
    loc <- which(!is.na(newi))
    cols[loc] <- names(relabel)
    cols <- unname(cols)
  }
  if(isTRUE(bold)) cols <- bold_each(cols)
  cols <- paste0(cols, collapse = " & ")
  cols <- paste0(cols, " \\\\")
  cols <- paste0("", cols)
  if(is.character(units) && any(nchar(units) > 0)) {
    cols <- paste0(cols, "[-0.5em]")
    cols <- c(cols, units)
  }
  cols
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
    return(ans)
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
