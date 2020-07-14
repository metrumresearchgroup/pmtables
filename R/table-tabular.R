
make_tabular <- function(data,indent = NULL) {
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

form_cols <- function(cols, bold = FALSE, relabel = NULL,
                      units = NULL) {
  if(!is.null(relabel)) {
    names(cols) <- cols
    col_rename <- eval_rename(relabel,data = cols)
    cols[col_rename] <- names(col_rename)
    unname(cols)
  }
  if(isTRUE(bold)) cols <- bold_each(cols)
  cols <- paste0(cols, collapse = " & ")
  cols <- paste0(cols, " \\\\")
  cols <- paste0("", cols)
  if(is.character(units)) {
    cols <- paste0(cols, "[-0.5em]")
    cols <- c(cols, units)
  }
  cols
}

form_unit <- function(units, cols) {
  if(is.null(units)) return(NULL)
  unit <- vector(mode = "character", length = length(cols))
  i <- match(names(units),cols)
  unit[i] <- units
  unit <- paste0(unit, collapse = " & ")
  unit <- paste0(unit, " \\\\ ")
  unit
}

form_open <- function(align) {
  paste0("\\begin{tabular}[h]{", align, "}")
}


