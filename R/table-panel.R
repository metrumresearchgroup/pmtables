panel_by <- function(data, col, prefix = NULL) {
  nc <- ncol(data)-1
  u <- unique(data[[col]])
  where <- match(u,data[[col]])
  lab <- data[[col]][where]
  lab <- paste(prefix,lab)
  lab <- bold_each(lab)
  insrt <- paste0("\\multicolumn{", nc,"}{l}{", lab,"}\\\\")
  insrt[2:length(insrt)] <- paste0("\\hline ", insrt[2:length(insrt)])
  list(where = where, to_insert = insrt)
}

