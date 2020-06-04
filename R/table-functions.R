tab_sp_delim <- mrggt::tab_spanner_delim

foot <- function(short,cols) {
  cols <- cvec_cs(cols)
  values <- unlist(short[cols], use.names=FALSE)
  paste0(cols, ": ",values,collapse="; ")
}

panel_labels <- function(name, value) {
  value
}
