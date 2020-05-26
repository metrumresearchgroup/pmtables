tab_sp_delim <- mrggt::tab_spanner_delim

foot <- function(short,cols) {
  cols <- cvec_cs(cols)
  keys <- cols
  values <- unlist(short[cols], use.names=FALSE)
  paste0(keys, ": ",values,collapse="; ")
}
