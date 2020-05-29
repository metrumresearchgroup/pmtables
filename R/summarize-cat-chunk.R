
summarize_cat_chunk <- function(data, cols) {
  data$Nchunk <- nrow(data)
  ans <- map_dfr(cols, summarize_cat_col, data = data)
  ans <- mutate(ans, name = fct_inorder(.data[["name"]]))
  ans <- mutate(ans, summary = paste0(n," (",.data[["Percent"]],")"))
  mutate(ans,n=NULL,N = NULL,Percent=NULL)
}

summarize_cat_col <- function(name, data) {
  .n <- data[["Nchunk"]][1]
  data <- ungroup(data)
  pick <- select(data, .data[["ID"]], level = all_of(unname(name)))
  pick <- mutate(pick, name = .env[["name"]])
  pick <- group_by(pick, .data[["name"]], .data[["level"]], .drop=FALSE)
  summ <- summarise(pick, n = n())
  summ <- ungroup(summ)
  summ <- mutate(summ, N = .n)
  summ <- mutate(summ, Percent = digit1(100*.data[["n"]]/.n))
  summ <- mutate(summ, level = as.character(.data[["level"]]))
  summ <- mutate(summ, name = as.character(.data[["name"]]))
  return(summ)
}

