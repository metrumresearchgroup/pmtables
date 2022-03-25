
summarize_cat_chunk <- function(data, cols, N, denom = "group") {
  Nchunk <- nrow(data)
  if(denom=="group") {
    D <- Nchunk
  } else {
    D <- N
  }
  ans <- map_dfr(cols, summarize_cat_col, data = data, D = D, Nchunk = Nchunk)
  ans <- mutate(ans, name = fct_inorder(.data[["name"]]))
  ans <- mutate(ans, summary = paste0(n," (",.data[["Percent"]],")"))
  mutate(ans,n=NULL,Percent=NULL)
}

summarize_cat_col <- function(name, data, D, Nchunk) {
  .n <- D
  data <- ungroup(data)
  pick <- select(data, .data[["ID"]], level = all_of(unname(name)))
  pick <- mutate(pick, name = .env[["name"]])
  pick <- group_by(pick, .data[["name"]], .data[["level"]], .drop=FALSE)
  summ <- summarise(pick, n = n())
  summ <- ungroup(summ)
  summ <- mutate(summ, N = Nchunk)
  summ <- mutate(summ, Percent = digit1(100*.data[["n"]]/D))
  summ <- mutate(summ, level = as.character(.data[["level"]]))
  summ <- mutate(summ, name = as.character(.data[["name"]]))
  return(summ)
}

