
summarize_cat_chunk <- function(data,cols) {
  data$Nchunk <- nrow(data)
  ans <- map_dfr(cols,summarize_cat_col,data = data) 
  ans <- mutate(ans,name = fct_inorder(name))
  ans <- mutate(ans,summary = paste0(n," (",Percent,")"))
  mutate(ans,n=NULL,N = NULL,Percent=NULL)
}

summarize_cat_col <- function(name,data) {
  .n <- data[["Nchunk"]][1]
  data <- ungroup(data)
  pick <- select(data,ID,level:=all_of(name)) %>% mutate(name := name)
  pick <- group_by(pick,name,level,.drop=FALSE)
  summ <- summarise(pick,n = n())
  summ <- ungroup(summ)
  summ <- mutate(summ,N = .n, Percent = digit1(100*n/.n))
  summ <- mutate(summ,level=as.character(level),name=as.character(name))
  return(summ)
}

