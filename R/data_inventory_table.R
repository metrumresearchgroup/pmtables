
data_inventory_chunk <- function(data,outer,inner=outer, stacked = FALSE, tot=FALSE,
                                 dv_col = "DV", bq_col = c("BQL", "BLQ"),
                                 id_col = "ID",...) {

  if(outer==".total" | inner==".total") {
    data <- data_total_col(data)
  }

  bq_col <- match.arg(bq_col)
  by <- c(outer,inner)
  data <- ungroup(data)
  if(stacked) {
    data <- group_by(data,!!sym(outer))
    data <- mutate(data,.N = n_non_missing(!!sym(dv_col)))
    data <- ungroup(data)
  } else {
    data <- mutate(data,.N = n_non_missing(!!sym(dv_col)))
  }
  data <- group_by(data,!!sym(outer))
  data <- mutate(data,..n = n_non_missing(!!sym(dv_col)))
  data <- ungroup(data)
  data <- group_by(data,!!!syms(by))
  body <- summarise(
    data,
    SUBJ = n_unique(!!sym(id_col)),
    NOBS = n_non_missing(!!sym(dv_col)),
    NMISS = n_missing(!!sym(dv_col),!!sym(bq_col)),
    POBS = digit1(100*NOBS/first(..n)),
    OOBS = digit1(100*NOBS/first(.N))
  )
  bq <- summarise(
    data,
    NBQL = sum(!!sym(bq_col) !=0),
    PBQL = digit1(100*NBQL/first(..n)),
    OBQL = digit1(100*NBQL/first(.N))
  )
  summ <- left_join(body,bq,by = c(outer,inner))
  summ <- select(
    summ,
    !!sym(outer),
    !!sym(inner),
    SUBJ,
    NOBS,
    NMISS,
    NBQL,
    POBS,
    PBQL,
    OOBS,
    OBQL
  )
  summ <- ungroup(summ)
  summ <- mutate(summ, !!sym(outer) := as.character(!!sym(outer)))
  summ <- mutate(summ, !!sym(inner) := as.character(!!sym(inner)))
  summ
}

data_inventory_data_split <- function(data,outer,inner=outer,stacked=FALSE,...) {
  data <- ungroup(data)
  data <- split(data,data[[outer]],drop=TRUE)
  data <- map_dfr(data,data_inventory_data,outer=outer,inner=inner,stacked=FALSE,...)
  data <- mutate(data, !!sym(inner) := replace_na(!!sym(inner),"all"))
  data <- fill(data,!!sym(outer),.direction = "down")
  data[[".total"]] <- NULL
  data

}

data_inventory_data <- function(data,outer,inner=outer,stacked=FALSE,...) {
  outer <- unname(outer)
  inner <- unname(inner)
  if(stacked) {
    ans <- data_inventory_data_split(data,outer,inner,stacked = FALSE, ...)
    return(ans)
  }
  data <- data_total_col(data)
  ans <- data_inventory_chunk(data,outer,inner,stacked,...)
  if(outer != ".total") {
    tot <- data_inventory_chunk(data,outer=".total",inner=".total",stacked=FALSE,...)
    ans <- bind_rows(ans,tot)
  }
  ans[[".total"]] <- NULL
  if(inner!=outer) {
    ans <- mutate(ans, !!sym(inner) := replace_na(!!sym(inner),".total"))
    ans <- fill(ans, !!sym(outer), .direction = "down")
  }
  if(inner==outer) {
    ans <- mutate(ans, !!sym(outer) := replace_na(!!sym(outer),".total"))
  }
  ans
}

data_inventory <- function(data,outer = ".total",inner=outer, ...,
                           inner_summary = TRUE, drop_miss = FALSE,
                           stacked = FALSE,table=NULL,subset = TRUE) {

  subset <- enquo(subset)
  data <- filter(data, !!subset)

  outer <- new_names(outer,table)
  inner <- new_names(inner,table)

  if(inner==outer) {
    inner_summary <- FALSE
    stacked <- FALSE
  }

  total_name <- ifelse(stacked, "Group Total", "Grand Total")

  ans <- data_inventory_data(
    data,
    outer = outer,
    inner = inner,
    stacked = stacked,
    ...
  )

  if(exists(inner,ans)) {
    ans <- mutate(
      ans,
      !!sym(inner) := ifelse(!!sym(inner)==".total", total_name, !!sym(inner))
    )
  }


  if(inner_summary) {
    ans <- rename(
      ans,
      `Group percent.OBS` = POBS,
      `Group percent.BQL` = PBQL,
      `Overall percent.OBS` = OOBS,
      `Overall percent.BQL` = OBQL
    )
  } else {
    ans <- rename(
      ans,
      `Percent.OBS` = OOBS,
      `Percent.BQL` = OBQL
    )
    ans <- mutate(ans, POBS=NULL,PBQL=NULL)
  }

  ans <- rename(
    ans,
    SUBJ = SUBJ,
    MISS = NMISS,
    OBS = NOBS,
    BQL = NBQL
  )

  if(drop_miss) {
    if(all(ans$MISS==0)) {
      ans <- mutate(ans, MISS = NULL)
    }
  }

  if(inner!=outer) {
    ans <- mutate(
      ans,
      !!sym(outer) := paste0(names(outer), ": ", !!sym(outer))
    )
    ans <- rename(ans,!!!syms(outer))
    ans <- rename(ans,!!!syms(inner))
    out <- gt(ans,groupname_col=names(outer))
  } else {
    ans <- rename(ans,!!!syms(outer))
    out <- gt(ans)
  }

  out <- cols_align(out, "left")
  out <- tab_sp_delim(out,delim = '.')

  out <- tab_source_note(
    out,
    "SUBJ: subjects; OBS: observations; MISS: missing; BQL: below quantitation limit"
  )
  out
}

data_inventory_stacked <- function(data,inner,stack_by,...) {
  data_inventory(
    data,
    outer = outer,
    inner = inner,
    inner_summary = FALSE,
    stacked = TRUE,
    ...
  )
}
to_html <- function(x) {
  if(interactive()) return(x)
  as_raw_html(x)
}
