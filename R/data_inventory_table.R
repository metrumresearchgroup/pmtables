
data_inventory_chunk <- function(data,outer,inner=outer, stacked = FALSE,
                                 tot=FALSE, all_name = "all",
                                 dv_col = "DV",
                                 bq_col = c("BQL", "BLQ"), id_col = "ID",...) {

  if(outer==".total" | inner==".total") {
    data <- data_total_col(data,all_name = all_name)
  }

  bq_col <- match.arg(bq_col)

  by <- unique(c(outer,inner))

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
  summ <- left_join(body,bq,by = unique(c(outer,inner)))
  summ <- select(
    summ,
    !!sym(outer),
    !!sym(inner),
    SUBJ,
    NMISS,
    NOBS,
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

#' @export
data_inventory_data <- function(data, outer,inner=outer,all_name = "all",
                                stacked=FALSE,...) {
  outer <- unname(outer)
  inner <- unname(inner)

  if(stacked) {
    ans <- data_inventory_data_split(data,outer,inner, stacked = FALSE, ...)
    return(ans)
  }

  data <- data_total_col(data, all_name = all_name)

  ans <- data_inventory_chunk(
    data = data,
    outer = outer,
    inner = inner,
    all_name = all_name,
    stacked = stacked,
    ...
  )

  if(outer != ".total") {
    tot <- data_inventory_chunk(
      data, outer = ".total", inner=".total", stacked = FALSE,
      all_name = all_name, ...
    )
    tot <- mutate(tot, .total = all_name)
    ans <- bind_rows(ans,tot)
  }

  if(inner!=outer) {
    ans <- mutate(ans, !!sym(inner) := replace_na(!!sym(inner),".total"))
    ans <- fill(ans, !!sym(outer), .direction = "down")
  }
  if(inner==outer) {
    ans <- mutate(ans, !!sym(outer) := replace_na(!!sym(outer),".total"))
  }
  ans
}

#' @export
pt_data_inventory <- function(data, outer = ".total", inner = outer, ...,
                              inner_summary = TRUE, drop_miss = FALSE,
                              stacked = FALSE, table = NULL, subset = TRUE,
                              all_name = "all", skip_total = FALSE) {

  subset <- enquo(subset)
  data <- filter(data, !!subset)

  outer <- new_names(outer,table)
  inner <- new_names(inner,table)

  if(inner==outer | stacked) {
    inner_summary <- FALSE
  }

  total_name <- ifelse(stacked, "Group Total", "Grand Total")

  ans <- data_inventory_data(
    data,
    outer = outer,
    inner = inner,
    stacked = stacked,
    all_name = all_name,
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
    ans <- mutate(ans,POBS=NULL,PBQL=NULL)
  }

  ans <- rename(
    ans,
    Number.SUBJ = SUBJ,
    Number.MISS = NMISS,
    Number.OBS = NOBS,
    Number.BQL = NBQL
  )

  if(drop_miss) {
    if(all(ans$MISS==0)) {
      ans <- mutate(ans, MISS = NULL)
    }
  }

  if(inner!=outer) {
    ans <- mutate(
      ans,
      .total = NULL,
      !!sym(outer) := paste0(names(outer), ": ", !!sym(outer))
    )
    ans <- rename(ans,inner)
    ans <- rename(ans,outer)
    out <- gt(ans,groupname_col=names(outer))
  }
  if(inner==outer & outer != ".total") {
    ans <- rename(ans, outer)
    ans <- mutate(ans,.total = NULL)
    out <- gt(ans)
  }
  if(outer==".total" & inner==outer ) {
    ans <- mutate(ans,.total = NULL)
    out <- gt(ans)
  }

  out <- tab_sp_delim(out,delim = '.')

  out <- tab_source_note(
    out,
    "SUBJ: subjects; OBS: observations; MISS: missing;
     BQL: below quantitation limit"
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
