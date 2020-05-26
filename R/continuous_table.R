
#' @export
cont_table_data <- function(data,cols,by=NULL,by_col=FALSE,fun=df_sum_2,
                            wide = FALSE, digits = list(), table=NULL) {

  cols <- unname(cvec_cs(cols))
  by <- unname(by)

  data <- data_total_col(data)

  digit_fun <- getOption("mrg.table.digit.fun", sig)
  digit_default <- getOption("mrg.table.digits", 3)
  digits <- update_list(def_digits(cols,digit_default),digits)

  groups <- c("name")
  if(!is.null(by)) groups <- c(by,groups)

  d0 <- select(data, all_of(unname(c(by,cols))))

  d1 <- pivot_longer(d0,all_of(cols))
  d1 <- mutate(d1,digits = digits[name])
  d1 <- mutate(d1,name = fct_inorder(name))

  if(!is.null(by)) {
    d1 <- group_by(d1,!!!syms(groups))
    join_cols <- c(by,"name")
  } else {
    d1 <- group_by(d1,!!!syms(groups))
    join_cols <- "name"
  }

  d2 <- group_modify(
    d1,
    ~fun(
      value = .$value,
      digit_fun = partial(digit_fun,digits=digits[[1]]),
      name = .$name[1]
    ),
    .keep = TRUE
  )

  d3 <- count(d1,!!!syms(groups))
  d4 <- left_join(d2,d3,by = join_cols)
  d4 <- rename(d4, outer = !!sym(by), inner = name)
  if(wide) {
    d4 <- pivot_wider(d4, names_from  = "inner", values_from = "summary")
  }
  d4 <- ungroup(d4)
  return(d4)
}

#' @export
pt_cont_wide <- function(data, cols, by = ".total",
                         panel = NULL,
                         fun = str_sum_2,
                         table=NULL,units=NULL,all_name="All data") {

  tst <- fun(rnorm(10))
  assert_that(identical(names(tst),"summary"))

  cols <- new_names(cols,table)
  by <- new_names(by,table)

  check_continuous(data,cols)
  check_discrete(data,by)

  ans <- cont_table_data(
    data=data,
    cols=cols,
    by=by,
    fun=fun,
    wide = TRUE
  )

  all_summary <- FALSE
  if(by != ".total") {
    all_summary <- TRUE
    ans2 <- cont_table_data(
      data=data,
      cols=cols,
      by=".total",
      fun=fun,
      wide = TRUE
    )
    ans2 <- mutate(ans2, outer := all_name)
    ans <- bind_rows(ans,ans2)
  }

  out <- gt(
    ans,
    row_group.sep=" "
  )

  if(all_summary) {
    out <- tab_row_group(
      out,
      group = "Total",
      rows = ans[[1]]==all_name
    )
    out <- row_group_order(
      out,
      groups = c(NA, "Total")
    )
  }

  out <- cols_label(out, outer = names(by)[1])
  if(is.list(units)) {
    for(col in cols) {
      if(exists(col,units)) {
        lab <- paste0(col, " [", units[[col]],"]")
        out <- cols_label(out, !!col := lab)
      }
    }
  }
  out <- tab_stubhead(out,"Variable")
  out <- cols_align(out,"right")
  if(is.logical(formals(fun)[["footnote"]])) {
    footn <- fun(footnote = TRUE)
    out <- tab_source_note(
      out,
      footn
    )
  }
  if(is.list(table)) {
    out <-
      tab_footnote(
        out,
        footnote = foot(table,unname(cols)),
        locations = cells_stubhead()
      )
  }
  out
}


#' @export
pt_cont_long <- function(data,cols,
                         panel=".total",
                         fun = df_sum_2,
                         table=NULL,
                         units=NULL, all_name="All data",
                         summarize_total = TRUE) {

  by <- panel
  summarize_total <- summarize_total & by != ".total"
  data <- data_total_col(data)

  cols <- new_names(cols,table)
  by <- new_names(by,table)

  check_continuous(data,cols)
  check_discrete(data,by)

  ans <- cont_table_data(
    data=data,
    cols=unname(cols),
    by=unname(by),
    fun=fun,
    wide = FALSE
  )

  ans <- mutate(ans,outer=paste(names(by)[1],outer))
  if(by==".total") ans <- mutate(ans,outer=all_name)

  if(summarize_total) {
    ans2 <- cont_table_data(
      data = data,
      cols = unname(cols),
      by = ".total",
      fun = fun,
      wide = FALSE
    )
    ans2 <- mutate(ans2, outer = all_name)
    ans <- bind_rows(ans,ans2)
  }

  ans <- mutate(ans, n = n_parens(n))

  if(is.list(units) & rlang::is_named(units)) {
    ans <- mutate(
      ans,
      inner = as.character(inner),
      Unit = as.character(units[inner])
    )
  }
  out <- gt(
    ans,
    row_group.sep=" ",
    rowname_col = "inner",
    groupname_col = c("outer", "n")
  )

  out <- cols_label(out, outer = names(by)[1])
  out <- tab_stubhead(out,"Variable")
  out <- cols_align(out,"right")

  if(is.list(table)) {
    out <-
      tab_footnote(
        out,
        footnote = foot(table,unname(cols)),
        locations = cells_stubhead()
      )
  }

  if(is.logical(formals(fun)[["footnote"]])) {
    footn <- fun(footnote = TRUE)
    if(is.list(footn)) {
      out <- tab_footnote(
        out,
        footnote = footn$footnote,
        locations = footn$locations
      )
    }
  }
  out
}

#' @export
pt_cont_study <- function(data,cols,study_col = "STUDY",wide = FALSE,...) {
  if(wide) {
    pt_cont_wide(
      data = data,
      cols = cols,
      by = study_col,
      ...
    )
  } else {
    pt_cont_long(
      data = data,
      cols = cols,
      panel = study_col,
      ...
    )
  }
}

