#
# across <- c(Formulation =  "FORMf", Sex = "SEXf")
# down <- c(Renal = "RFf", `Race` = "ASIANf", Study = "STUDYf")
nn_factor <- function(x, col) {
  factor(x, levels = col, labels = names(col))
}

n_pct <- function(count, total) {
  pct <- digit1(100*count/total)
  as.character(glue("{count} ({pct})"))
}

#' Categorical by categorical table
#'
#' @export
pt_cat_cat <- function(data, cols_down, cols_across) {
  tab <- pt_cat_cat_data(data, cols_down, cols_across)
  pt_cat_cat_style(tab)
}
#' @rdname pt_cat_cat
#' @export
pt_cat_cat_style <- function(tab) {
    #final %>% st_new() %>%
    #st_panel("name") %>%
    #st_hline(at = 2, n = 2) %>%
    #st_span_split(sep = '__') %>%
    #st_blank("value") %>%
    sub_name <- grepl("Summary", tab$value[1])
    if(sub_name) {
      #tab$name[1] <- NA_character_
      #tab$value[1] <- NA_character_
    }

    object <- list(
      data = tab,
      panel = as.panel("name"),
      #hline_at = c(2,2),
      span_split = colsplit(sep = "__"),
      cols_blank = c("value"),
      sumrows = sumrow(nrow(tab), depanel =FALSE)
    )
    class(object) <- c("pmtable", class(object))
    object
}

#' @rdname pt_cat_cat
#' @export
pt_cat_cat_data <- function(data, cols_down, cols_across) {
  na_value <- "0 (0.00)"
  all_cols <- unname(c(across, down))
  check_discrete(data, cols_across)
  check_discrete(data, cols_down)
  across <- rename_cols(cols_across)
  down <- rename_cols(cols_down)
  NNN <- nrow(data) # The number of records

  long <- pivot_longer(data, cols = all_of(unname(down)))
  long2 <- pivot_longer(
    long,
    cols = all_of(unname(across)),
    names_to = "name2", values_to = "value2"
  )
  summ <- group_by(long2, name, name2, value, value2)
  summ <- count(summ)
  summ <- ungroup(summ)
  summ <- mutate(summ, name = nn_factor(name, down))
  summ <- mutate(summ, name2 = nn_factor(name2, across))
  summ <- mutate(summ, name2 = paste(name2, value2, sep = '__'))
  summ$value2 <- NULL

  # primary summary --------
  summ1 <- mutate(summ, summary = n_pct(n, NNN))
  summ1 <- pivot_wider(
    select(summ1, -n),
    names_from = "name2",
    values_from = "summary"
  )
  ncol <- ncol(summ1)
  summ1 <- mutate(summ1, across(seq(3, ncol), as.character))
  summ1 <- mutate(summ1, across(seq(3, ncol), replace_na, na_value))

  # summary of of down columns -------
  summ2 <- distinct(summ, name, value, n)
  summ2 <- group_by(summ2, name, value)
  summ2 <- summarise(summ2, n = sum(n))
  summ2 <- group_by(summ2, name)
  summ2 <- mutate(summ2, Summary = n_pct(n, sum(n)))
  summ2 <- ungroup(summ2)
  summ2$n <- NULL
  summ2 <- mutate(summ2, Summary = replace_na(Summary, na_value))

  # column summary -------
  # Count the across covariates
  summ3 <- pivot_longer(data, cols = all_of(unname(across)))
  summ3 <- group_by(summ3, name, value)
  summ3 <- count(summ3)
  summ3 <- ungroup(summ3)
  # Substitutes rename
  summ3 <- mutate(summ3, name = nn_factor(name, across))
  summ3 <- mutate(summ3, name = paste0(name, "__", value))
  summ3$value <- NULL
  summ3 <- mutate(summ3, Summary = n_pct(n, NNN))
  summ3 <- mutate(summ3, Summary = replace_na(Summary, na_value))
  summ3 <- pivot_wider(select(summ3, -n), values_from = "Summary")
  summ3 <- mutate(summ3, Summary = paste0(NNN, " (100.0)"))
  summ3$name <- "All data"
  summ3$value <- "Summary"
  # end column summary ------

  tab <- left_join(summ1, summ2, by = c("name", "value"))
  tab <- select(tab, name, value, everything())

  summ3 <- select(summ3, names(tab))
  tabl <- slice(tab, nrow(tab))
  for(j in seq(2,ncol(tab))) {
    tabl[j] <- ""
  }
  tabl[2] <- "\\hline"
  tab <- bind_rows(tab, tabl, summ3)
  #tab <- select(tab, name, value, Summary, everything())
  #names(tab) <- sub("Summary", tex_bold("Summary"), names(tab), fixed = TRUE)
  tab
}

#

#
#

#

#
# summ2 <- distinct(
#   summ,
#   name, value, n
# )
# summ2 <- group_by(summ2, name, value) %>%
#   summarise(n = sum(n)) %>% group_by(name) %>%
#   mutate(N = sum(n), pct = 100*n/N,
#          Summary = paste0(n, " (", pmtables:::digit1(pct), ")")) %>%
#   select(-n, -N, -pct) %>%
#   ungroup()
#
# summ1 <- left_join(summ1, summ2)
# summ1 <- select(summ1, name, value, everything())
#
#

#
# final <- summ1 %>% mutate(NNN= NULL)
# bot <- select(bot, names(final))
# final <- bind_rows(bot, final)
# names(final) <- sub("Summary", tex_bold("Summary"), names(final), fixed = TRUE)
