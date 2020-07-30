#' ---
#' title: "Previous pmtables implemented with stable framework"
#' author: ""
#' date: ""
#' output:
#'   pdf_document:
#'     extra_dependencies:
#'       threeparttable:
#'       array:
#'       booktabs:
#'       mathdesign: utopia
#'     toc: true
#'     number_sections: true
#' ---

#+ include = FALSE
library(tibble)
library(dplyr)
library(pmtables)
library(tidyverse)
library(tidyselect)
library(assertthat)
library(yspec)

#' # Setup

#+ include = TRUE
units = ys_get_unit(ys_help$spec(), parens = TRUE)

#+ include = TRUE

data <- pmtables:::data("all") %>% filter(SEQ > 0)
d <- filter(data, SEQ==1)

#' \clearpage


#' # Data inventory tables

#' ## Stacked by endpoint


#+ pt-inventory-data-stacked, results = 'asis'
x <- pt_data_inventory(
  data,
  by = vars(Study = "STUDYf"),
  panel = vars("Endpoint" = "SEQf"),
  stacked = TRUE
) %>% as_stable(
  wrapw = TRUE, r_file = "test.R", output_file = "test.tex",
  panel = rowpanel(c("Endpoint:" = "SEQf"), prefix_name = TRUE)
)

#+ include = FALSE
out <- pt_data_inventory(
  data,
  by = vars(Study = "STUDYf"),
  panel = vars("Endpoint" = "SEQf"),
  stacked = TRUE
)

a <- group_by(data, SEQf,STUDYf)
b <- summarize(
  a,
  NID = length(unique(SUBJ)),
  NMISS = sum(is.na(DV) & BQL==0),
  NBQL = sum(BQL !=0),
  NOBS = sum(!is.na(DV)),
  .groups = "drop"
)
dd <- group_by(b, SEQf) %>% mutate(TOBS = sum(NOBS), TBQL = sum(NBQL)) %>% ungroup()
e <- mutate(dd, POBS = 100*NOBS/TOBS, PBQL = 100*NBQL/TOBS)
f <- mutate(e, across(c(POBS,PBQL), .fns = ~ifelse(is.nan(.x), 0, .x)))
g <- f
i <- map_df(split(g, g$SEQf, drop=TRUE), function(df) {
  df2 <- select_if(df, is.numeric)
  bind_rows(df,summarize(df2,across(everything(), sum)))
})
j <- mutate(i, across(POBS:PBQL, pmtables:::digit1))

assert_that(all(out$data$Number.SUBJ==j$NID))
assert_that(all(out$data$Number.OBS==j$NOBS))
assert_that(all(out$data$Number.BQL==j$NBQL))
assert_that(all(out$data$Number.MISS==j$NMISS))
assert_that(all(out$data$Percent.BQL==j$PBQL))
assert_that(all(out$data$Percent.OBS==j$POBS))

#+

#' \clearpage


#' ## Paneled

#+ results = 'asis'

pt_data_inventory(
  d,
  by = vars(Study = "STUDYf"),
  panel = vars(Race = ASIANf)
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#+
out <- pt_data_inventory(
  d,
  by = vars(Study = "STUDYf"),
  panel = vars(Race = ASIANf)
) %>% as_stable(inspect = TRUE) %>% get_stable_data()

a <- group_by(d, ASIANf,STUDYf)
b <- summarize(
  a,
  NID = length(unique(SUBJ)),
  NMISS = sum(is.na(DV) & BQL==0),
  NBQL = sum(BQL !=0),
  NOBS = sum(!is.na(DV)),
  .groups = "drop"
)
dd <- b %>% mutate(TOBS = sum(NOBS), TBQL = sum(NBQL)) %>% ungroup()
ddd <- group_by(dd, ASIANf) %>% mutate(GOBS = sum(NOBS), GBQL = sum(NBQL)) %>% ungroup()
e <- mutate(
  ddd,
  POBS = 100*NOBS/TOBS,
  PBQL = 100*NBQL/TOBS,
  GPOBS = 100*NOBS/GOBS,
  GPBQL = 100*NBQL/GBQL
)
f <- mutate(e, across(c(POBS,PBQL), .fns = ~ifelse(is.nan(.x), 0, .x)))

g <- summarise_at(f, -c(1,2), sum)
h <- bind_rows(f,g)
i <- mutate(h, across(POBS:PBQL, pmtables:::digit1))


check <- list(
  c("NID", "Number.SUBJ"),
  c("NMISS", "Number.MISS"),
  c("NOBS", "Number.OBS"),
  c("NBQL", "Number.BQL"),
  c("POBS", "Overall percent.OBS"),
  c("PBQL", "Overall percent.BQL")
)

ans <- map_lgl(check, ~ identical(i[[.x[1]]], out$data[[.x[2]]]))
assert_that(all(ans))


#+

#' \clearpage

#' ## Grouped (by study)

#+ results = 'asis'

pt_data_inventory(
  d,
  by = vars(Study = "STUDYf")
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#+ include = FALSE
out <- pt_data_inventory(
  d,
  by = vars(Study = "STUDYf")
) %>% as_stable(inspect = TRUE) %>% get_stable_data()

a <- group_by(d,STUDYf)
b <- summarize(
  a,
  NID = length(unique(SUBJ)),
  NMISS = sum(is.na(DV) & BQL==0),
  NBQL = sum(BQL !=0),
  NOBS = sum(!is.na(DV)),
  .groups = "drop"
)
dd <- b %>% mutate(TOBS = sum(NOBS), TBQL = sum(NBQL)) %>% ungroup()
e <- mutate(
  dd,
  POBS = 100*NOBS/TOBS,
  PBQL = 100*NBQL/TOBS
)
f <- mutate(e, across(c(POBS,PBQL), .fns = ~ifelse(is.nan(.x), 0, .x)))

g <- summarise_at(f, -c(1), sum)
h <- bind_rows(f,g)
i <- mutate(h, across(POBS:PBQL, pmtables:::digit1))


check <- list(
  c("NID", "Number.SUBJ"),
  c("NMISS", "Number.MISS"),
  c("NOBS", "Number.OBS"),
  c("NBQL", "Number.BQL"),
  c("POBS", "Percent.OBS"),
  c("PBQL", "Percent.BQL")
)

ans <- map_lgl(check, ~ identical(i[[.x[1]]], out$data[[.x[2]]]))
assert_that(all(ans))


#+

#' \clearpage

#+ include = FALSE
data <- pmtables:::data("id")

#' # Wide categorical table

##' ## Basic

#+ results = 'asis'

pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf,Sex = SEXf,"Race group" = ASIANf)) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#+ include = FALSE
out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf,Sex = SEXf,"Race group" = ASIANf)
) %>% as_stable(inspect = TRUE) %>% get_stable_data()

w <- pivot_longer(data, cols = c("FORMf", "SEXf", "ASIANf"))
N <- length(unique(data$ID))
x <- group_by(w,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/N),
  .groups = "drop"
)
y <- mutate(
  x,
  name = paste0(name, '.', value),
  value = paste0(number, " (", percent, ")"),
  number = NULL, percent=NULL,
  name = gsub("-", "", name, fixed = TRUE)
)
ans <- y$value
names(ans) <- y$name
out$list <- as.list(out$data)
check <- list(
  c("ASIANf.Asian", "Race group.Asian"),
  c("ASIANf.nonAsian", "Race group.non-Asian"),
  c("FORMf.tablet", "Formulation.tablet"),
  c("FORMf.capsule", "Formulation.capsule"),
  c("FORMf.troche", "Formulation.troche"),
  c("SEXf.male", "Sex.female"),
  c("SEXf.female","Sex.male" )
)
comp <- map_df(check, ~ tibble(ans = ans[[.x[1]]], out =  out$list[[.x[2]]]))
identical(comp$ans, comp$out)

#+

##' ## Paneled (limited utility, IMO)

#+ results = 'asis'

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf,Sex = SEXf,"Race group" = ASIANf),
  panel = c(Study = "STUDYf")) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")


#+ include = FALSE
out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf,Sex = SEXf,"Race group" = ASIANf),
  panel = c(Study = "STUDYf")) %>%
  as_stable(inspect=TRUE) %>% get_stable_data()
as.data.frame(out$data)

#+

#' \clearpage

##' ## Grouped (by male / female)

#+ results = 'asis'
pt_cat_wide(
  data = data,
  by = vars(Sex = SEXf),
  cols = vars(Formulation = FORMf,"Race group" = ASIANf)) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#' ## Paneled and grouped

#+ results = 'asis'

pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf,"Race group" = ASIANf),
  panel = c(Study = "STUDYf"),
  by = c("RF Group" = "RFf")) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#+ include = TRUE

#' # Long categorical table

#' ## Ungrouped

#+ results = 'asis'
pt_cat_long(
  data = data,
  cols = vars(Study = STUDYf,Sex = SEXf,"Race group" = ASIANf, "Child-Pugh" = CPf)) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage



#' ## Gropued (by formulation)

#+ results = 'asis'
pt_cat_long(
  data = data,
  cols = vars(Study = STUDYf,Sex = SEXf,"Race group" = ASIANf, "Child-Pugh" = CPf),
  by = c(Formulation = "FORMf")) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage



#' # Wide continuous table

#' ## Ungrouped

#+ results = "asis"
pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  units = units
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")


##' ## Paneled

#+ results = "asis"
pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  panel = c(Study = "STUDYf"),
  units = units
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#' ## Grouped (by study)

#+ results = "asis"
pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  by = c(Study = "STUDYf"),
  units = units
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#' ## Paneled and grouped

#+ results = "asis"
pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  by = c(Study = "STUDYf"),
  panel = c(Formulation = "FORMf"),
  units = units
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage



#' # Long continuous table

#' ## Ungrouped

#+ results = 'asis'
pt_cont_long(
  data = data,
  cols = "WT,SCR,AGE",
  units = units) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#+ results='asis'
pt_cont_long(
  data = data,
  cols = "WT,SCR,AGE",
  panel = vars(Study = STUDYf),
  units = units) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

