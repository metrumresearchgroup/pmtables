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

data <- pmt.obs
d <- filter(data, SEQ==1)

#' # Data inventory tables

#' ## Stacked by endpoint

out <- pt_data_inventory(
  data,
  by = c(Study = "STUDYf"),
  panel = as.panel("SEQf"),
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


#' ## Paneled

#+ include = FALSE
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


#' ## Grouped (by study)

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


#+ include = FALSE
data <- pmt.first

#' # Wide categorical table

##' ## Basic

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf, "Race group" = ASIANf)
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
  c("SEXf.male", "Sex.male"),
  c("SEXf.female","Sex.female" )
)

comp <- map_df(check, ~ tibble(ans = ans[[.x[1]]], out =  out$list[[.x[2]]]))
identical(comp$ans, comp$out)

#+

##' ## Paneled (limited utility, IMO)

#+ results = 'asis'

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf, "Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: ")) %>%
  as_stable(
    r_file = "test.R", output_file = "test.tex",
    inspect = TRUE
  ) %>% get_stable_data()

w <- pivot_longer(data, cols = c("FORMf", "SEXf", "ASIANf"))
w <- mutate(w, name = fct_inorder(name))
N <- length(unique(data$ID))
x0 <- group_by(w, STUDYf,name) %>% mutate(.N = n()) %>% ungroup()
x1 <- group_by(x0,STUDYf,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/.N[1]),
  .groups = "drop"
)
x2 <- group_by(w,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/N),
  .groups = "drop"
) %>% mutate(STUDYf = "ZZZ")
x <- bind_rows(x1,x2) %>%
  mutate(STUDYf = fct_inorder(STUDYf)) %>%
  arrange(STUDYf,name)

y <- mutate(
  x,
  name = paste0(name, '.', value),
  value = paste0(number, " (", percent, ")"),
  number = NULL, percent=NULL,
  name = gsub("-", "", name, fixed = TRUE)
)
z <- pivot_wider(y)
z

head(out$data) %>% as.data.frame
head(z) %>% as.data.frame
check <- list(
  c("ASIANf.Asian", "Race group.Asian"),
  c("ASIANf.nonAsian", "Race group.non-Asian"),
  c("FORMf.tablet", "Formulation.tablet"),
  c("FORMf.capsule", "Formulation.capsule"),
  c("FORMf.troche", "Formulation.troche"),
  c("SEXf.male", "Sex.male"),
  c("SEXf.female","Sex.female" )
)
comp <- map_df(check, ~ tibble(a = .x[1], b = .x[2], ans = z[[.x[1]]], out =  out$data[[.x[2]]]))
identical(comp$ans, comp$out)

#+

#' \clearpage

##' ## Grouped (by male / female)

#+ results = 'asis'
out <- pt_cat_wide(
  data = data,
  by = c(Sex = "SEXf"),
  cols = vars(Formulation = FORMf, "Race group" = ASIANf)) %>%
  as_stable(inspect=TRUE, r_file = "test.R", output_file = "test.tex") %>%
  get_stable_data()


w <- pivot_longer(data, cols = c("FORMf", "ASIANf"))
w <- mutate(w, name = fct_inorder(name))
N <- length(unique(data$ID))
x0 <- group_by(w,SEXf,name) %>% mutate(.N = n()) %>% ungroup()
x1 <- group_by(x0,SEXf,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/.N[1]),
  .groups = "drop"
)
x2 <- group_by(w,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/N),
  .groups = "drop"
)
x <- bind_rows(x1,x2) %>% arrange(name)

y <- mutate(
  x,
  name = paste0(name, '.', value),
  value = paste0(number, " (", percent, ")"),
  number = NULL, percent=NULL,
  name = gsub("-", "", name, fixed = TRUE)
)
z <- pivot_wider(y)
z

head(out$data) %>% as.data.frame
head(z) %>% as.data.frame
check <- list(
  c("ASIANf.Asian", "Race group.Asian"),
  c("ASIANf.nonAsian", "Race group.non-Asian"),
  c("FORMf.tablet", "Formulation.tablet"),
  c("FORMf.capsule", "Formulation.capsule"),
  c("FORMf.troche", "Formulation.troche")
)
comp <- map_df(check, ~ tibble(a = .x[1], b = .x[2], ans = z[[.x[1]]], out =  out$data[[.x[2]]]))
identical(comp$ans, comp$out)


#' ## Paneled and grouped

#+ results = 'asis'

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf,"Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: "),
  by = c("RF Group" = "RFf")) %>%
  as_stable(r_file = "test.R", output_file = "test.tex") %>%
  st_preview()

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf,"Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: "),
  by = c("RF Group" = "RFf")) %>%
  as_stable(inspect = TRUE, r_file = "test.R", output_file = "test.tex") %>%
  get_stable_data()

w <- pivot_longer(data, cols = c("FORMf","SEXf", "ASIANf"))
w <- mutate(w, name = fct_inorder(name))
N <- length(unique(data$ID))
x0 <- group_by(w,STUDYf,RFf,name) %>% mutate(.N = n()) %>% ungroup()
x1 <- group_by(x0,STUDYf,RFf,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/.N[1]),
  .groups = "drop"
)
x2 <- group_by(w,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/N),
  .groups = "drop"
) %>% mutate(STUDYf = "ZZZ")
x <- bind_rows(x1,x2) %>% arrange(name)

y <- mutate(
  x,
  name = paste0(name, '.', value),
  value = paste0(number, " (", percent, ")"),
  number = NULL, percent=NULL,
  name = gsub("-", "", name, fixed = TRUE)
)
z <- pivot_wider(y)

z <- mutate(z, FORMf.capsule = replace_na(FORMf.capsule,"0 (0.0)"))

head(out$data) %>% as.data.frame
head(z) %>% as.data.frame
check <- list(
  c("ASIANf.Asian", "Race group.Asian"),
  c("ASIANf.nonAsian", "Race group.non-Asian"),
  c("FORMf.tablet", "Formulation.tablet"),
  c("FORMf.capsule", "Formulation.capsule"),
  c("FORMf.troche", "Formulation.troche"),
  c("SEXf.male", "Sex.male"),
  c("SEXf.female", "Sex.female")
)
comp <- map_df(check, ~ tibble(a = .x[1], b = .x[2], ans = z[[.x[1]]], out =  out$data[[.x[2]]]))
identical(comp$ans, comp$out)


#' # Long categorical table

#' ## Ungrouped

#+ results = 'asis'
out <- pt_cat_long(
  data = data,
  cols = vars(Study = STUDYf, Sex = SEXf, "Race group" = ASIANf, "Child-Pugh" = CPf)) %>%
  as_stable(inspect=TRUE, r_file = "test.R", output_file = "test.tex") %>%
  get_stable_data()

w <- pivot_longer(data, cols = c("STUDYf","SEXf", "ASIANf", "CPf"))
w <- mutate(w, name = fct_inorder(name))
N <- length(unique(data$ID))
x0 <- group_by(w,name) %>% mutate(.N = n()) %>% ungroup()
x1 <- group_by(x0,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/.N[1]),
  .groups = "drop"
)

x <- x1
y <- mutate(
  x,
  name = paste0(name, '.', value),
  value = paste0(number, " (", percent, ")"),
  number = NULL, percent=NULL,
  name = gsub("-", "", name, fixed = TRUE)
)

z <- y

all.equal(unname(z[,2]), unname(out$data[,2]))


#' ## Gropued (by formulation)

#+ results = 'asis'
out <- pt_cat_long(
  data = data,
  cols = vars(Study = STUDYf,Sex = SEXf,"Race group" = ASIANf, "Child-Pugh" = CPf),
  by = c(Formulation = "FORMf")) %>%
  as_stable(inspect = TRUE, r_file = "test.R", output_file = "test.tex") %>%
  get_stable_data()


w <- pivot_longer(data, cols = c("STUDYf","SEXf", "ASIANf", "CPf"))
w <- mutate(w, name = fct_inorder(name))
N <- length(unique(data$ID))
x0 <- group_by(w,FORMf,name) %>% mutate(.N = n()) %>% ungroup()
x1 <- group_by(x0,FORMf,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/.N[1]),
  .groups = "drop"
)

x2 <- group_by(w,name,value) %>% summarise(
  number = length(unique(ID)),
  percent = pmtables:::digit1(100*number/N),
  .groups = "drop"
) %>% mutate(FORMf = "ALL")
x <- bind_rows(x1,x2)


x <- bind_rows(x1,x2)

y <- mutate(
  x,
  name = FORMf,
  FORMf = NULL,
  val  = value,
  value = paste0(number, " (", percent, ")"),
  number = NULL, percent=NULL,
  name = gsub("-", "", name, fixed = TRUE)
)

z <- pivot_wider(y)

z <- mutate(z, troche = replace_na(troche, "0 (0.0)"))

status <- map_lgl(c(2,3,4,5), ~ all(z[[.x]]==out$data[[.x]]))
assert_that(all(status))


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

