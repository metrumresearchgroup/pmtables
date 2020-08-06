library(tibble)
library(dplyr)
library(pmtables)
library(tidyverse)
library(tidyselect)
library(assertthat)
library(yspec)
options(tibble.width = Inf)
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
)


a <- group_by(d, ASIANf,STUDYf)
b <- summarize(
  a,
  NID = length(unique(SUBJ)),
  NMISS = sum(is.na(DV) & BQL==0),
  NBQL = sum(BQL !=0),
  NOBS = sum(!is.na(DV)),
  .groups = "drop"
)
dd <- b %>%
  mutate(TOBS = sum(NOBS), TBQL = sum(NBQL)) %>%
  ungroup()
ddd <- group_by(dd, ASIANf) %>%
  mutate(GOBS = sum(NOBS), GBQL = sum(NBQL)) %>%
  ungroup()

e <- mutate(
  ddd,
  POBS = 100*NOBS/TOBS,
  PBQL = 100*NBQL/TOBS,
  GPOBS = 100*NOBS/GOBS,
  GPBQL = 100*NBQL/GOBS
)
f <- mutate(e, across(c(POBS,PBQL), .fns = ~ifelse(is.nan(.x), 0, .x)))

g <- summarise_at(f, -c(1,2), sum)
h <- bind_rows(f,g)

i <- mutate(h, across(POBS:PBQL, pmtables:::digit1))
j <- mutate(i, across(GPOBS:GPBQL, pmtables:::digit1))
j$GPOBS[nrow(j)] <- "---"
j$GPBQL[nrow(j)] <- "---"

k <- rename(
  j,
  Number.SUBJ = NID,
  Number.MISS = NMISS,
  Number.BQL = NBQL,
  Number.OBS = NOBS,
  `Group percent.OBS` = GPOBS,
  `Group percent.BQL` = GPBQL,
  `Overall percent.OBS` = POBS,
  `Overall percent.BQL` = PBQL
)
chk <- names(out$data)[3:10]
assert_that(identical(out$data[chk], k[chk]))


#' ## Grouped (by study)

out <- pt_data_inventory(
  d,
  by = vars(Study = "STUDYf")
)

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

k <- rename(
  i,
  Number.SUBJ = NID,
  Number.MISS = NMISS,
  Number.BQL = NBQL,
  Number.OBS = NOBS,
  `Percent.OBS` = POBS,
  `Percent.BQL` = PBQL
)
all.equal(out$data, k)
chk <- c(
  "Number.SUBJ", "Number.MISS", "Number.BQL",
  "Number.OBS", "Percent.OBS", "Percent.BQL"
)

assert_that(identical(k[chk], out$data[chk]))

#+ include = FALSE
data <- pmt.first

#' # Wide categorical table

##' ## Basic

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf, "Race group" = ASIANf)
)

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
  panel = as.panel("STUDYf", prefix = "Study: "))

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
  cols = vars(Formulation = FORMf, "Race group" = ASIANf))

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
  by = c("RF Group" = "RFf")
)

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
  cols = vars(
    Study = STUDYf, Sex = SEXf,
    "Race group" = ASIANf,
    "Child-Pugh" = CPf
  )
)

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
  value = as.character(value),
  Summary = paste0(number, " (", percent, ")"),
  number = NULL, percent=NULL,
  name = gsub("-", "", name, fixed = TRUE)
)

z <- y

identical(unname(z[,c(2,3)]), unname(out$data[,c(2,3)]))


#' ## Gropued (by formulation)

#+ results = 'asis'
out <- pt_cat_long(
  data = data,
  cols = vars(Study = STUDYf,Sex = SEXf,"Race group" = ASIANf, "Child-Pugh" = CPf),
  by = c(Formulation = "FORMf"))


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
z <- mutate(z, val = as.character(val))

z <- rename(z, `\\ ` = val, `\\textbf{All Groups}` = ALL)

chk <- names(z)

identical(z[chk], out$data[chk])


#' # Wide continuous table

#' ## Ungrouped

#+ results = "asis"
out <- pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  units = units
)

fun <- function(x,id=1) {
  a <- sig(mean(x, na.rm=TRUE))
  b <- sig(sd(x, na.rm = TRUE))
  c <- length(unique(id[!is.na(x)]))
  paste0(a, " (",b,") [", c, "]", collapse = "")
}

x1 <- summarise(
  data,
  across(c(WT,SCR,AGE,ALB,HT),fun, id = ID)
)

assert_that(identical(out$data, x1))


##' ## Paneled

#+ results = "asis"
out <- pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  panel = c(Study = "STUDYf"),
  units = units
)

fun <- function(x,id=1) {
  a <- sig(mean(x, na.rm=TRUE))
  b <- sig(sd(x, na.rm = TRUE))
  c <- length(unique(id[!is.na(x)]))
  paste0(a, " (",b,") [", c, "]", collapse = "")
}

x1 <- data %>%
  group_by(STUDYf) %>%
  summarise(
    across(c(WT,SCR,AGE,ALB,HT),fun, id = ID),
    .groups = "drop"
  )

x2 <- data %>%
  summarise(
    across(c(WT,SCR,AGE,ALB,HT),fun, id = ID),
    .groups = "drop"
  )

x <- bind_rows(x1,x2)
x <- mutate(x, STUDYf = as.character(STUDYf))
x <- mutate(x, STUDYf = replace_na(STUDYf, "All data"))

assert_that(identical(out$data, x))

#' \clearpage

#' ## Grouped (by study)

#+ results = "asis"
out <- pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  by = c(Study = "STUDYf"),
  units = units
)

x1 <- data %>%
  group_by(STUDYf) %>%
  summarise(
    across(c(WT,SCR,AGE,ALB,HT),fun, id = ID),
    .groups = "drop"
  )

x2 <- data %>%
  summarise(
    across(c(WT,SCR,AGE,ALB,HT),fun, id = ID),
    .groups = "drop"
  ) %>% mutate(STUDYf = "\\hline \\hline {\\bf All data}")

x <- bind_rows(x1,x2)
x <- rename(x, Study = STUDYf)

assert_that(identical(out$data, x))


#' \clearpage

#' ## Paneled and grouped

#+ results = "asis"
out <- pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  by = c(Study = "STUDYf"),
  panel = c(Formulation = "FORMf"),
  units = units
)


x1 <- data %>%
  group_by(FORMf,STUDYf) %>%
  summarise(
    across(c(WT,SCR,AGE,ALB,HT),fun, id = ID),
    .groups = "drop"
  )

x2 <- data %>%
  summarise(
    across(c(WT,SCR,AGE,ALB,HT),fun, id = ID),
    .groups = "drop"
  )

x <- bind_rows(x1,x2) %>% select(-STUDYf)
x <- mutate(x, FORMf = as.character(FORMf))
x <- mutate(x, FORMf = replace_na(FORMf, "All data"))

assert_that(identical(out$data, x))

#' # Long continuous table

#' ## Ungrouped

#+ results = 'asis'
out <-
  pt_cont_long(
    data = data,
    cols = "WT,SCR,AGE",
    units = units
  )

w <- pivot_longer(data, cols = c("WT", "SCR", "AGE"))
w <- mutate(w, name = fct_inorder(name))
x1 <- group_by(w, Variable = name)  %>%
  summarise(
    n = length(value[!is.na(value)]),
    Mean = sig(mean(value, na.rm=TRUE)),
    Median = sig(median(value, na.rm = TRUE)),
    SD = sig(sd(value, na.rm = TRUE)),
    Min = sig(min(value,na.rm=TRUE)),
    Max = sig(max(value,na.rm=TRUE)),
    `Min / Max` = paste0(Min , " / ", Max)
  ) %>% select(-Min, -Max)
x2 <- mutate(x1,Variable = as.character(Variable))
x <- mutate(x2, Variable = paste0(Variable, c(" (kg)", " (mg/dL)", " (years)")))

assert_that(identical(out$data, x))

#' \clearpage

#+ results='asis'
out <- pt_cont_long(
  data = data,
  cols = "WT,SCR,AGE",
  panel = vars(Study = STUDYf),
  units = units)

w <- pivot_longer(data, cols = c("WT", "SCR", "AGE"))
w <- mutate(w, name = fct_inorder(name))
x1 <- group_by(w,STUDYf, Variable = name)  %>%
  summarise(
    n = length(value[!is.na(value)]),
    Mean = sig(mean(value, na.rm=TRUE)),
    Median = sig(median(value, na.rm = TRUE)),
    SD = sig(sd(value, na.rm = TRUE)),
    Min = sig(min(value,na.rm=TRUE)),
    Max = sig(max(value,na.rm=TRUE)),
    `Min / Max` = paste0(Min , " / ", Max),
    .groups = "drop"
  ) %>% select(-Min, -Max)
x2 <- mutate(x1,Variable = as.character(Variable))
x2 <- ungroup(x2)

x3 <- group_by(w, Variable = name)  %>%
  summarise(
    n = length(value[!is.na(value)]),
    Mean = sig(mean(value, na.rm=TRUE)),
    Median = sig(median(value, na.rm = TRUE)),
    SD = sig(sd(value, na.rm = TRUE)),
    Min = sig(min(value,na.rm=TRUE)),
    Max = sig(max(value,na.rm=TRUE)),
    `Min / Max` = paste0(Min , " / ", Max),
    .groups = "drop"
  ) %>% select(-Min, -Max)

x <- bind_rows(x2,x3)
x <- mutate(x, Variable = paste0(Variable, c(" (kg)", " (mg/dL)", " (years)")))
x <- mutate(x, STUDYf = as.character(STUDYf))
x <- mutate(x, STUDYf = replace_na(STUDYf, "All data"))
all.equal(out$data,x)
assert_that(identical(out$data, x))
