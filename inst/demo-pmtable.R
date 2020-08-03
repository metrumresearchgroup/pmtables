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
  by = c(Study = "STUDYf"),
  panel = as.panel("SEQf", prefix = "Endpoint: "),
  stacked = TRUE
) %>% as_stable(
  wrapw = TRUE,
  r_file = "test.R",
  output_file = "test.tex"
)


#' \clearpage


#' ## Paneled

#+ results = 'asis'

pt_data_inventory(
  d,
  by = c(Study = "STUDYf"),
  panel = "ASIANf"
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#' ## Grouped (by study)

#+ results = 'asis'

pt_data_inventory(
  d,
  by = c(Study = "STUDYf")
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")


#' \clearpage

#+ include = FALSE
data <- pmtables:::data("id")

#' # Wide categorical table

##' ## Basic

#+ results = 'asis'

pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf,Sex = SEXf, "Race group" = ASIANf)) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

##' ## Paneled (limited utility, IMO)

#+ results = 'asis'

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf, "Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: ")) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#+

#' \clearpage

##' ## Grouped (by male / female)

#+ results = 'asis'
pt_cat_wide(
  data = data,
  by = c(Sex = "SEXf"),
  cols = vars(Formulation = FORMf, "Race group" = ASIANf)) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#' ## Paneled and grouped

#+ results = 'asis'

pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf,"Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: "),
  by = c("RF Group" = "RFf")) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#+ include = TRUE

#' # Long categorical table

#' ## Ungrouped

#+ results = 'asis'
pt_cat_long(
  data = data,
  cols = vars(Study = STUDYf, Sex = SEXf, "Race group" = ASIANf, "Child-Pugh" = CPf)) %>%
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

