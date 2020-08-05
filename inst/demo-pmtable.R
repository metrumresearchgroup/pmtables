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

data <- pmt_first
data_pk <- pmt_pk
data_all <- pmt_obs

#' \clearpage


#' # Data inventory tables
#'
#' - Count number of
#'   - individuals
#'   - observations
#'   - BQL observations
#'   - missing values
#' - Calculate the percent  of observations or BQL in different sub groups
#'

#' \clearpage
#'
#' ## Stacked by endpoint
#'
#' - The stacked plot creates multiple independent tables to summarize different
#' endpoints; there is no single overall summary for the table because we
#' are summarizing different endpoints
#'


#+ pt-inventory-data-stacked, results = 'asis'
x <- pt_data_inventory(
  data_all,
  by = c(Study = "STUDYf"),
  panel = as.panel("SEQf", prefix = "Endpoint: "),
  stacked = TRUE
) %>% as_stable( wrapw = TRUE, r_file = "test.R", output_file = "test.tex")


#' \clearpage


#' ## Paneled
#'
#' - Just summarize a single endpoint

#+ results = 'asis'

pt_data_inventory(
  data_pk,
  by = c(Study = "STUDYf"),
  panel = "ASIANf"
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage

#' ## Grouped (by study)

#+ results = 'asis'

pt_data_inventory(
  data_pk,
  by = c(Study = "STUDYf")
) %>% as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")


#' \clearpage

#+ include = FALSE


#' # Wide categorical table
#'
#' - Summary of categorical data in wide format
#' - The summary is `number (percent within group)`
#' - Wide refers to the fact that the covariates go across the table
#'

##' ## Basic

#+ results = 'asis'

pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf,Sex = SEXf, "Race group" = ASIANf)) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#' \clearpage
#'
#' ## Paneled (limited utility, IMO)
#'
#' - Provided here for completeness

#+ results = 'asis'

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf, "Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: ")) %>%
  as_stable(wrapw = TRUE, r_file = "test.R", output_file = "test.tex")

#+

#' \clearpage

#' ## Grouped (by male / female)

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
#'
#' - Categorical table in long format
#' - Long indicates that the covariates go down the table

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
#'
#' - Continuous table in wide format
#' - Wide means that the covariates go across the table
#'

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
#'
#' - Continuous summary table in long format
#' - Long indicates that covariates go down the table
#'

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

