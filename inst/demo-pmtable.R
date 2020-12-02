#' ---
#' title: "Previous pmtables implemented with stable framework"
#' author: ""
#' date: ""
#' output:
#'   pdf_document:
#'     extra_dependencies:
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


#+ pt-inventory-data-stacked
out <- pt_data_inventory(
  data_all,
  by = c(Study = "STUDYf"),
  panel = as.panel("SEQf", prefix = "Endpoint: "),
  stacked = TRUE
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()


#' \clearpage


#' ## Paneled
#'
#' - Just summarize a single endpoint

out <- pt_data_inventory(
  data_pk,
  by = c(Study = "STUDYf"),
  panel = "ASIANf"
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage

#' ## Grouped (by study)


out <- pt_data_inventory(
  data_pk,
  by = c(Study = "STUDYf")
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()


#' \clearpage

#+ include = FALSE


#' # Wide categorical table
#'
#' - Summary of categorical data in wide format
#' - The summary is `number (percent within group)`
#' - Wide refers to the fact that the covariates go across the table
#'

##' ## Ungrouped


out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf, "Race group" = ASIANf)
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage
#'
#' ## Paneled (limited utility, IMO)
#'
#' - Provided here for completeness


out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf, "Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: ")
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#+

#' \clearpage

#' ## Grouped (by male / female)

out <- pt_cat_wide(
  data = data,
  by = c(Sex = "SEXf"),
  cols = vars(Formulation = FORMf, "Race group" = ASIANf)
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage

#' ## Paneled and grouped

out <- pt_cat_wide(
  data = data,
  cols = vars(Formulation = FORMf, Sex = SEXf,"Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: "),
  by = c("RF Group" = "RFf")
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage

#' ## No summary

out <- pt_cat_wide(
  data = data,
  summarize = "none",
  cols = vars(Formulation = FORMf, Sex = SEXf,"Race group" = ASIANf),
  panel = as.panel("STUDYf", prefix = "Study: "),
  by = c("RF Group" = "RFf")
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage


#+ include = TRUE

#' # Long categorical table
#'
#' - Categorical table in long format
#' - Long indicates that the covariates go down the table

#' ## Ungrouped

out <- pt_cat_long(
  data = data,
  cols = vars(Study = STUDYf, Sex = SEXf, "Race group" = ASIANf, "Child-Pugh" = CPf)
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage

#' ## Grouped (by formulation)

out <- pt_cat_long(
  data = data,
  cols = vars(Study = STUDYf,Sex = SEXf,"Race group" = ASIANf, "Child-Pugh" = CPf),
  span = c(Formulation = "FORMf")
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage

#' ## Summary on bottom and right

out <- pt_cat_long(
  data = data,
  summarize = "both",
  cols = vars(Formulation = FORMf, Sex = SEXf,"Race group" = ASIANf),
  span = vars(Study = STUDYf)
  )

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()


#' \clearpage

#' ## No summary

out <- pt_cat_long(
  data = data,
  summarize = "none",
  cols = vars(Formulation = FORMf, Sex = SEXf,"Race group" = ASIANf),
  span = vars(Study = STUDYf)
  )

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()


#' \clearpage



#' # Wide continuous table
#'
#' - Continuous table in wide format
#' - Wide means that the covariates go across the table
#'

#' ## Ungrouped

out <- pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  units = units
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_wrap()


##' ## Paneled

out <- pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  panel = c(Study = "STUDYf"),
  units = units
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage

#' ## Grouped (by study)

out <- pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  by = c(Study = "STUDYf"),
  units = units
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage

#' ## Paneled and grouped

out <- pt_cont_wide(
  data = data,
  cols = "WT,SCR,AGE,ALB,HT",
  by = c(Study = "STUDYf"),
  panel = c(Formulation = "FORMf"),
  units = units
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage



#' # Long continuous table
#'
#' - Continuous summary table in long format
#' - Long indicates that covariates go down the table
#'

#' ## Ungrouped

out <- pt_cont_long(
  data = data,
  cols = "WT,SCR,AGE",
  units = units
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

#' \clearpage

#' ## Paneled

out <- pt_cont_long(
  data = data,
  cols = "WT,SCR,AGE",
  panel = vars(Study = STUDYf),
  units = units
)

out %>% stable(r_file = "test.R", output_file = "test.tex") %>% st_asis()

