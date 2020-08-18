library(tidyverse)
library(yspec)
spec <- yspec::ys_help$spec()
units <- ys_get_unit(spec)


library(pmtables)
data <- stdata()

dat <- st_new(data)

data %>%
  st_new() %>%
  st_make(.preview = TRUE)

data %>%
  st_new() %>%
  st_rename(`Study / ... Protocol ID` = STUDYf) %>%
  st_rename(Sex  = SEXf) %>%
  st_units(units) %>%
  #st_panel("STUDYf", bold = FALSE, it = TRUE) %>%
  st_clear_reps("STUDYf", .now=TRUE) %>%
  st_bold(cols = "STUDYf") %>%
  st_hline(cols = "STUDYf", pattern = "\\S+") %>%
  st_files(r = "foo.R", output = "foo.tex") %>%
  #st_it("SEXf", pattern = "female") %>%
  st_edit("\\bmale", "dude") %>%
  st_drop(N) %>%
  st_center(.outer = "l") %>%
  st_span("Covariate", WT:ALB) %>%
  st_span("EVERYTHING!", SEXf:ALB, level = 2) %>%
  st_make(.preview = TRUE)

