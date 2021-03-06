% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_inventory_table.R
\name{pt_data_inventory}
\alias{pt_data_inventory}
\title{Create a data inventory table}
\usage{
pt_data_inventory(
  data,
  by = ".total",
  panel = by,
  inner_summary = TRUE,
  drop_miss = FALSE,
  stacked = FALSE,
  table = NULL,
  all_name = "all",
  dv_col = "DV",
  bq_col = find_bq_col(data),
  id_col = "ID",
  ...
)
}
\arguments{
\item{data}{the data frame to summarize; the user should filter or subset
so that \code{data} contains exactly the records to be summarized; pmtables will
not add or remove rows prior to summarizing \code{data}}

\item{by}{the outer grouping variable; may be character or quosure}

\item{panel}{the panel grouping variable; may be character or quosure}

\item{inner_summary}{if \code{TRUE}, then a summary of the inner variable will
be provided}

\item{drop_miss}{if \code{TRUE}, then \code{MISS} will be dropped, but only when all
\code{MISS} values are equal to zero}

\item{stacked}{if \code{TRUE}, then independent summaries are created by \code{outer}
and included in a single table (see examples)}

\item{table}{a named list to use for renaming columns (see details and
examples)}

\item{all_name}{a name to use for the complete data summary}

\item{dv_col}{character name of \code{DV} column}

\item{bq_col}{character name of \code{BQL} column; see \code{\link[=find_bq_col]{find_bq_col()}}}

\item{id_col}{character name of \code{ID} column}

\item{...}{other arguments passed to \code{\link[=data_inventory_chunk]{data_inventory_chunk()}}}
}
\value{
An object with class \code{pmtable}; see \link{class-pmtable}.
}
\description{
This function summarizes your data in a specific way and returns an object
that can be converted into a \code{latex} table.
}
\details{
Output columns include counts for subjects (\code{SUBJ}), observations (\verb{OBS)},
BQL observations, missing observations (\code{MISS}) and percentage of
observations that are BQL. When panels are requested, then the percentages
for \code{OBS} and \code{BQL} are presented for the \code{Overall} data and for the panel
\code{Group}.

The summary function is expecting certain columns to be named in a certain
way. This can be modified to suit your need by passing the following
arguments: \code{dv_col} (for observations), \code{bq_col} (for BQL observations),
and \code{id_col} (for ID). For example, if BQL indicator is in a column called
\code{BELOW} you would pass \code{bq_col = BELOW}.

See the \code{\link[=data_inventory_chunk]{data_inventory_chunk()}} help topic for a description of these
columns.

The notes for this table are generated by \code{\link[=pt_data_inventory_notes]{pt_data_inventory_notes()}}.
}
\examples{

ans <- pt_data_inventory(pmt_pk, by = c("Renal function" = "RFf"))

ans <- pt_data_inventory(
   pmt_pk,
   by = "STUDYf",
   panel = "RFf"
)

ans <- pt_data_inventory(
   pmt_obs,
   by = "STUDYf",
   panel = "SEQf",
   stacked = TRUE
)

\dontrun{
st2report(stable(ans))
}

}
