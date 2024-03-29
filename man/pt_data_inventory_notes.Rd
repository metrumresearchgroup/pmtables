% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_inventory_table.R
\name{pt_data_inventory_notes}
\alias{pt_data_inventory_notes}
\title{Return table notes for pt_data_inventory}
\usage{
pt_data_inventory_notes(
  bq = c("BQL", "BLQ"),
  drop_bql = FALSE,
  note_add = NULL
)
}
\arguments{
\item{bq}{Abbreviation for below limit of quantification.}

\item{drop_bql}{If \code{TRUE}, the \code{BQL}/\code{BLQ} summary is omitted.}

\item{note_add}{Additional notes to be include.}
}
\description{
See \code{\link[=pt_data_inventory]{pt_data_inventory()}}. The function generates standard table notes for
the table.
}
