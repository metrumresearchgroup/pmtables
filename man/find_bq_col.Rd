% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_inventory_table.R
\name{find_bq_col}
\alias{find_bq_col}
\title{Scan data set columns for BQL / BLQ}
\usage{
find_bq_col(data)
}
\arguments{
\item{data}{a data frame}
}
\description{
Scan data set columns for BQL / BLQ
}
\details{
Will return the first match among \code{BQL} and \code{BLQ}.  If no match is found,
returns \code{NA_character_}.
}
