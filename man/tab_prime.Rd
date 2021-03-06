% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-tabular.R
\name{tab_prime}
\alias{tab_prime}
\alias{tab_escape}
\title{Prime a data frame for inclusion into a latex table}
\usage{
tab_prime(data, escape_fun = tab_escape)

tab_escape(string, esc = getOption("pmtables.escape", c("_", "\%")), ...)
}
\arguments{
\item{data}{a data frame}

\item{escape_fun}{a function to sanitize data frame contents}

\item{string}{data to sanitize}

\item{esc}{a character vector of strings to escape}

\item{...}{used only to allow arguments through}
}
\description{
Prime a data frame for inclusion into a latex table
}
