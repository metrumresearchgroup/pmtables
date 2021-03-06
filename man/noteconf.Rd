% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-notes.R
\name{noteconf}
\alias{noteconf}
\title{Configure table notes}
\usage{
noteconf(
  width = 0.8,
  type = c("tpt", "minipage"),
  hline = c("top", "bottom", "both", "none"),
  table_skip = 0.67,
  note_skip = 0.02,
  hline_pt = 0.4,
  sanitize = TRUE,
  escape = "_"
)
}
\arguments{
\item{width}{the width (as fraction of linewidth) is \code{minipage} notes}

\item{type}{put table notes in either the \code{tablenotes} section of
threeparttable (\code{tpt}) or in a minipage below the table (\code{minipage})}

\item{hline}{where to include horizontal lines}

\item{table_skip}{vertical space (\code{cm}) between the last row of the table
and the hline (if any) at the top of the table notes}

\item{note_skip}{vertical space (\code{cm}) between the top hline of the table
notes and the first note line}

\item{hline_pt}{the point size for hlines}

\item{sanitize}{if \code{TRUE}, notes will be sanitized}

\item{escape}{vector of characters to escape in notes}
}
\description{
Configure table notes
}
