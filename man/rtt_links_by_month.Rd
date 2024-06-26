% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtt_links.R
\name{rtt_links_by_month}
\alias{rtt_links_by_month}
\title{Get monthly CSV download links}
\usage{
rtt_links_by_month(links_by_year, start_month, end_month)
}
\arguments{
\item{links_by_year}{Character vector of links to financial year RTT stats
pages.}

\item{start_month}{The first month in the range of links wanted.}

\item{end_month}{The last month in the range of links wanted.}
}
\value{
A character vector of links for downloading RTT ZIPs
}
\description{
Get the links for every CSV file for the date range specified.
}
\details{
Unlike \code{\link[=rtt_links_by_year]{rtt_links_by_year()}} we cannot generate exact strings because of
variations in the links by file size, and whether the data has been revised
and upload date. e.g.:

Full-CSV-data-file-May23-ZIP-3627K-revised.zip

Full-CSV-data-file-Apr24-ZIP-3855K-11417.zip

Instead we approach using regex patterns passed to \code{\link[=grepl]{grepl()}}. First is
\code{"Full-CSV-data-file"} to find only the full CSV ZIP files. A second pattern
addresses the date range. The files have dates in format \verb{\%b\%y} like Apr20. So
we generate all months in that format from the start month to the stop month.

So with \code{start_month = "2020-03"} and \code{end_month = "2020-05"} the pattern would
be \code{"Mar20|Apr20|May20"} and a length 3 character vector would be returned.
}
