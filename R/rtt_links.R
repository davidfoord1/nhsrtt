rtt_links <- function(start_month, end_month = Sys.Date()) {
  rtt_stats_page <- paste0("https://www.england.nhs.uk/statistics/",
                      "statistical-work-areas/rtt-waiting-times/")

  rtt_links_by_year(rtt_stats_page, start_month, end_month) |>
    rtt_links_by_month(start_month, end_month)
}

links_in_page <- function(url) {
  url |>
    rvest::read_html() |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
}

#' Get RTT page links for a range of years
#'
#' Generates links by financial year to the RTT statistics webpages. The
#' financial year runs from April to March. So for example from "2019-01-01" to
#' "2024-01-01" should create links for 2018-19 to 2023-24.
#'
#' Appends to the base url in the format rtt-data-yyyy-yy
#'
#' @param url The base RTT stats webpage address.
#' @param start_month The first month in the range of links wanted.
#' @param end_month The last month in the range of links wanted.
#'
#' @return A character vector of webpage
rtt_links_by_year <- function(url, start_month, end_month) {
  # First get the input calendar year and month
  start_yyyy <- substr(start_month, 1, 4)
  stop_yyyy <- substr(end_month, 1, 4)

  start_mm <- as.numeric(substr(start_month, 6, 7))
  stop_mm <- as.numeric(substr(end_month, 6, 7))

  # Adjust the calendar year based on the month. If before April, the financial
  # year would start in the year before the given calendar year. For example:

  # "2019-01-01" would be in year "2018-19", so we would subtract 1 from the
  # given year to get the 2018 base.

  # If before April we subtract 1 to get the financial year's base.
  if (start_mm < 4) start_yyyy <- as.numeric(start_yyyy) - 1
  if (stop_mm < 4)  stop_yyyy  <- as.numeric(stop_yyyy) - 1

  # Then the end of the financial year is always 1 greater than that.
  start_yy <- as.numeric(substr(start_yyyy, 3, 4)) + 1
  stop_yy <- as.numeric(substr(stop_yyyy, 3, 4)) + 1

  # Then we generate the sequence of years.
  yyyy_list <- seq(start_yyyy, stop_yyyy)
  yy_list <- seq(start_yy, stop_yy)

  # To append to the base url.
  paste0(
    url, "rtt-data-", yyyy_list, "-", yy_list
  )
}

#' Get monthly CSV download links
#'
#' Get the links for every CSV file for the date range specified.
#'
#' Unlike [rtt_links_by_year()] we cannot generate exact strings because of
#' variations in the links by file size, and whether the data has been revised
#' and upload date. e.g.:
#'
#' Full-CSV-data-file-May23-ZIP-3627K-revised.zip
#'
#' Full-CSV-data-file-Apr24-ZIP-3855K-11417.zip
#'
#' Instead we approach using regex patterns passed to [grepl()]. First is
#' `"Full-CSV-data-file"` to find only the full CSV ZIP files. A second pattern
#' addresses the date range. The files have dates in format `%b%y` like Apr20. So
#' we generate all months in that format from the start month to the stop month.
#'
#' So with `start_month = "2020-03"` and `end_month = "2020-05"` the pattern would
#' be `"Mar20|Apr20|May20"` and a length 3 character vector would be returned.
#'
#' @param links_by_year Character vector of links to financial year RTT stats
#'   pages.
#'
#' @param start_month The first month in the range of links wanted.
#' @param end_month The last month in the range of links wanted.
#'
#' @return A character vector of links for downloading RTT ZIPs
rtt_links_by_month <- function(links_by_year,
                               start_month,
                               end_month) {

  # Get all the links in the page for each year
  links <- links_by_year |>
    lapply(links_in_page) |>
    unlist() |>
    stats::na.omit() |>
    unique()

  links <- links[grepl("Full-CSV-data-file", links)]

  # The files have dates in format %b%y like Apr20. So we generate all months in
  # that format from the start month to the stop month.
  start_yy <- substr(start_month, 3, 4)
  stop_yy <- substr(end_month, 3, 4)

  start_mm <- as.numeric(substr(start_month, 6, 7))
  stop_mm <- as.numeric(substr(end_month, 6, 7))

  if (start_yy == stop_yy) {
    # If the start and stop year are the same we can just get the months
    # for the month range.
    include_months <- month.abb[start_mm:stop_mm] |>
      paste0(start_yy, collapse = "|")
  } else {
    # Otherwise we have to list months from the the specified month to Dec for
    # the start of the range, and January to the specified month for the end of
    # the range
    start_months <- month.abb[start_mm:12]
    stop_months <- month.abb[1:stop_mm]

    start_year_months <- paste0(start_months, start_yy)
    stop_year_months <- paste0(stop_months, stop_yy)

    # Create every month Jan to Dec for years in the middle
    inbetween_years <- seq(start_yy, stop_yy)
    inbetween_years <- inbetween_years[-1]
    inbetween_years <- inbetween_years[-length(inbetween_years)]

    inbetween_months <-
      unlist(lapply(inbetween_years, \(x) paste0(month.abb, x)))

    include_months <- c(start_year_months,
                        inbetween_months,
                        stop_year_months) |>
      paste(collapse = "|")
  }

  links[grepl(include_months, links)]
}
