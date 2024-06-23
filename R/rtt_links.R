rtt_links <- function(start_month, end_month) {
  rtt_stats_page <- paste0("https://www.england.nhs.uk/statistics/",
                      "statistical-work-areas/rtt-waiting-times/")

  rtt_links_by_year(rtt_stats_page, start_month, end_month)
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
