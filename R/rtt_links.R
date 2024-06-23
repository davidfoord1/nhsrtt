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


rtt_links_by_year <- function(url, start_month, end_month) {
  start_yyyy <- substr(start_month, 1, 4)
  stop_yyyy <- substr(end_month, 1, 4)

  start_yy <- as.numeric(substr(start_yyyy, 3, 4)) + 1
  stop_yy <- as.numeric(substr(stop_yyyy, 3, 4)) + 1

  yyyy_list <- seq(start_yyyy, stop_yyyy)
  yy_list <- seq(start_yy, stop_yy)

  paste0(
    url, "rtt-data-", yyyy_list, "-", yy_list
  )
}
