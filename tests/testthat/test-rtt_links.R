# rtt_links ---------------------------------------------------------------


# rtt_links_by_year -------------------------------------------------------

test_that("rtt_links_by_year returns the correct years", {
  url <- "base/"

  # Months before April starts with previous calendar year
  expect_equal(
    rtt_links_by_year(url, "2023-01", "2024-01"),
    c("base/rtt-data-2022-23", "base/rtt-data-2023-24")
  )

  # April or later starts at same calendar year
  expect_equal(
    rtt_links_by_year(url, "2023-12", "2024-04"),
    c("base/rtt-data-2023-24", "base/rtt-data-2024-25")
  )

  # Same calendar year, different financial year
  expect_equal(
    rtt_links_by_year(url, "2024-02", "2024-04"),
    c("base/rtt-data-2023-24", "base/rtt-data-2024-25")
  )

  # Same calendar year, same financial year
  expect_equal(
    rtt_links_by_year(url, "2023-05", "2023-06"),
    "base/rtt-data-2023-24"
  )

  # Different calendar year, same financial year
  expect_equal(
    rtt_links_by_year(url, "2023-05", "2024-01"),
    "base/rtt-data-2023-24"
  )

  # Several years
  expect_equal(
    rtt_links_by_year(url, "2019-01", "2024-12"),
    c(
      "base/rtt-data-2018-19",
      "base/rtt-data-2019-20",
      "base/rtt-data-2020-21",
      "base/rtt-data-2021-22",
      "base/rtt-data-2022-23",
      "base/rtt-data-2023-24",
      "base/rtt-data-2024-25"
    )
  )
})

# rtt_links_by_month ------------------------------------------------------

test_that("rtt_links_by_month returns the correct month links", {
  # Mock the HTML parsing function
  local_mocked_bindings(
    links_in_page = \(x) x
  )

  links_by_year <- c(
    "not-the-right-file/Oct19",
    "Full-CSV-data-file/Nov19",
    "Full-CSV-data-file/Dec19",
    "Full-CSV-data-file/Jan20",
    "Full-CSV-data-file/Feb20",
    "Full-CSV-data-file/Mar20",
    "Full-CSV-data-file/Apr20",
    "Full-CSV-data-file/May20",
    "Full-CSV-data-file/Jun20",
    "Full-CSV-data-file/Jul20",
    "Full-CSV-data-file/Aug20"
  )

  # Correct file links
  expect_equal(rtt_links_by_month(links_by_year, "2019-10", "2020-08"),
               links_by_year[-1])

  # One month only
  expect_equal(rtt_links_by_month(links_by_year, "2020-08", "2020-08"),
               "Full-CSV-data-file/Aug20")
})
