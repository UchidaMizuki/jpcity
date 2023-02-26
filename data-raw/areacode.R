source("data-raw/setup.R")
source("data-raw/setup-selenium.R")

# areacode ----------------------------------------------------------------

url_areacode <- "https://www.e-stat.go.jp/municipalities/cities/areacode"
get_areacode <- function(date, exdir) {
  driver <- new_driver(exdir)
  driver$get(url_areacode)

  select_date(driver, date,
              year_name = "date_year",
              month_name = "date_month",
              day_name = "date_day")
  click_city_category(driver)
  click_submit_button(driver)

  dir_delete(exdir)
  click_download_button(driver)

  close_driver(driver, exdir)
}

date_start <- ymd("1970-04-01",
                  tz = tz_jst)
exdir_start <- "data-raw/areacode/areacode_start"
get_areacode(date = date_start,
             exdir = exdir_start)
areacode_start <- list(date = date_start,
                       areacode = read_areacode(exdir_start))

date_end <- today(tzone = tz_jst)
exdir_end <- "data-raw/areacode/areacode_end"
get_areacode(date = date_end,
             exdir = exdir_end)
areacode_end <- list(date = date_end,
                     areacode = read_areacode(exdir_end))

dir_create("data-raw/areacode")
write_rds(areacode_start, "data-raw/areacode/areacode_start.rds")
write_rds(areacode_end, "data-raw/areacode/areacode_end.rds")
