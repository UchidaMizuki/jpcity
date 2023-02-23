source("data-raw/setup.R")
source("data-raw/setup-selenium.R")

# areacode ----------------------------------------------------------------

url_areacode <- "https://www.e-stat.go.jp/municipalities/cities/areacode"
get_areacode <- function(date) {
  exdir <- file_temp()

  dir_create(exdir)
  driver <- new_driver(exdir)
  driver$get(url_areacode)

  select_date(driver, date,
              year_name = "date_year",
              month_name = "date_month",
              day_name = "date_day")
  click_city_category(driver)
  click_submit_button(driver)
  click_download_button(driver)

  close_driver(driver, exdir)

  col_types <- cols(`標準地域コード` = "c",
                    `都道府県` = "c",
                    `政令市･郡･支庁･振興局等` = "c",
                    `政令市･郡･支庁･振興局等（ふりがな）` = "c",
                    `市区町村` = "c",
                    `市区町村（ふりがな）` = "c",
                    `廃置分合等施行年月日` = "D",
                    `廃置分合等情報有無` = "c")

  col_names <- c(`標準地域コード` = "city_code",
                 `都道府県` = "pref_name",
                 `政令市･郡･支庁･振興局等` = "subpref_name",
                 `政令市･郡･支庁･振興局等（ふりがな）` = "subpref_name_kana",
                 `市区町村` = "city_name",
                 `市区町村（ふりがな）` = "city_name_kana",
                 `廃置分合等施行年月日` = "date_merger",
                 `廃置分合等情報有無` = "merged")

  dir_ls(exdir) |>
    read_csv(locale = locale(encoding = "shift-jis"),
             col_types = col_types) |>
    rename_with(\(.) col_names,
                names(col_names)) |>
    mutate(across(c(pref_name, subpref_name, subpref_name_kana, city_name, city_name_kana),
                  \(x) x |>
                    str_remove_all(r"(\s)") |>
                    stringi::stri_trans_nfkc())) |>
    replace_na(list(merged = "")) |>
    mutate(merged = merged == "有")
}

date_start <- ymd("1970-04-01")
areacode_start <- get_areacode(date_start)

date_end <- today()
areacode_end <- get_areacode(date_end)

dir_create("data-raw/areacode")
write_rds(list(date = date_start,
               areacode = areacode_start),
          "data-raw/areacode/areacode_start.rds")
write_rds(list(date = date_end,
               areacode = areacode_end),
          "data-raw/areacode/areacode_end.rds")
