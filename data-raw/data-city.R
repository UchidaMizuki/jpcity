library(tidyverse)
library(fs)
library(reticulate)
library(lubridate)
library(arrow)

pkgload::load_all()

if (int_end(interval_merger) != today()) {
  source("data-raw/data-merger.R",
         encoding = "utf-8")
}

# data-city ---------------------------------------------------------------

path_city <- "data-raw/data-city"
path_city_raw <- str_c(path_city, "raw",
                       sep = "/")

if (dir_exists(path_city_raw)) {
  dir_ls(path_city_raw) |>
    file_delete()
} else {
  dir_create(path_city_raw)
}



# TODO

col_types_city <- cols(`標準地域コード` = "c",
                       `都道府県` = "c",
                       `政令市･郡･支庁･振興局等` = "c",
                       `政令市･郡･支庁･振興局等（ふりがな）` = "c",
                       `市区町村` = "c",
                       `市区町村（ふりがな）` = "c",
                       `廃置分合等施行年月日` = col_date("%Y-%m-%d"),
                       `廃置分合等情報有無` = "c")

col_names_city <- c(`標準地域コード` = "city_code",
                    `都道府県` = "prefecture_name",
                    `政令市･郡･支庁･振興局等` = "subprefecture_name",
                    `政令市･郡･支庁･振興局等（ふりがな）` = "subprefecture_name_kana",
                    `市区町村` = "city_name",
                    `市区町村（ふりがな）` = "city_name_kana",
                    `廃置分合等施行年月日` = "date_merger",
                    `廃置分合等情報有無` = "merged")

city_start <- read_csv(str_glue("data-raw/city/city_{date_start}.csv"),
                       locale = locale(encoding = "shift-jis"),
                       col_types = col_types_city)

# URL: https://www.e-stat.go.jp/municipalities/cities/areacode
# Don't forget to check the boxes for '支庁・振興局等' and '郡'.
city_end <- read_csv(str_glue("data-raw/city/city_{date_end}.csv"),
                     locale = locale(encoding = "shift-jis"),
                     col_types = col_types_city)

# usethis::use_data(DATASET, overwrite = TRUE)
city <- bind_rows(city_start %>%
                    add_column(date = date_start),
                  city_end %>%
                    add_column(date = date_end)) %>%
  relocate(date) %>%
  rename_with(~ col_names_city,
              names(col_names_city)) %>%
  replace_na(list(merged = "")) %>%
  mutate(merged = merged == "有")

usethis::use_data(city,
                  internal = TRUE,
                  overwrite = TRUE)
