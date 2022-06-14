library(tidyverse)
library(fs)
library(reticulate)
library(lubridate)
library(arrow)

pkgload::load_all()

source("data-raw/utils-selenium.R",
       encoding = "utf-8")

# data-merger -------------------------------------------------------------

interval_merger <- ymd("1970-04-01") %--% today()

# download-data -----------------------------------------------------------

path_merger <- "data-raw/data-merger"
path_merger_raw <- str_c(path_merger, "raw",
                         sep = "/")

if (dir_exists(path_merger_raw)) {
  dir_ls(path_merger_raw) |>
    file_delete()
} else {
  dir_create(path_merger_raw)
}

url_merger <- "https://www.e-stat.go.jp/municipalities/cities/absorption-separation-of-municipalities"

driver <- selenium_driver(path_merger_raw)
driver$get(url_merger)

city_category_list <- driver$find_element_by_xpath('//td[@data-alias="city_category_list"]')
city_category_list$find_element_by_xpath('*/input[@name="city_kd[2]"]')$click()
city_category_list$find_element_by_xpath('*/input[@name="city_kd[3]"]')$click()

driver$find_element_by_xpath('//li[contains(@class,"js-dbview-download-button")]/button')$click()

button <- driver$find_element_by_xpath('//div[contains(@class,"stat-display_selector-modal-ok")]')
action <- ActionChains(driver)
action$move_to_element(button)
action$click(button)
action$perform()

insistent_close_driver(driver, path_merger_raw)

# write-data --------------------------------------------------------------

col_types_merger <- cols(`標準地域コード` = "c",
                         `都道府県` = "c",
                         `政令市･郡･支庁･振興局等` = "c",
                         `政令市･郡･支庁･振興局等（ふりがな）` = "c",
                         `市区町村` = "c",
                         `市区町村（ふりがな）` = "c",
                         `廃置分合等施行年月日` = "D",
                         `改正事由` = "c")

col_names_merger <- c(`標準地域コード` = "city_code",
                      `都道府県` = "pref_name",
                      `政令市･郡･支庁･振興局等` = "subpref_name",
                      `政令市･郡･支庁･振興局等（ふりがな）` = "subpref_name_kana",
                      `市区町村` = "city_name",
                      `市区町村（ふりがな）` = "city_name_kana",
                      `廃置分合等施行年月日` = "date",
                      `改正事由` = "event")

merger <- dir_ls(path_merger_raw) |>
  read_csv(locale = locale(encoding = "shift-jis"),
           col_types = col_types_merger) |>
  rename_with(~ col_names_merger,
              names(col_names_merger)) |>
  relocate(date) |>
  arrange(date) |>
  mutate(across(c(pref_name, subpref_name, subpref_name_kana, city_name, city_name_kana),
                ~ .x |>
                  str_remove_all(r"(\s)") |>
                  stringi::stri_trans_nfkc()),
         event = event |>
           str_split(r"(\n|(?<=編入)し、)")) |>
  unnest(event)  |>

  add_row(date = ymd("2006-03-01"),
          city_code = "19201",
          pref_name = "山梨県",
          city_name = "甲府市",
          city_name_kana = "こうふし",
          event = "上九一色村(19341)大字梯及び古関が甲府市(19201)に編入") |>
  add_row(date = ymd("2006-03-01"),
          city_code = "19341",
          pref_name = "山梨県",
          subpref_name = "西八代郡",
          subpref_name_kana = "にしやつしろぐん",
          city_name = "上九一色村",
          city_name_kana = "かみくいしきむら",
          event = "中道町(19326)と上九一色村(19341)大字梯及び古関が甲府市(19201)に編入") |>

  mutate(event = event |>
           str_remove_all(r"(\s)") |>
           stringi::stri_trans_nfkc(),
         pattern_city = case_when(!is.na(city_name) ~ str_glue(r"((\(({city_name_kana}、)?{city_code}\)))"),
                                  is.na(city_name) ~ str_glue(r"((\(({subpref_name_kana}、)?{city_code}\)))")),
         pattern_city = case_when(city_name == "上九一色村" ~ str_glue("{city_name}{pattern_city}大字梯及び古関|大字精進、本栖及び富士ヶ嶺"),

                                  !is.na(city_name) ~ str_glue("({pref_name})?({replace_na(subpref_name, '')})?{city_name}{pattern_city}?") |>
                                    as.character(),
                                  is.na(city_name) ~ str_glue("({pref_name})?{subpref_name}{pattern_city}?") |>
                                    as.character()))

city_merger <- merger |>
  distinct(across(!event)) |>
  mutate(pattern_city = str_glue("^({pattern_city})$")) |>
  rowid_to_column("city_id")

merger <- merger |>
  group_by(date, event) |>
  summarise(pattern_city = pattern_city |>
              unique() |>
              str_c(collapse = "|"),
            pattern_city = str_glue("({pattern_city})"),
            .groups = "drop")

# pattern_type
zero_or_more <- str_c("{{", str_glue("0,{max(str_length(merger$event))}"), "}}")
pattern_city <- "{pattern_city}"
pattern_cities <- str_glue("({pattern_city}[、と]){zero_or_more}{pattern_city}")
pattern_merger <- list(`編入合併` =  c(pattern_cities, "が", pattern_city, "に編入"),
                       `新設合併` = c(pattern_cities, "が(合併|統合)し、", pattern_city, "を新設"),
                       `政令指定都市施行` = c(pattern_city, "の", pattern_city, "への政令指定都市[施移]行"),
                       `区の新設|郡の新設` = c("", "", pattern_cities, "の新設"),
                       `分割` = c(pattern_city, "を分割し、", pattern_cities, "を新設"),
                       `分離` = c(pattern_cities, "から分離し、", pattern_cities, "を新設"),
                       `名称変更` = c(pattern_city, str_glue("が(.{zero_or_more}に[市町]制施行し、)?"), pattern_city, "に名称変更"),
                       `町制施行` = c(pattern_city, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_city, "に町制施行"),
                       `市制施行` = c(pattern_city, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_city, "に市制施行"),
                       `郡の区域変更` = c(pattern_cities, "が", pattern_cities, "に(郡の)?区域変更"),
                       `郡の廃止` = c(pattern_city, "の廃止", "", ""))

pattern_type <- pattern_merger |>
  map_chr(~ str_glue("^{.x[[1L]]}{.x[[2L]]}{.x[[3L]]}{.x[[4L]]}$"))

pattern_city_from <- pattern_merger |>
  map_chr(~ str_glue("^{.x[[1L]]}(?={.x[[2L]]}{.x[[3L]]}{.x[[4L]]}$)"))

pattern_city_to <- pattern_merger |>
  map_chr(~ str_glue("(?<={.x[[1L]]}{.x[[2L]]}){.x[[3L]]}(?={.x[[4L]]}$)"))

merger <- merger |>
  filter(!str_detect(event, "(特例市に|中核市に)移行$")) |>

  mutate(type = case_when(str_detect(event, str_glue(pattern_type[["編入合併"]])) ~ "編入合併",
                          str_detect(event, str_glue(pattern_type[["新設合併"]])) ~ "新設合併",
                          str_detect(event, str_glue(pattern_type[["政令指定都市施行"]])) ~ "政令指定都市施行",
                          str_detect(event, str_glue(pattern_type[["区の新設|郡の新設"]])) ~ "区の新設|郡の新設",
                          str_detect(event, str_glue(pattern_type[["分割"]])) ~ "分割",
                          str_detect(event, str_glue(pattern_type[["分離"]])) ~ "分離",
                          str_detect(event, str_glue(pattern_type[["名称変更"]])) ~ "名称変更",
                          str_detect(event, str_glue(pattern_type[["町制施行"]])) ~ "町制施行",
                          str_detect(event, str_glue(pattern_type[["市制施行"]])) ~ "市制施行",
                          str_detect(event, str_glue(pattern_type[["郡の区域変更"]])) ~ "郡の区域変更",
                          str_detect(event, str_glue(pattern_type[["郡の廃止"]])) ~ "郡の廃止")) |>
  nest_by(type,
          .key = "merger") |>
  left_join(pattern_city_from |>
              enframe("type", "pattern_city_from"),
            by = "type") |>
  left_join(pattern_city_to |>
              enframe("type", "pattern_city_to"),
            by = "type") |>
  mutate(merger = merger |>
           mutate(pattern_city_from = str_glue(pattern_city_from),
                  pattern_city_to = str_glue(pattern_city_to)) |>
           list(),
         .keep = "unused") |>
  ungroup() |>
  unnest(merger) |>

  relocate(date, type) |>
  arrange(date, type) |>

  mutate(city_from = event |>
           str_extract(pattern_city_from) |>
           str_extract_all(pattern_city),
         city_to = event |>
           str_extract(pattern_city_to) |>
           str_extract_all(pattern_city)) |>
  select(!c(event, pattern_city_from, pattern_city_to, pattern_city)) |>

  unnest(city_from) |>
  unnest(city_to) |>

  left_join(city_merger |>
              select(date, city_id, pattern_city) |>
              group_nest(date,
                         .key = "city_merger"),
            by = "date") |>
  rowwise() |>
  mutate(city_id_from = city_merger |>
           filter(str_detect(city_from, pattern_city)) |>
           pull(city_id) |>
           list(),
         city_id_to = city_merger |>
           filter(str_detect(city_to, pattern_city)) |>
           pull(city_id) |>
           list(),
         .keep = "unused") |>
  ungroup() |>
  unnest(c(city_id_from, city_id_to))

# 2006-03-01: 上九一色村の分割・編入合併では富士河口湖町の比重 (人口) のほうが大きい (https://www.gappei-archive.soumu.go.jp/db/19yama/0260kou/hikaku/hikaku.html)
# 1974-04-01: 小倉区の分割では，1980年時点人口で，小倉北区 (217204人) > 小倉南区 (181740人) => 逆転?
# 1974-04-01: 八幡区の分割では，1980年時点人口で，八幡東区 (107880人) < 八幡西区 (248069人) => そのまま

write_parquet(merger, str_c(path_merger, "merger.parquet", sep = "/"))
write_parquet(city_merger, str_c(path_merger, "city_merger.parquet", sep = "/"))

usethis::use_data(interval_merger,
                  overwrite = TRUE)
