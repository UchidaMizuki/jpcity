source("data-raw/setup.R")
source("data-raw/setup-selenium.R")

# absorption_separation ---------------------------------------------------

areacode_start <- read_rds("data-raw/areacode/areacode_start.rds")
areacode_end <- read_rds("data-raw/areacode/areacode_end.rds")

date_start <- areacode_start$date
date_end <- areacode_end$date

exdir <- "data-raw/absorption_separation"
driver <- new_driver(exdir)
driver$get("https://www.e-stat.go.jp/municipalities/cities/absorption-separation-of-municipalities")

select_date(driver, date_start,
            year_name = "year_from",
            month_name = "month_from",
            day_name = "day_from")
select_date(driver, date_end,
            year_name = "year_to",
            month_name = "month_to",
            day_name = "day_to")
click_city_category(driver)
click_submit_button(driver)

dir_delete(exdir)
click_download_button(driver)

close_driver(driver, exdir)

col_types <- cols(`標準地域コード` = "c",
                  `都道府県` = "c",
                  `政令市･郡･支庁･振興局等` = "c",
                  `政令市･郡･支庁･振興局等（ふりがな）` = "c",
                  `市区町村` = "c",
                  `市区町村（ふりがな）` = "c",
                  `廃置分合等施行年月日` = "D",
                  `改正事由` = "c")

col_names <- c(`標準地域コード` = "city_code",
               `都道府県` = "pref_name",
               `政令市･郡･支庁･振興局等` = "city_desig_name",
               `政令市･郡･支庁･振興局等（ふりがな）` = "city_desig_name_kana",
               `市区町村` = "city_name",
               `市区町村（ふりがな）` = "city_name_kana",
               `廃置分合等施行年月日` = "date",
               `改正事由` = "event")

absorption_separation <- dir_ls(exdir,
                                regexp = "csv$") |>
  read_csv(locale = locale(encoding = "shift-jis"),
           col_types = col_types) |>
  rename_with(\(.) col_names,
              names(col_names)) |>
  relocate(date) |>
  arrange(date) |>
  mutate(across(c(pref_name, city_desig_name, city_desig_name_kana, city_name, city_name_kana),
                \(x) x |>
                  str_remove_all("\\s") |>
                  stringi::stri_trans_nfkc()),
         city_name = city_name |>
           coalesce(city_desig_name),
         city_name_kana = city_name_kana |>
           coalesce(city_desig_name_kana),
         event = event |>
           str_split("\\n|(?<=編入)し、")) |>
  unnest(event)  |>
  mutate(event = event |>
           str_remove_all("\\s") |>
           stringi::stri_trans_nfkc()) |>
  add_row(date = ymd("2006-03-01"),
          city_code = "19201",
          pref_name = "山梨県",
          city_name = "甲府市",
          city_name_kana = "こうふし",
          event = "上九一色村(19341)大字梯及び古関が甲府市(19201)に編入") |>
  add_row(date = ymd("2006-03-01"),
          city_code = "19341",
          pref_name = "山梨県",
          city_desig_name = "西八代郡",
          city_desig_name_kana = "にしやつしろぐん",
          city_name = "上九一色村",
          city_name_kana = "かみくいしきむら",
          event = "中道町(19326)と上九一色村(19341)大字梯及び古関が甲府市(19201)に編入") |>
  vec_unique() |>
  summarise(across(c(city_code, pref_name, city_desig_name, city_desig_name_kana, city_name_kana),
                   \(x) x |>
                     replace_na("") |>
                     str_c(collapse = "|")),
            .by = c(date, city_name, event)) |>
  relocate(city_code) |>
  mutate(pattern_city = str_glue("(\\((({city_name_kana})、)?({city_code})\\))"),
         pattern_city = case_when(city_name == "上九一色村" ~ str_glue("{city_name}{pattern_city}大字梯及び古関|大字精進、本栖及び富士ヶ嶺"),
                                  TRUE ~ str_glue("({pref_name})?({city_desig_name})?{city_name}{pattern_city}?")),
         pattern_city = str_glue("^{pattern_city}$")) |>
  nest(.by = c(date, event),
       .key = "city") |>
  mutate(pattern_city = city |>
           map_chr(\(city) {
             pattern_city <- city |>
               pull(pattern_city) |>
               str_extract("(?<=\\^).+(?=\\$)") |>
               vec_unique() |>
               str_c(collapse = "|")

             str_glue("({pattern_city})")
           }))

zero_or_more <- str_c("{{", str_glue("0,{max(str_length(absorption_separation$event))}"), "}}")
pattern_city <- "{pattern_city}"
pattern_cities <- str_glue("({pattern_city}[、と]){zero_or_more}{pattern_city}")
pattern_absorption_separation <- tribble(
  ~ name, ~ city_from, ~ verb_city_from, ~ city_to, ~ verb_city_to,
  "編入合併", pattern_cities, "が", pattern_city, "に編入",
  "新設合併", pattern_cities, "が(合併|統合)し、", pattern_city, "を新設",
  "政令指定都市施行", pattern_city, "の", pattern_city, "への政令指定都市[施移]行",
  "区の新設|郡の新設", "", "", pattern_cities, "の新設",
  "分割", pattern_city, "を分割し、", pattern_cities, "を新設",
  "分離", pattern_cities, "から分離し、", pattern_cities, "を新設",
  "名称変更", pattern_city, str_glue("が(.{zero_or_more}に[市町]制施行し、)?"), pattern_city, "に名称変更",
  "町制施行", pattern_city, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_city, "に町制施行",
  "市制施行", pattern_city, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_city, "に市制施行",
  "郡の区域変更", pattern_cities, "が", pattern_cities, "に(郡の)?区域変更",
  "郡の廃止", pattern_city, "の廃止", "", ""
)

pattern_type <- pattern_absorption_separation |>
  str_glue_data("^{city_from}{verb_city_from}{city_to}{verb_city_to}$") |>
  set_names(pattern_absorption_separation$name)

pattern_city_from <- pattern_absorption_separation |>
  str_glue_data("^{city_from}(?={verb_city_from}{city_to}{verb_city_to}$)") |>
  set_names(pattern_absorption_separation$name)

pattern_city_to <- pattern_absorption_separation |>
  str_glue_data("(?<={city_from}{verb_city_from}){city_to}(?={verb_city_to}$)") |>
  set_names(pattern_absorption_separation$name)

get_city_from_to <- function(city, city_from_to) {
  if (is.na(city_from_to)) {
    city |>
      select(!pattern_city) |>
      vec_init()
  } else {
    out <- city |>
      filter(str_detect(city_from_to, pattern_city)) |>
      select(!pattern_city)
    stopifnot(vec_size(out) == 1L)
    if (str_detect(out$city_code, "\\|")) {
      city_code <- city_from_to |>
        str_extract("\\d{5}")
      city_name_kana <- city_from_to |>
        str_extract("(?<=\\()\\p{Hiragana}+(?=、\\d{5}\\))")

      out <- out |>
        mutate(across(everything(),
                      \(x) x |>
                        str_split("\\|"))) |>
        unnest(everything()) |>
        filter(city_code == .env$city_code,
               is.na(.env$city_name_kana) | city_name_kana == .env$city_name_kana)
      stopifnot(vec_size(out) == 1L)
    }
    out |>
      mutate(across(everything(),
                    \(x) x |>
                      na_if("")),
             city_name = city_name |>
               na_if(city_desig_name),
             city_name_kana = city_name_kana |>
               na_if(city_desig_name_kana)) |>
      relocate(city_code, pref_name, city_desig_name, city_desig_name_kana, city_name, city_name_kana)
  }
}

absorption_separation <- absorption_separation |>
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
  nest(.by = type,
       .key = "absorption_separation") |>
  left_join(pattern_city_from |>
              enframe("type", "pattern_city_from"),
            by = join_by(type)) |>
  left_join(pattern_city_to |>
              enframe("type", "pattern_city_to"),
            by = join_by(type)) |>
  mutate(absorption_separation = list(absorption_separation, pattern_city_from, pattern_city_to) |>
           pmap(\(absorption_separation, pattern_city_from, pattern_city_to) {
             absorption_separation |>
               mutate(pattern_city_from = str_glue(pattern_city_from),
                      pattern_city_to = str_glue(pattern_city_to))
           }),
         .keep = "unused") |>
  unnest(absorption_separation) |>
  relocate(date, type) |>
  arrange(date, type) |>
  mutate(city_from = event |>
           str_extract(pattern_city_from) |>
           str_extract_all(pattern_city),
         city_to = event |>
           str_extract(pattern_city_to) |>
           str_extract_all(pattern_city),
         .keep = "unused") |>
  unnest(city_from,
         keep_empty = TRUE) |>
  unnest(city_to,
         keep_empty = TRUE) |>
  mutate(from = list(city, city_from) |>
           pmap(get_city_from_to) |>
           list_c(),
         to = list(city, city_to) |>
           pmap(get_city_from_to) |>
           list_c(),
         .keep = "unused")

# okinawa_reversion -------------------------------------------------------

# https://ja.wikipedia.org/wiki/%E6%B2%96%E7%B8%84%E8%BF%94%E9%82%84
date_okinawa_reversion <- ymd("1972-06-07")

exdir <- "data-raw/absorption_separation/okinawa_reversion"
driver <- new_driver(exdir)
driver$get("https://www.e-stat.go.jp/municipalities/cities/areacode")
select_date(driver, date_okinawa_reversion,
            year_name = "date_year",
            month_name = "date_month",
            day_name = "date_day")
click_pref(driver, 47L)
click_city_category(driver)
click_submit_button(driver)
click_download_button(driver)

close_driver(driver, exdir)

absorption_separation_okinawa_reversion <- read_areacode(exdir) |>
  rename(date = date_absorption_separation) |>
  select(!absorption_separation)
absorption_separation_okinawa_reversion <- tibble(date = absorption_separation_okinawa_reversion$date,
                                                  type = "沖縄返還",
                                                  from = vec_init(absorption_separation_okinawa_reversion),
                                                  to = absorption_separation_okinawa_reversion)

# northern_territories ----------------------------------------------------

date_northern_territories <- ymd("1983-04-01")

exdir <- "data-raw/absorption_separation/northern_territories"
driver <- new_driver(exdir)
driver$get("https://www.e-stat.go.jp/municipalities/cities/areacode")
select_date(driver, date_northern_territories,
            year_name = "date_year",
            month_name = "date_month",
            day_name = "date_day")
click_pref(driver, 1L)
click_city_category(driver)
click_submit_button(driver)
click_download_button(driver)

close_driver(driver, exdir)

absorption_separation_northern_territories <- read_areacode(exdir) |>
  filter(as.integer(city_code) %in% 1695:1700) |>
  rename(date = date_absorption_separation) |>
  select(!absorption_separation)
absorption_separation_northern_territories <- tibble(date = absorption_separation_northern_territories$date,
                                                     type = "北方領土",
                                                     from = vec_init(absorption_separation_northern_territories),
                                                     to = absorption_separation_northern_territories)


# write_rds ---------------------------------------------------------------

absorption_separation <- list(interval = date_start %--% date_end,
                              absorption_separation = bind_rows(absorption_separation,
                                                                absorption_separation_okinawa_reversion,
                                                                absorption_separation_northern_territories) |>
                                arrange(date, type))

write_rds(absorption_separation, "data-raw/absorption_separation.rds")
