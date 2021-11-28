library(lubridate)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

pkgload::load_all()

# merger ------------------------------------------------------------------

# Last update date: November 27, 2021
date_start <- ymd("1970-04-01")
date_end <- ymd("2021-11-27")

col_types_city <- cols(`標準地域コード` = "c",
                       `都道府県` = "c",
                       `政令市･郡･支庁･振興局等` = "c",
                       `政令市･郡･支庁･振興局等（ふりがな）` = "c",
                       `市区町村` = "c",
                       `市区町村（ふりがな）` = "c",
                       `廃置分合等施行年月日` = col_date("%Y/%m/%d"),
                       `改正事由` = "c")

col_names_city <- c(`標準地域コード` = "city_code",
                    `都道府県` = "prefecture_name",
                    `政令市･郡･支庁･振興局等` = "subprefecture_name",
                    `政令市･郡･支庁･振興局等（ふりがな）` = "subprefecture_name_kana",
                    `市区町村` = "city_name",
                    `市区町村（ふりがな）` = "city_name_kana",
                    `廃置分合等施行年月日` = "date",
                    `改正事由` = "event")

# https://www.e-stat.go.jp/municipalities/cities/absorption-separation-of-municipalities
# Don't forget to check the boxes for '支庁・振興局等' and '郡'.
merger <- read_csv(str_glue("data-raw/merger/merger_{date_start}--{date_end}.csv"),
                   locale = locale(encoding = "shift-jis"),
                   col_types = col_types_city) %>%
  rename_with(~ col_names_city,
              names(col_names_city)) %>%
  relocate(date) %>%
  arrange(date) %>%
  mutate(city_code = city_code %>%
           str_pad(5,
                   pad = "0"),
         across(c(prefecture_name, subprefecture_name, subprefecture_name_kana, city_name, city_name_kana),
                . %>%
                  str_remove_all("\\s") %>%
                  stringi::stri_trans_nfkc()),
         event = event %>%
           str_split("\\n|(?<=編入)し、")) %>%
  unnest(event)  %>%

  add_row(date = ymd("2006-03-01"),
          city_code = "19201",
          prefecture_name = "山梨県",
          city_name = "甲府市",
          city_name_kana = "こうふし",
          event = "上九一色村(19341)大字梯及び古関が甲府市(19201)に編入") %>%
  add_row(date = ymd("2006-03-01"),
          city_code = "19341",
          prefecture_name = "山梨県",
          subprefecture_name = "西八代郡",
          subprefecture_name_kana = "にしやつしろぐん",
          city_name = "上九一色村",
          city_name_kana = "かみくいしきむら",
          event = "中道町(19326)と上九一色村(19341)大字梯及び古関が甲府市(19201)に編入") %>%

  mutate(event = event %>%
           str_remove_all("\\s") %>%
           stringi::stri_trans_nfkc(),
         pattern_city = case_when(!is.na(city_name) ~ str_glue("(\\(({city_name_kana}、)?{city_code}\\))"),
                                  is.na(city_name) ~ str_glue("(\\(({subprefecture_name_kana}、)?{city_code}\\))")),
         pattern_city = case_when(city_name == "上九一色村" ~ str_glue("{city_name}{pattern_city}大字梯及び古関|大字精進、本栖及び富士ヶ嶺"),

                                  !is.na(city_name) ~ str_glue("({prefecture_name})?({replace_na(subprefecture_name, '')})?{city_name}{pattern_city}?") %>%
                                    as.character(),
                                  is.na(city_name) ~ str_glue("({prefecture_name})?{subprefecture_name}{pattern_city}?") %>%
                                    as.character()))

city_merger <- merger %>%
  distinct(across(!event)) %>%
  mutate(pattern_city = str_glue("^({pattern_city})$")) %>%
  group_by(date) %>%
  mutate(city_id = row_number()) %>%
  ungroup() %>%
  relocate(date, city_id)

merger <- merger %>%
  group_by(date, event) %>%
  summarise(pattern_city = pattern_city %>%
              unique() %>%
              str_c(collapse = "|"),
            pattern_city = str_glue("({pattern_city})"),
            .groups = "drop")

# pattern_type
zero_or_more <- str_glue("{{{{0,{max(str_length(merger$event))}}}}}")
pattern_city <- "{pattern_city}"
pattern_cities <- str_glue("({pattern_city}[、と]){zero_or_more}{pattern_city}")
pattern_merger <- list(`編入合併` =  c(pattern_cities, "が", pattern_city, "に編入"),
                       `新設合併` = c(pattern_cities, "が(合併|統合)し、", pattern_city, "を新設"),
                       `政令指定都市施行` = c(pattern_city, "の", pattern_city, "への政令指定都市(施|移)行"),
                       `区の新設|郡の新設` = c("", "", pattern_cities, "の新設"),
                       `分割` = c(pattern_city, "を分割し、", pattern_cities, "を新設"),
                       `分離` = c(pattern_cities, "から分離し、", pattern_cities, "を新設"),
                       `名称変更` = c(pattern_city, str_glue("が(.{zero_or_more}に[市町]制施行し、)?"), pattern_city, "に名称変更"),
                       `町制施行` = c(pattern_city, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_city, "に町制施行"),
                       `市制施行` = c(pattern_city, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_city, "に市制施行"),
                       `郡の区域変更` = c(pattern_cities, "が", pattern_cities, "に(郡の)?区域変更"),
                       `郡の廃止` = c(pattern_city, "の廃止", "", ""))

pattern_type <- pattern_merger %>%
  map_chr(~ str_glue("^{.x[[1]]}{.x[[2]]}{.x[[3]]}{.x[[4]]}$"))

pattern_city_from <- pattern_merger %>%
  map_chr(~ str_glue("^{.x[[1]]}(?={.x[[2]]}{.x[[3]]}{.x[[4]]}$)"))

pattern_city_to <- pattern_merger %>%
  map_chr(~ str_glue("(?<={.x[[1]]}{.x[[2]]}){.x[[3]]}(?={.x[[4]]}$)"))

merger <- merger %>%

  filter(!str_detect(event, "(特例市に|中核市に)移行$")) %>%

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
                          str_detect(event, str_glue(pattern_type[["郡の廃止"]])) ~ "郡の廃止")) %>%
  group_nest(type,
             .key = "merger") %>%
  left_join(pattern_city_from %>%
              enframe("type", "pattern_city_from"),
            by = "type") %>%
  left_join(pattern_city_to %>%
              enframe("type", "pattern_city_to"),
            by = "type") %>%
  rowwise() %>%
  mutate(merger = merger %>%
           mutate(pattern_city_from = str_glue(pattern_city_from),
                  pattern_city_to = str_glue(pattern_city_to)) %>%
           list(),
         .keep = "unused") %>%
  ungroup() %>%
  unnest(merger) %>%

  relocate(date, type) %>%
  arrange(date, type) %>%

  mutate(city_from = event %>%
           str_extract(pattern_city_from) %>%
           str_extract_all(pattern_city),
         city_to = event %>%
           str_extract(pattern_city_to) %>%
           str_extract_all(pattern_city)) %>%
  select(!c(event, pattern_city_from, pattern_city_to, pattern_city)) %>%

  unnest(city_from) %>%
  unnest(city_to)

# 2006-03-01: 上九一色村の分割・編入合併では富士河口湖町の比重 (人口) のほうが大きい (https://www.gappei-archive.soumu.go.jp/db/19yama/0260kou/hikaku/hikaku.html)
# 1974-04-01: 小倉区の分割では，1980年時点人口で，小倉北区 (217204人) > 小倉南区 (181740人)
# 1974-04-01: 八幡区の分割では，1980年時点人口で，八幡東区 (107880人) < 八幡西区 (248069人)

merger <- merger %>%
  left_join(city_merger %>%
              select(date, city_id, pattern_city) %>%
              group_nest(date,
                         .key = "city_merger"),
            by = "date") %>%
  rowwise() %>%
  mutate(city_id_from = city_merger %>%
           filter(str_detect(city_from, pattern_city)) %>%
           pull(city_id) %>%
           list(),
         city_id_to = city_merger %>%
           filter(str_detect(city_to, pattern_city)) %>%
           pull(city_id) %>%
           list(),
         .keep = "unused") %>%
  ungroup() %>%
  unnest(c(city_id_from, city_id_to))

merger
