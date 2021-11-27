library(lubridate)
library(readr)
library(stringr)
library(dplyr)

pkgload::load_all()

# merger ------------------------------------------------------------------

date_start <- date(int_start(interval_merger))
date_end <- date(int_end(interval_merger))

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
                    `廃置分合等施行年月日` = "date_merger",
                    `改正事由` = "merger")

# https://www.e-stat.go.jp/municipalities/cities/absorption-separation-of-municipalities
# Don't forget to check the boxes for '支庁・振興局等' and '郡'.
merger <- read_csv(str_glue("data-raw/merger/merger_{date_start}--{date_end}.csv"),
                   locale = locale(encoding = "shift-jis"),
                   col_types = col_types_city) %>%
  rename_with(~ col_names_city,
              names(col_names_city)) %>%
  relocate(date_merger) %>%
  arrange(date_merger) %>%
  mutate(city_code = city_code %>%
           str_pad(5,
                   pad = "0"),
         across(c(prefecture_name, subprefecture_name, subprefecture_name_kana, city_name, city_name_kana),
                . %>%
                  str_remove_all("\\s") %>%
                  stringi::stri_trans_nfkc()),
         merger = merger %>%
           str_split("\\n|(?<=編入)し、")) %>%
  unnest(merger)  %>%

  mutate(merger = merger %>%
           str_remove_all("\\s") %>%
           stringi::stri_trans_nfkc(),
         pattern_city = case_when(!is.na(city_name) ~ str_glue("(\\(({city_name_kana}、)?{city_code}\\))"),
                                  is.na(city_name) ~ str_glue("(\\(({subprefecture_name_kana}、)?{city_code}\\))")),
         pattern_city = case_when(city_name == "上九一色村" ~ "上九一色村\\(19341\\)大字梯及び古関|大字精進、本栖及び富士ヶ嶺",
                                  !is.na(city_name) ~ str_glue("({prefecture_name})?({replace_na(subprefecture_name, '')})?{city_name}{pattern_city}?") %>%
                                    as.character(),
                                  is.na(city_name) ~ str_glue("({prefecture_name})?{subprefecture_name}{pattern_city}?") %>%
                                    as.character()))

merger <- merger %>%
  group_by(date_merger, merger) %>%
  summarise(pattern_city = pattern_city %>%
              unique() %>%
              str_c(collapse = "|"),
            pattern_city = str_glue("({pattern_city})"),
            .groups = "drop")

# pattern_mergertype
zero_or_more <- str_glue("{{{{0,{max(str_length(merger$merger))}}}}}")
pattern_cities <- str_glue("({{pattern_city}}[、と]){zero_or_more}{{pattern_city}}")
pattern_merger <- list(`編入合併` =  c(pattern_cities, "が", pattern_cities, "に編入"),
                       `新設合併` = c(pattern_cities, "が(合併|統合)し、", pattern_cities, "を新設"),
                       `政令指定都市施行` = c(pattern_cities, "の", pattern_cities, "への政令指定都市(施|移)行"),
                       `区の新設|郡の新設` = c("", "", pattern_cities, "の新設"),
                       `分割` = c(pattern_cities, "を分割し、", pattern_cities, "を新設"),
                       `分離` = c(pattern_cities, "から分離し、", pattern_cities, "を新設"),
                       `名称変更` = c(pattern_cities, str_glue("が(.{zero_or_more}に[市町]制施行し、)?"), pattern_cities, "に名称変更"),
                       `町制施行` = c(pattern_cities, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_cities, "に町制施行"),
                       `市制施行` = c(pattern_cities, str_glue("が(.{zero_or_more}に名称変更し、)?"), pattern_cities, "に市制施行"),
                       `郡の区域変更` = c(pattern_cities, "が", pattern_cities, "に(郡の)?区域変更"),
                       `郡の廃止` = c(pattern_cities, "の廃止", "", ""))

pattern_mergertype <- pattern_merger %>%
  map_chr(~ str_glue("^{.x[[1]]}{.x[[2]]}{.x[[3]]}{.x[[4]]}$"))

pattern_city_from <- pattern_merger %>%
  map_chr(~ str_glue("^{.x[[1]]}(?={.x[[2]]}{.x[[3]]}{.x[[4]]}$)"))

pattern_city_to <- pattern_merger %>%
  map_chr(~ str_glue("(?<={.x[[1]]}{.x[[2]]}){.x[[3]]}(?={.x[[4]]}$)"))

merger <- merger %>%

  filter(!str_detect(merger, "(特例市に|中核市に)移行$")) %>%

  mutate(mergertype = case_when(str_detect(merger, str_glue(pattern_mergertype[["編入合併"]])) ~ "編入合併",
                                str_detect(merger, str_glue(pattern_mergertype[["新設合併"]])) ~ "新設合併",
                                str_detect(merger, str_glue(pattern_mergertype[["政令指定都市施行"]])) ~ "政令指定都市施行",
                                str_detect(merger, str_glue(pattern_mergertype[["区の新設|郡の新設"]])) ~ "区の新設|郡の新設",
                                str_detect(merger, str_glue(pattern_mergertype[["分割"]])) ~ "分割",
                                str_detect(merger, str_glue(pattern_mergertype[["分離"]])) ~ "分離",
                                str_detect(merger, str_glue(pattern_mergertype[["名称変更"]])) ~ "名称変更",
                                str_detect(merger, str_glue(pattern_mergertype[["町制施行"]])) ~ "町制施行",
                                str_detect(merger, str_glue(pattern_mergertype[["市制施行"]])) ~ "市制施行",
                                str_detect(merger, str_glue(pattern_mergertype[["郡の区域変更"]])) ~ "郡の区域変更",
                                str_detect(merger, str_glue(pattern_mergertype[["郡の廃止"]])) ~ "郡の廃止")) %>%
  left_join(pattern_city_from %>%
              enframe("mergertype", "pattern_city_from"),
            by = "mergertype") %>%
  left_join(pattern_city_to %>%
              enframe("mergertype", "pattern_city_to"),
            by = "mergertype") %>%
  rowwise() %>%
  mutate(pattern_city_from = str_glue(pattern_city_from),
         pattern_city_to = str_glue(pattern_city_to)) %>%
  ungroup() %>%
  mutate(city_from = merger %>%
           str_extract(pattern_city_from) %>%
           str_extract_all(pattern_city),
         city_to = merger %>%
           str_extract(pattern_city_to) %>%
           str_extract_all(pattern_city)) %>%
  select(!c(pattern_city, pattern_city_from, pattern_city_to)) %>%

  unnest(city_from) %>%
  unnest(city_to)

# # 上九一色村(19341)大字梯及び古関が甲府市(19201)に編入し、大字精進、本栖及び富士ヶ嶺が富士河口湖町(19430)に編入
# merger %>%
#   filter(is.na(mergertype),
#          merger == "上九一色村(19341)大字梯及び古関が甲府市(19201)に編入し、大字精進、本栖及び富士ヶ嶺が富士河口湖町(19430)に編入") %>%
#   mutate(mergertype = "編入合併",
#          city_from = "上九一色村(19341)",
#          city_to = list(c("甲府市(19201)", "富士河口湖町(19430)"))) %>%
#   unnest(city_to)
#
# # 中道町(19326)と上九一色村(19341)大字梯及び古関が甲府市(19201)に編入
#
# merger %>%
#   filter(is.na(mergertype)) %>%
#   mutate(mergertype = case_when(merger == "上九一色村(19341)大字梯及び古関が甲府市(19201)に編入し、大字精進、本栖及び富士ヶ嶺が富士河口湖町(19430)に編入" ~ "編入合併",
#                                 merger == "中道町(19326)と上九一色村(19341)大字梯及び古関が甲府市(19201)に編入" ~ "編入合併"),
#          city_from = case_when(merger == "上九一色村(19341)大字梯及び古関が甲府市(19201)に編入し、大字精進、本栖及び富士ヶ嶺が富士河口湖町(19430)に編入" ~ list(""),
#                                merger == "中道町(19326)と上九一色村(19341)大字梯及び古関が甲府市(19201)に編入" ~ "編入合併"))


